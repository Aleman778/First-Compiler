use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use crate::ast::{Symbol, intern_string};
use crate::ir::*;

struct X86Assembler {
    machine_code: Vec<u8>,
    functions: HashMap<IrIdent, X86Function>,
    jump_targets: HashMap<IrIdent, X86JumpTarget>,
    local_variables: HashMap<IrIdent, (X86Operand, IrType)>,
    allocated_registers: VecDeque<(X86Reg, Option<IrIdent>)>,
    free_registers: VecDeque<X86Reg>,
    curr_stack_offset: isize,
    debug_break_symbol: Symbol,
    assembly: String,
    print_assembly: bool,
    addr_size: isize,
    x64_mode: bool,
}

struct X86Function { // TODO(alexander): do we need to use this really??!??!?
    prologue_byte_position: usize,
    epilogue_byte_offset: usize,
    byte_length: usize,

    enter_label: IrIdent,
    exit_label: IrIdent,
    return_type: IrType,
    func_address: Option<usize>,
    is_foreign: bool,
}

struct X86JumpTarget {
    label: IrIdent,
    jumps: Vec<usize>,
    pos: usize,
}

#[derive(Debug, Clone, Copy)]
enum X86Opcode {
    NOP,
    MOV,
    MOVSX,
    LEA,
    ADD,
    SUB,
    IMUL,
    IDIV,
    AND,
    OR,
    XOR,
    CDQ,
    CMP,
    TEST,
    SETL,
    SETG,
    SETLE,
    SETGE,
    SETE,
    SETNE,
    JL,
    JLE,
    JG,
    JGE,
    JE,
    JNE,
    JMP,
    PUSH,
    POP,
    CALL,
    RET,
    INT3,
}

#[derive(Debug, Clone, Copy)]
enum X86Operand {
    Stack(isize),
    Register(X86Reg),
    Value(X86Value),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum X86Reg {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, Clone, Copy)]
enum X86Value {
    Int8(i8),
    Int32(i32),
    Int64(i64),
}

macro_rules! print_asm {
    ($x86:expr, $str:expr) => {
        if $x86.print_assembly {
            $x86.assembly.push_str($str);
        }
    };

    ($x86:expr, $fmt:expr, $( $arg:expr ),* ) => {
        if $x86.print_assembly {
            $x86.assembly.push_str(&format!($fmt $(, $arg)*));
        }
    }
}

pub fn compile_ir_to_x86_machine_code(
    instructions: Vec<IrInstruction>,
    functions: HashMap<IrIdent, IrBasicBlock>
) -> (Vec<u8>, String) {
    let mut x86 = X86Assembler {
        machine_code: Vec::new(),
        functions: HashMap::new(),
        jump_targets: HashMap::new(),
        local_variables: HashMap::new(),
        allocated_registers: VecDeque::new(),
        free_registers: VecDeque::new(),
        curr_stack_offset: 0,
        debug_break_symbol: intern_string("debug_break"),
        assembly: String::new(),
        print_assembly: true,
        addr_size: std::mem::size_of::<usize>() as isize,
        x64_mode: cfg!(target_arch="x86_64"),
    };

    // Register all functions
    for (ident, bb) in &functions {
        let func = X86Function {
            prologue_byte_position: 0,
            epilogue_byte_offset: 0,
            byte_length: 0,

            enter_label:  bb.enter_label,
            exit_label:   bb.exit_label,
            return_type:  bb.return_type,
            func_address: bb.func_address,
            is_foreign:   bb.is_foreign,
        };
        x86.functions.insert(*ident, func);
    }

    // Compile first main function
    let main_ident = create_ir_ident(intern_string("main"), 0);
    if let Some(bb) = functions.get(&main_ident) {
        let insns = &instructions[bb.prologue_index + 1..=bb.epilogue_index];
        push_function(&mut x86, insns, bb);
    } else {
        panic!("x86: failed to locate `main` function");
    }

    // Compile the rest of the functions
    for (ident, bb) in &functions {
        if *ident != main_ident && !bb.is_foreign {
            let insns = &instructions[bb.prologue_index + 1..bb.epilogue_index];
            push_function(&mut x86, insns, bb);
        }
    }

    // Connect jump distances

    return (x86.machine_code, x86.assembly);
}

fn allocate_register(x86: &mut X86Assembler, ident: Option<IrIdent>) -> X86Reg {
    if x86.free_registers.is_empty() {
        let (reg, opt_ident) = x86.allocated_registers.pop_front().unwrap();
        if let Some(prev_ident) = opt_ident {
            match x86.local_variables.get(&prev_ident) {
                Some(prev_variable) => {
                    // Spill out to stack
                    let (prev_operand, prev_ty) = *prev_variable;
                    x86.curr_stack_offset -= size_of_ir_type(prev_ty, x86.addr_size);
                    let stack_op = X86Operand::Stack(x86.curr_stack_offset);
                    x86.local_variables.insert(prev_ident, (stack_op, prev_ty));
                    push_instruction(x86, X86Opcode::MOV, prev_ty, stack_op, prev_operand);
                }

                None => {}
            }
        }
        x86.allocated_registers.push_back((reg, ident));
        reg
    } else {
        let reg = x86.free_registers.pop_front().unwrap();
        x86.allocated_registers.push_back((reg, ident));
        reg
    }
}

#[inline]
fn free_register(x86: &mut X86Assembler, reg: X86Reg) {
    x86.free_registers.push_front(reg);
    // NOTE(alexander): slow has to iterate through all allocated registers
    // not too big of a concern right now since alloced regsiters is rather small.
    for (i, (areg, _)) in x86.allocated_registers.iter().enumerate() {
        if *areg == reg {
            x86.allocated_registers.remove(i);
            break;
        }
    }
}

fn allocate_specific_register(x86: &mut X86Assembler, reg: X86Reg) -> Option<X86Reg> {
    let mut is_occupied = false;
    let mut prev_ident: Option<IrIdent> = None;
    for (areg, ident) in x86.allocated_registers.iter() {
        if *areg == reg {
            prev_ident = *ident;
            is_occupied = true;
            break;
        }
    }

    if !is_occupied {
        return None;
    }

    let temp_reg = allocate_register(x86, prev_ident);
    if temp_reg != reg {
        let src = X86Operand::Register(reg);
        let dst = X86Operand::Register(temp_reg);
        if let Some(ident) = prev_ident {
            match x86.local_variables.get_mut(&ident) {
                Some(var) => {
                    println!("before: {:#?}", var);
                    var.0 = dst;
                    println!("after: {:#?}", var);
                }
                None => {},
            }
        }
        push_instruction(x86, X86Opcode::MOV, IrType::PtrI32(1), dst, src);
    }
    Some(temp_reg)
}

fn free_specific_register(x86: &mut X86Assembler, reg: X86Reg, prev_reg: Option<X86Reg>) {
    if let Some(src_reg) = prev_reg {
        let src = X86Operand::Register(src_reg);
        let dst = X86Operand::Register(reg);
        push_instruction(x86, X86Opcode::MOV, IrType::PtrI32(1), dst, src);
        free_register(x86, src_reg);
    }
}

#[inline]
fn get_ir_ident(op: IrOperand) -> Option<IrIdent> {
    match op {
        IrOperand::Ident(ident) => Some(ident),
        _ => None
    }
}

#[inline]
fn insert_variable(x86: &mut X86Assembler, dst: IrOperand, src: X86Operand, ty: IrType) {
    match get_ir_ident(dst) {
        Some(ident) => x86.local_variables.insert(ident, (src, ty)),
        None => panic!("x86: expected identifier as first operand"),
    };
}

fn to_x86_operand(x86: &mut X86Assembler, op: IrOperand, insn_ty: IrType) -> X86Operand {
    match op {
        IrOperand::Ident(ident) => match x86.local_variables.get(&ident) {
            Some((operand, _)) => *operand,
            None => {
                let operand = X86Operand::Register(allocate_register(x86, Some(ident)));
                x86.local_variables.insert(ident, (operand, insn_ty));
                operand
            }
        }

        IrOperand::Value(value) => match value {
            IrValue::I32(v)  => X86Operand::Value(X86Value::Int32(v)),
            IrValue::U32(v)  => X86Operand::Value(X86Value::Int32(v as i32)),
            IrValue::U64(v)  => X86Operand::Value(X86Value::Int64(v as i64)),
            IrValue::Bool(v) => X86Operand::Value(X86Value::Int8(v as i8)),
        }

        IrOperand::None => panic!("x86: unexpected empty operand"),
    }
}

fn to_ref_type(ty: IrType) -> IrType {
    match ty {
        IrType::I8 => IrType::PtrI8(1),
        IrType::I32 => IrType::PtrI32(1),
        IrType::PtrI8(i) => IrType::PtrI8(i + 1),
        IrType::PtrI32(i) => IrType::PtrI32(i + 1),
        _ => panic!("unexpected type"),
    }
}

fn move_operand_to_register(
    x86: &mut X86Assembler,
    op: X86Operand,
    ident: Option<IrIdent>,
    ty: IrType
) -> (X86Reg, X86Operand) {

    if let X86Operand::Register(reg) = op {
        (reg, op)
    } else {
        let reg = allocate_register(x86, Some(ident.unwrap()));
        let new_operand = X86Operand::Register(reg);
        push_instruction(x86, X86Opcode::MOV, ty, new_operand, op);
        (reg, new_operand)
    }
}

fn move_operand_to_stack(x86: &mut X86Assembler, op: X86Operand, ty: IrType) -> (isize, X86Operand) {
    if let X86Operand::Stack(disp) = op {
        (disp, op)
    } else {
        x86.curr_stack_offset -= size_of_ir_type(ty, x86.addr_size);
        let stack_op = X86Operand::Stack(x86.curr_stack_offset);
        push_instruction(x86, X86Opcode::MOV, ty, stack_op, op);
        (x86.curr_stack_offset, stack_op)
    }
}

#[inline]
fn get_ir_ident(op: IrOperand) -> IrLabel {
    if let IrOperand::Ident(ident) = insn.op3 {
        ident
    } else {
        panic!("x86: expected an identifier as operand")
    }
}

fn push_function(x86: &mut X86Assembler, insns: &[IrInstruction], bb: &IrBasicBlock) {
    // Setup the x86 state
    x86.curr_stack_offset = 0;
    x86.local_variables.clear();
    x86.free_registers.clear();
    x86.free_registers.push_back(X86Reg::RAX);
    x86.free_registers.push_back(X86Reg::RBX);
    x86.free_registers.push_back(X86Reg::RCX);
    x86.free_registers.push_back(X86Reg::RDX);
    if x86.x64_mode {
        x86.free_registers.push_back(X86Reg::R10);
        x86.free_registers.push_back(X86Reg::R11);
        x86.free_registers.push_back(X86Reg::R12);
        x86.free_registers.push_back(X86Reg::R13);
        x86.free_registers.push_back(X86Reg::R14);
        x86.free_registers.push_back(X86Reg::R15);
        x86.free_registers.push_back(X86Reg::R8); // NOTE(alexander): this regsiters are usually needed for
        x86.free_registers.push_back(X86Reg::R9); // calling functions to use those if really needed
    }
    x86.free_registers.push_back(X86Reg::RSI);
    x86.free_registers.push_back(X86Reg::RDI);

    // Setup enter jump target
    let base_pos = x86.machine_code.len();
    let jt = get_mut_x86_jump_target(x86, bb.enter_label);
    jt.pos = base_pos;

    // x86.machine_code.push(0xcc); // FIXME: debugging remove this

    // Prologue
    print_asm!(x86, "{}:\n", bb.enter_label);

    // push rbp
    x86.machine_code.push(0xff);
    x86.machine_code.push(modrm(6, reg_id(X86Reg::RBP)));
    print_asm!(x86, "    push  rbp\n");

    // mov rbp rsp
    push_instruction(x86,
                     X86Opcode::MOV,
                     IrType::I64,
                     X86Operand::Register(X86Reg::RBP),
                     X86Operand::Register(X86Reg::RSP));

    // sub rsp x (gets filled in later, if needed)
    let sub_byte_pos = x86.machine_code.len();

    // Function body
    let mut require_stack_frame = false;

    let num_insns = insns.len();

    for (i, insn) in insns.iter().enumerate() {
        let insn_index = bb.prologue_index + i + 1;
        
        match insn.opcode {
            IrOpcode::Nop => {
                x86.machine_code.push(0x90);
                print_asm!(x86, "    nop\n");
            }

            IrOpcode::Alloca => {
                x86.curr_stack_offset -= size_of_ir_type(insn.ty, x86.addr_size);
                let dst = X86Operand::Stack(x86.curr_stack_offset);
                let src = to_x86_operand(x86, insn.op2, insn.ty);
                push_instruction(x86, X86Opcode::MOV, insn.ty, dst, src);
                insert_variable(x86, insn.op1, dst, insn.ty);
            }

            IrOpcode::Copy => {
                let dst = to_x86_operand(x86, insn.op1, insn.ty);
                let src = to_x86_operand(x86, insn.op2, insn.ty);
                push_instruction(x86, X86Opcode::MOV, insn.ty, dst, src);
                insert_variable(x86, insn.op1, dst, insn.ty);
            }

            IrOpcode::CopyFromDeref => {
                let ref_ty = to_ref_type(insn.ty);
                let dst = to_x86_operand(x86, insn.op1, insn.ty);
                let src = to_x86_operand(x86, insn.op2, ref_ty);

                // Make sure that we access the data from register rather than stack.
                let ident = get_ir_ident(insn.op2);
                let (src_reg, src) = move_operand_to_register(x86, src, ident, ref_ty);

                // NOTE(alexander): will always be a regsiter
                let dst_reg = if let X86Operand::Register(reg) = dst {
                    reg
                } else {
                    panic!("x86: copy_from_deref expects register as first operand");
                };

                // mov dst, x ptr [src]
                push_rex_prefix(x86, Some(dst_reg), Some(src_reg), insn.ty);
                x86.machine_code.push(0x8b); // RM
                x86.machine_code.push(modrm_disp(reg_id(dst_reg), reg_id(src_reg), 0));
                x86.machine_code.push(0); // NOTE(alexander): use no displacement
                print_instruction(x86, X86Opcode::MOV, insn.ty, dst, src, true);
                insert_variable(x86, insn.op1, dst, insn.ty);
            }

            IrOpcode::CopyFromRef => {
                let ref_ty = to_ref_type(insn.ty);
                let dst = to_x86_operand(x86, insn.op1, ref_ty);
                let src = to_x86_operand(x86, insn.op2, insn.ty);

                // Make sure the src data is stored in memory (only supports stack, no support for heap allocs)
                let (disp, src) = move_operand_to_stack(x86, src, insn.ty);

                // NOTE(alexander): will always be a regsiter
                let reg = if let X86Operand::Register(reg) = dst {
                    reg
                } else {
                    panic!("x86: copy_from_ref expects register as first operand");
                };

                // lea dst, src
                push_rex_prefix(x86, Some(reg), None, insn.ty);
                x86.machine_code.push(0x8d);
                x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(X86Reg::RBP), disp));
                push_displacement(x86, disp);
                print_instruction(x86, X86Opcode::LEA, insn.ty, dst, src, false);

                insert_variable(x86, insn.op1, dst, ref_ty);
            }

            IrOpcode::CopyToDeref => {
                let ref_ty = to_ref_type(insn.ty);
                let dst = to_x86_operand(x86, insn.op1, ref_ty);
                let src = to_x86_operand(x86, insn.op2, insn.ty);
                
                // Make sure that we access the data from register rather than stack.
                let ident = get_ir_ident(insn.op1);
                let (dst_reg, dst) = move_operand_to_register(x86, dst, ident, ref_ty);

                // Has to be register op otherwise use auxiliary register (make sure to free it later)
                match src {
                    X86Operand::Value(val) => {
                        push_rex_prefix(x86, None, Some(dst_reg), insn.ty);
                        x86.machine_code.push(0xc7); // MI
                        x86.machine_code.push(modrm_disp(0, reg_id(dst_reg), 0));
                        x86.machine_code.push(0); // NOTE(alexander): use no displacement
                        push_immediate(x86, val);
                        print_instruction(x86, X86Opcode::MOV, insn.ty, dst, src, false);
                    }

                    X86Operand::Stack(disp) => {
                        // Move first source into auxiliary register
                        let src_reg = allocate_register(x86, None);
                        push_rex_prefix(x86, Some(src_reg), None, insn.ty);
                        x86.machine_code.push(0x8b);
                        x86.machine_code.push(modrm_disp(reg_id(src_reg), reg_id(X86Reg::RBP), disp));
                        push_displacement(x86, disp);
                        print_instruction(x86, X86Opcode::MOV, insn.ty, X86Operand::Register(src_reg), src, false);

                        push_rex_prefix(x86, Some(src_reg), Some(dst_reg), insn.ty);
                        x86.machine_code.push(0x89); // MR
                        x86.machine_code.push(modrm_disp(reg_id(src_reg), reg_id(dst_reg), 0));
                        x86.machine_code.push(0); // NOTE(alexander): use no displacement
                        print_instruction(x86, X86Opcode::MOV, insn.ty, dst, X86Operand::Register(src_reg), true);

                        free_register(x86, src_reg);
                    }

                    X86Operand::Register(src_reg) => {
                        // mov x ptr [dst], src
                        push_rex_prefix(x86, Some(src_reg), Some(dst_reg), insn.ty);
                        x86.machine_code.push(0x89); // MR
                        x86.machine_code.push(modrm_disp(reg_id(src_reg), reg_id(dst_reg), 0));
                        x86.machine_code.push(0); // NOTE(alexander): use no displacement
                        print_instruction(x86, X86Opcode::MOV, insn.ty, dst, src, true);
                    }
                }
            }

            IrOpcode::Clear => {
                let op = to_x86_operand(x86, insn.op1, insn.ty);
                push_instruction(x86, X86Opcode::XOR, insn.ty, op, op);
                insert_variable(x86, insn.op1, op, insn.ty);
            }

            IrOpcode::Add |
            IrOpcode::Sub |
            IrOpcode::And |
            IrOpcode::Or  |
            IrOpcode::Xor => {
                let lhs = to_x86_operand(x86, insn.op2, insn.ty);
                let rhs = to_x86_operand(x86, insn.op3, insn.ty);
                let opcode = match insn.opcode {
                    IrOpcode::Add => X86Opcode::ADD,
                    IrOpcode::Sub => X86Opcode::SUB,
                    IrOpcode::And => X86Opcode::AND,
                    IrOpcode::Or  => X86Opcode::OR,
                    IrOpcode::Xor => X86Opcode::XOR,
                    _ => unreachable!(),
                };
                push_instruction(x86, opcode, insn.ty, lhs, rhs);
                insert_variable(x86, insn.op1, lhs, insn.ty);
            }

            IrOpcode::Mul => {
                let lhs = to_x86_operand(x86, insn.op2, insn.ty);
                let rhs = to_x86_operand(x86, insn.op3, insn.ty);

                // Make sure the left hand side is stored in register
                let ident = get_ir_ident(insn.op2);
                let (reg, lhs) = move_operand_to_register(x86, lhs, ident, insn.ty);

                match rhs {
                    X86Operand::Value(val) => {
                        push_rex_prefix(x86, Some(reg), Some(reg), insn.ty);
                        x86.machine_code.push(0x69); // RMI
                        x86.machine_code.push(modrm(reg_id(reg), reg_id(reg)));
                        push_immediate(x86, val);
                    }

                    X86Operand::Stack(disp) => {
                        push_rex_prefix(x86, Some(reg), None, insn.ty);
                        x86.machine_code.push(0x0f); // RM
                        x86.machine_code.push(0xaf);
                        x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(X86Reg::RBP), disp));
                        push_displacement(x86, disp);
                    }

                    X86Operand::Register(rm) => {
                        push_rex_prefix(x86, Some(reg), Some(rm), insn.ty);
                        x86.machine_code.push(0x0f); // RM
                        x86.machine_code.push(0xaf);
                        x86.machine_code.push(modrm(reg_id(reg), reg_id(rm)));
                    }
                }

                print_instruction(x86, X86Opcode::IMUL, insn.ty, lhs, rhs, false);
                insert_variable(x86, insn.op1, lhs, insn.ty);
            }

            IrOpcode::Div |
            IrOpcode::Mod => {
                let lhs = to_x86_operand(x86, insn.op2, insn.ty);
                let rhs = to_x86_operand(x86, insn.op3, insn.ty);

                // Make sure RDX and RAX is stored somewhere safe!
                let rax_state = allocate_specific_register(x86, X86Reg::RAX);
                let rdx_state = allocate_specific_register(x86, X86Reg::RDX);

                // Make sure the left-hand side is stored in RAX
                let mut lhs_is_rax = false;
                if let X86Operand::Register(reg) = lhs {
                    if let X86Reg::RAX = reg { lhs_is_rax = true; }
                }
                if !lhs_is_rax {
                    let dst = X86Operand::Register(X86Reg::RAX);
                    push_instruction(x86, X86Opcode::MOV, insn.ty, dst, lhs);
                }

                x86.machine_code.push(0x99); // cdq (sign extends EAX to EDX:EAX)
                print_asm!(x86, "    cdq\n");

                match rhs {
                    X86Operand::Value(_) => {
                        // Move first to auxiliary register
                        let reg = allocate_register(x86, None);
                        push_instruction(x86, X86Opcode::MOV, insn.ty, X86Operand::Register(reg), rhs);

                        push_rex_prefix(x86, None, Some(reg), insn.ty);
                        x86.machine_code.push(0xf7); // M
                        x86.machine_code.push(modrm(7, reg_id(reg)));
                        free_register(x86, reg);
                        print_asm!(x86, "    idiv  {}\n", reg);
                    }

                    X86Operand::Stack(disp) => {
                        push_rex_prefix(x86, None, None, insn.ty);
                        x86.machine_code.push(0xf7); // M
                        x86.machine_code.push(modrm_disp(7, reg_id(X86Reg::RBP), disp));
                        push_displacement(x86, disp);
                        print_asm!(x86, "    idiv  dword ptr {}\n", rhs);
                    }

                    X86Operand::Register(reg) => {
                        push_rex_prefix(x86, None, Some(reg), insn.ty);
                        x86.machine_code.push(0xf7); // M
                        x86.machine_code.push(modrm(7, reg_id(reg)));
                        print_asm!(x86, "    idiv  {}\n", reg);
                    }
                }


                // Save the result to the the destination (first operand)
                let dst = to_x86_operand(x86, insn.op1, insn.ty);
                match insn.opcode {
                    IrOpcode::Div => {
                        let mut dst_is_rax = false;
                        if let X86Operand::Register(reg) = lhs {
                            if let X86Reg::RAX = reg { dst_is_rax = true; }
                        }
                        if !dst_is_rax {
                            push_instruction(x86, X86Opcode::MOV, insn.ty, dst, X86Operand::Register(X86Reg::RAX));
                            insert_variable(x86, insn.op1, dst, insn.ty);
                        }
                        
                    }
                    IrOpcode::Mod => {
                        let mut dst_is_rdx = false;
                        if let X86Operand::Register(reg) = lhs {
                            if let X86Reg::RAX = reg { dst_is_rdx = true; }
                        }
                        if !dst_is_rdx {
                            push_instruction(x86, X86Opcode::MOV, insn.ty, dst, X86Operand::Register(X86Reg::RDX));
                            insert_variable(x86, insn.op1, dst, insn.ty);
                        }
                    }
                    _ => unreachable!(),
                }

                // Make sure that we give back the original registers, and free the new ones
                free_specific_register(x86, X86Reg::RAX, rax_state);
                free_specific_register(x86, X86Reg::RDX, rdx_state);
            }

            IrOpcode::Pow => {
                unimplemented!(); // TODO implement this.
            }

            IrOpcode::Lt |
            IrOpcode::Le |
            IrOpcode::Gt |
            IrOpcode::Ge |
            IrOpcode::Eq |
            IrOpcode::Ne => {
                let dst = to_x86_operand(x86, insn.op1, IrType::I8);
                let lhs = to_x86_operand(x86, insn.op2, insn.ty);
                let rhs = to_x86_operand(x86, insn.op3, insn.ty);
                push_instruction(x86, X86Opcode::CMP, insn.ty, lhs, rhs);
                match insn.opcode {
                    IrOpcode::Lt => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9c);
                        print_asm!(x86, "    setl  {}\n", dst);
                    },

                    IrOpcode::Le => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9e);
                        print_asm!(x86, "    setle {}\n", dst);
                    },

                    IrOpcode::Gt => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9f);
                        print_asm!(x86, "    setg  {}\n", dst);
                    },

                    IrOpcode::Ge => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9d);
                        print_asm!(x86, "    setge {}\n", dst);
                    },

                    IrOpcode::Eq => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x94);
                        print_asm!(x86, "    sete  {}\n", dst);
                    },

                    IrOpcode::Ne => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x95);
                        print_asm!(x86, "    setne {}\n", dst);
                    },
                    _ => unreachable!(),
                }

                match dst {
                    X86Operand::Value(_) => panic!("x86: cannot assign to value"),
                    X86Operand::Register(reg) => x86.machine_code.push(modrm(0, reg_id(reg))),
                    X86Operand::Stack(disp) => {
                        x86.machine_code.push(modrm_disp(0, reg_id(X86Reg::RBP), disp));
                        push_displacement(x86, disp);
                    }
                }
                insert_variable(x86, insn.op1, dst, IrType::I8);
            }

            IrOpcode::IfLt |
            IrOpcode::IfLe |
            IrOpcode::IfGt |
            IrOpcode::IfGe |
            IrOpcode::IfEq |
            IrOpcode::IfNe => {
                let lhs = to_x86_operand(x86, insn.op1, insn.ty);
                let rhs = to_x86_operand(x86, insn.op2, insn.ty);
                let label = get_ir_ident(insn.op3);

                push_instruction(x86, X86Opcode::CMP, insn.ty, lhs, rhs);
                match insn.opcode {
                    IrOpcode::IfLt => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8c);
                        print_asm!(x86, "    jl    {}\n", label);
                    },

                    IrOpcode::IfLe => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8e);
                        print_asm!(x86, "    jle   {}\n", label);
                    },

                    IrOpcode::IfGt => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8f);
                        print_asm!(x86, "    jg    {}\n", label);
                    },

                    IrOpcode::IfGe => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8d);
                        print_asm!(x86, "    jge   {}\n", label);
                    },

                    IrOpcode::IfEq => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x84);
                        print_asm!(x86, "    je    {}\n", label);
                    },

                    IrOpcode::IfNe => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x85);
                        print_asm!(x86, "    jne   {}\n", label);
                    },
                    _ => unreachable!(),
                }

                let insn_index = x86.machine_code.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.jumps.push(insn_index);
                x86.machine_code.push(0x0); // NOTE(alexander): jump distance is calculated later.
            }

            IrOpcode::Jump => {
                let label = get_ir_ident(insn.op1);
                let insn_index = x86.machine_code.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.jumps.push(insn_index + 1);

                // jump label
                x86.machine_code.push(0xeb); // D (8-bit relative jump) may need to expand this later 
                x86.machine_code.push(0x0); // NOTE(alexander): jump distance is calculated later.
            }

            IrOpcode::Label => {
                let label = get_ir_ident(insn.op1);
                let insn_index = x86.machine_code.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.pos = insn_index;
                print_asm!(x86, "{}:\n", label);
            }
            
            IrOpcode::Call => {
                if let IrOperand::Ident(ident) = insn.op2 {
                    if ident.symbol == x86.debug_break_symbol {
                        x86.machine_code.push(0xcc);
                    }
                }
            }

            IrOpcode::Return => {
                // Store return value in RAX
                if let IrOperand::None = insn.op1 {
                } else {
                    let src = to_x86_operand(x86, insn.op1, insn.ty);
                    if let X86Operand::Register(X86Reg::RAX) = src {
                    } else {
                        push_instruction(x86, X86Opcode::MOV, insn.ty, X86Operand::Register(X86Reg::RAX), src);
                    }
                }

                // Jump to the end of the function
                if i < num_insns - 1 {
                    let pos = x86.machine_code.len();
                    let jt = get_mut_x86_jump_target(x86, bb.exit_label);
                    jt.jumps.push(pos);
                }
            }

            _ => {},
        }

        // Free registers that are nolonger in use
        for (ident, interval) in &bb.live_intervals {
            if insn_index == interval.end + 1 {
                let opt_reg = match x86.local_variables.get(ident) {
                    Some((operand, _)) => if let X86Operand::Register(reg) = operand {
                        Some(reg)
                    } else {
                        None
                    }
                    
                    None => None,
                };
                if let Some(reg) = opt_reg {
                    let r = *reg;
                    free_register(x86, r);
                    x86.local_variables.remove(ident);
                }
            }
        }
    }
    print_asm!(x86, "{}:\n", bb.exit_label);

    // Setup exit jump target
    let return_pos = x86.machine_code.len();
    let jt = get_mut_x86_jump_target(x86, bb.exit_label);
    jt.pos = return_pos;

    // Epilogue
    // TODO(alexander) add rsp x

    // pop rbp
    x86.machine_code.push(0x8f);
    x86.machine_code.push(modrm(0, reg_id(X86Reg::RBP)));
    print_asm!(x86, "    pop    rbp\n");

    // ret
    x86.machine_code.push(0xc3);
    print_asm!(x86, "    ret\n");
}

fn get_mut_x86_jump_target<'a>(x86: &'a mut X86Assembler, label: IrIdent) -> &'a mut X86JumpTarget {
    match x86.jump_targets.get(&label) {
        Some(_) => {}
        None => {
            let jt = X86JumpTarget {
                label,
                jumps: Vec::new(),
                pos: 0,
            };
            x86.jump_targets.insert(label, jt);
        }
    }

    x86.jump_targets.get_mut(&label).unwrap()
}

/***************************************************************************
 * Machine code builder helpers
 ***************************************************************************/

fn push_instruction(x86: &mut X86Assembler, opcode: X86Opcode, ty: IrType, dst: X86Operand, src: X86Operand) {
    let opcode_offset = match ty {
        IrType::I8 => 1,
        _ => 0,
    };

    match (dst, src) {
        (X86Operand::Stack(disp1), X86Operand::Stack(disp2)) => {
            // Move first source into auxiliary register
            let reg = allocate_register(x86, None);
            push_rex_prefix(x86, None, None, ty);
            x86.machine_code.push(get_rm_opcode(X86Opcode::MOV, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(X86Reg::RBP), disp2));
            push_displacement(x86, disp2);
            print_instruction(x86, X86Opcode::MOV, ty, X86Operand::Register(reg), src, false);

            // Move the auxiliary register to the destination
            push_rex_prefix(x86, None, None, ty);
            x86.machine_code.push(get_mr_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(X86Reg::RBP), disp1));
            push_displacement(x86, disp1);
            print_instruction(x86, opcode, ty, dst, X86Operand::Register(reg), false);

            free_register(x86, reg);
        }

        (X86Operand::Stack(disp), X86Operand::Register(reg)) => {
            push_rex_prefix(x86, Some(reg), None, ty);
            x86.machine_code.push(get_mr_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(X86Reg::RBP), disp));
            push_displacement(x86, disp);
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        (X86Operand::Stack(disp), X86Operand::Value(val)) => {
            let (opcode_byte, opcode_reg) = get_mi_opcode(opcode, opcode_offset);
            push_rex_prefix(x86, None, None, ty);
            x86.machine_code.push(opcode_byte);
            x86.machine_code.push(modrm_disp(opcode_reg, reg_id(X86Reg::RBP), disp));
            push_displacement(x86, disp);
            push_immediate(x86, val);
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        (X86Operand::Register(reg), X86Operand::Stack(disp)) => {
            push_rex_prefix(x86, Some(reg), None, ty);
            x86.machine_code.push(get_rm_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(X86Reg::RBP), disp));
            push_displacement(x86, disp);
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        (X86Operand::Register(reg1), X86Operand::Register(reg2)) => {
            push_rex_prefix(x86, Some(reg1), Some(reg2), ty);
            x86.machine_code.push(get_rm_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm(reg_id(reg1), reg_id(reg2)));
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        (X86Operand::Register(reg), X86Operand::Value(val)) => {
            let (opcode_byte, opcode_reg) = get_mi_opcode(opcode, opcode_offset);
            push_rex_prefix(x86, None, Some(reg), ty);
            x86.machine_code.push(opcode_byte);
            x86.machine_code.push(modrm(opcode_reg, reg_id(reg)));
            push_immediate(x86, val);
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        _ => panic!("x86: cannot store to value operand"),
    }
}

fn get_mr_opcode(opcode: X86Opcode, opcode_offset: u8) -> u8{
    match opcode {
        X86Opcode::MOV  => 0x89 - opcode_offset,
        X86Opcode::ADD  => 0x01 - opcode_offset,
        X86Opcode::SUB  => 0x29 - opcode_offset,
        X86Opcode::AND  => 0x20 - opcode_offset,
        X86Opcode::OR   => 0x08 - opcode_offset,
        X86Opcode::XOR  => 0x31 - opcode_offset,
        X86Opcode::CMP  => 0x39 - opcode_offset,
        X86Opcode::TEST => 0x84 - opcode_offset,
        _ => unimplemented!(),
    }
}

fn get_rm_opcode(opcode: X86Opcode, opcode_offset: u8) -> u8 {
    match opcode {
        X86Opcode::MOV  => 0x8b - opcode_offset,
        X86Opcode::ADD  => 0x03 - opcode_offset,
        X86Opcode::SUB  => 0x2b - opcode_offset,
        X86Opcode::AND  => 0x22 - opcode_offset,
        X86Opcode::OR   => 0x0a - opcode_offset,
        X86Opcode::XOR  => 0x33 - opcode_offset,
        X86Opcode::CMP  => 0x3b - opcode_offset,
        X86Opcode::TEST => 0x84 - opcode_offset,
        _ => unimplemented!(),
    }
}

fn get_mi_opcode(opcode: X86Opcode, opcode_offset: u8) -> (u8, u8) {
    match opcode {
        X86Opcode::MOV  => (0xc7 - opcode_offset, 0),
        X86Opcode::ADD  => (0x81 - opcode_offset, 0),
        X86Opcode::SUB  => (0x81 - opcode_offset, 5),
        X86Opcode::AND  => (0x81 - opcode_offset, 4),
        X86Opcode::OR   => (0x81 - opcode_offset, 1),
        X86Opcode::XOR  => (0x81 - opcode_offset, 6),
        X86Opcode::CMP  => (0x81 - opcode_offset, 7),
        X86Opcode::TEST => (0xf6 - opcode_offset, 0),
        _ => unimplemented!(),
    }
}

fn push_rex_prefix(x86: &mut X86Assembler, reg: Option<X86Reg>, rm: Option<X86Reg>, ty: IrType) {
    if !x86.x64_mode {
        return;
    }

    let mut rex_prefix = match ty {
        IrType::I64       |
        IrType::U64       |
        IrType::PtrI8(_)  |
        IrType::PtrI32(_) => REX_W,
        _ => 0u8,
    };

    if let Some(r) = reg {
        if is_reg_x64_only(r) {
            rex_prefix = rex_prefix | REX_R;
        }
    }

    if let Some(r) = rm {
        if is_reg_x64_only(r) {
            rex_prefix = rex_prefix | REX_B;
        }
    }

    if rex_prefix > 0 {
        x86.machine_code.push(rex_prefix);
    }
}

fn push_displacement(x86: &mut X86Assembler, disp: isize) {
    if disp < -128 && disp > 127 {
        let v = disp as i32;
        x86.machine_code.push((v         & 0xFFi32) as u8);
        x86.machine_code.push(((v >> 8)  & 0xFFi32) as u8);
        x86.machine_code.push(((v >> 16) & 0xFFi32) as u8);
        x86.machine_code.push(((v >> 24) & 0xFFi32) as u8);
    } else {
        let v = disp as i8;
        x86.machine_code.push(v as u8);
    }
}

fn push_immediate(x86: &mut X86Assembler, immediate: X86Value) {
    match immediate {
        X86Value::Int8(v) => {
            x86.machine_code.push(v as u8);
        }
        X86Value::Int32(v) => {
            x86.machine_code.push((v         & 0xFFi32) as u8);
            x86.machine_code.push(((v >> 8)  & 0xFFi32) as u8);
            x86.machine_code.push(((v >> 16) & 0xFFi32) as u8);
            x86.machine_code.push(((v >> 24) & 0xFFi32) as u8);
        }
        X86Value::Int64(v) => {
            x86.machine_code.push((v         & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 8)  & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 16) & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 24) & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 32) & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 40) & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 48) & 0xFFi64) as u8);
            x86.machine_code.push(((v >> 56) & 0xFFi64) as u8);
        }
    }
}

fn reg_id(reg: X86Reg) -> u8 {
    match reg {
        X86Reg::RAX => 0,
        X86Reg::RCX => 1,
        X86Reg::RDX => 2,
        X86Reg::RBX => 3,
        X86Reg::RSP => 4,
        X86Reg::RBP => 5,
        X86Reg::RSI => 6,
        X86Reg::RDI => 7,
        X86Reg::R8  => 0, // NOTE(alexander): set rex_r = 1, only in 64-bit mode
        X86Reg::R9  => 1,
        X86Reg::R10 => 2,
        X86Reg::R11 => 3,
        X86Reg::R12 => 4,
        X86Reg::R13 => 5,
        X86Reg::R14 => 6,
        X86Reg::R15 => 7,
    }
}

fn is_reg_x64_only(reg: X86Reg) -> bool {
    match reg {
        X86Reg::R8  |
        X86Reg::R9  |
        X86Reg::R10 |
        X86Reg::R11 |
        X86Reg::R12 |
        X86Reg::R13 |
        X86Reg::R14 |
        X86Reg::R15 => true,
        _ => false,
    }
}

const REX:   u8 = 0b01000000;
const REX_W: u8 = 0b01001000;
const REX_R: u8 = 0b01000100;
const REX_B: u8 = 0b01000001;

#[inline]
fn modrm(reg: u8, rm: u8) -> u8 {
    return 0b11000000 | (reg << 3) | rm;
}

#[inline]
fn modrm_disp(reg: u8, rm: u8, disp: isize) -> u8 {
    if disp < -128 && disp > 127 {
        return 0b10000000 | (reg << 3) | rm;
    } else {
        return 0b01000000 | (reg << 3) | rm;
    }
}

/***************************************************************************
 * Printing help
 ***************************************************************************/

fn print_instruction(
    x86: &mut X86Assembler,
    opcode: X86Opcode,
    ty: IrType,
    op1: X86Operand,
    op2: X86Operand,
    op2_indirect: bool
) {
    if x86.print_assembly {
        let ptr_str = match ty {
            IrType::I8        => "byte ptr",
            IrType::I32       => "dword ptr",
            IrType::I64       => "qword ptr",
            IrType::U32       => "dword ptr",
            IrType::U64       => "qword ptr",
            IrType::PtrI8(_)  => "byte ptr",
            IrType::PtrI32(_) => "dword ptr",
            IrType::None      => "dword ptr", // NOTE(alexander): default type.
        };

        x86.assembly.push_str(&format!("    {:<6}", format!("{}", opcode)));
        match op1 {
            X86Operand::Stack(_) => x86.assembly.push_str(&format!("{} {}, ", ptr_str, op1)),
            _ => x86.assembly.push_str(&format!("{}, ", op1)),
        }

        match op2 {
            X86Operand::Stack(_) => x86.assembly.push_str(&format!("{} {}\n", ptr_str, op2)),
            _ => if op2_indirect {
                x86.assembly.push_str(&format!("{} [{}]\n", ptr_str, op2));
            } else {
                x86.assembly.push_str(&format!("{}\n", op2));
            }
        }
    }
}

impl fmt::Display for X86Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Opcode::NOP   => write!(f, "nop"),
            X86Opcode::MOV   => write!(f, "mov"),
            X86Opcode::MOVSX => write!(f, "movsx"),
            X86Opcode::LEA   => write!(f, "lea"),
            X86Opcode::ADD   => write!(f, "add"),
            X86Opcode::SUB   => write!(f, "sub"),
            X86Opcode::IMUL  => write!(f, "imul"),
            X86Opcode::IDIV  => write!(f, "idiv"),
            X86Opcode::AND   => write!(f, "and"),
            X86Opcode::OR    => write!(f, "or"),
            X86Opcode::XOR   => write!(f, "xor"),
            X86Opcode::CDQ   => write!(f, "cdq"),
            X86Opcode::CMP   => write!(f, "cmp"),
            X86Opcode::TEST  => write!(f, "test"),
            X86Opcode::SETL  => write!(f, "setl"),
            X86Opcode::SETG  => write!(f, "setg"),
            X86Opcode::SETLE => write!(f, "setle"),
            X86Opcode::SETGE => write!(f, "setge"),
            X86Opcode::SETE  => write!(f, "sete"),
            X86Opcode::SETNE => write!(f, "setne"),
            X86Opcode::JL    => write!(f, "jl"),
            X86Opcode::JLE   => write!(f, "jle"),
            X86Opcode::JG    => write!(f, "jg"),
            X86Opcode::JGE   => write!(f, "jge"),
            X86Opcode::JE    => write!(f, "je"),
            X86Opcode::JNE   => write!(f, "jne"),
            X86Opcode::JMP   => write!(f, "jmp"),
            X86Opcode::PUSH  => write!(f, "push"),
            X86Opcode::POP   => write!(f, "pop"),
            X86Opcode::CALL  => write!(f, "call"),
            X86Opcode::RET   => write!(f, "ret"),
            X86Opcode::INT3  => write!(f, "int3"),
        }
    }
}

impl fmt::Display for X86Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Reg::RAX => write!(f, "rax"),
            X86Reg::RCX => write!(f, "rcx"),
            X86Reg::RDX => write!(f, "rdx"),
            X86Reg::RBX => write!(f, "rbx"),
            X86Reg::RSP => write!(f, "rsp"),
            X86Reg::RBP => write!(f, "rbp"),
            X86Reg::RSI => write!(f, "rsi"),
            X86Reg::RDI => write!(f, "rdi"),
            X86Reg::R8  => write!(f, "r8"),
            X86Reg::R9  => write!(f, "r9"),
            X86Reg::R10 => write!(f, "r10"),
            X86Reg::R11 => write!(f, "r11"),
            X86Reg::R12 => write!(f, "r12"),
            X86Reg::R13 => write!(f, "r13"),
            X86Reg::R14 => write!(f, "r14"),
            X86Reg::R15 => write!(f, "r15"),
        }
    }
}

impl fmt::Display for X86Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Value::Int64(v) => write!(f, "{}", v),
            X86Value::Int32(v) => write!(f, "{}", v),
            X86Value::Int8(v)  => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Operand::Stack(disp) => if *disp > 0 {
                write!(f, "[rbp + {}]", disp)
            } else {
                write!(f, "[rbp - {}]", -disp)
            }
            X86Operand::Register(reg) => write!(f, "{}", reg),
            X86Operand::Value(val) => write!(f, "{}", val),
        }
    }
}
