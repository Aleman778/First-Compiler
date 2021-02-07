use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use crate::ast::{Symbol, intern_string};
use crate::ir::*;

struct X86Assembler {
    machine_code: Vec<u8>,
    label_byte_pos: HashMap<IrIdent, usize>, // position in machine_code to each label
    relative_jumps: Vec<X86RelJump>,
    local_variables: HashMap<IrIdent, (X86Operand, IrType)>,
    allocated_registers: VecDeque<(X86Reg, Option<IrIdent>)>,
    free_registers: VecDeque<X86Reg>,
    argument_stack: VecDeque<(IrOperand, IrType)>, // ordered left-to-right
    curr_stack_offset: isize,
    max_stack_requirement: isize,
    debug_break_symbol: Symbol,
    assembly: String,
    print_assembly: bool,
    addr_size: isize,
    x64_mode: bool,
}

#[derive(Debug, Clone, Copy)]
struct X86RelJump {
    ident: IrIdent,
    pos: usize,
    next_pos: usize,
    target: usize,
    opcode: X86Opcode,
    is_long_jump: bool,
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum X86Operand {
    Stack(X86Reg, isize),
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum X86Value {
    Int8(i8),
    Int32(i32),
    Int64(i64),
}

macro_rules! sprint_asm {
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
        label_byte_pos: HashMap::new(),
        relative_jumps: Vec::new(),
        local_variables: HashMap::new(),
        allocated_registers: VecDeque::new(),
        free_registers: VecDeque::new(),
        argument_stack: VecDeque::new(),
        curr_stack_offset: 0,
        max_stack_requirement: 0,
        debug_break_symbol: intern_string("debug_break"),
        assembly: String::new(),
        print_assembly: true,
        addr_size: std::mem::size_of::<usize>() as isize,
        x64_mode: cfg!(target_arch="x86_64"),
    };

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

    // Before calculating jump distances make sure all jumps have its target set
    for jmp in x86.relative_jumps.iter_mut() {
        jmp.target = *x86.label_byte_pos.get(&jmp.ident).unwrap();
    }

    fn calculate_jump_distance(
        calculated_jumps: &mut Vec<(usize, Vec<u8>, u8)>,
        active_jumps: &mut Vec<X86RelJump>,
        curr_pos: usize,
        x64_mode: bool
    ) -> Vec<bool> {

        let mut bytes_added = 0; // TODO(alexander): this is currently not set!
        let mut keep_active_jumps: Vec<bool> = Vec::new();
        for jmp in active_jumps.iter_mut() {
            let exit_pos = jmp.pos.min(jmp.target);
            if exit_pos == jmp.pos {
                jmp.pos += bytes_added;
            } else {
                jmp.target += bytes_added;
            }

            if curr_pos <= exit_pos {
                let mut jmp_code = Vec::new();
                let dist = jmp.target as isize - jmp.next_pos as isize;
                let pre_allocated_bytes = if jmp.is_long_jump {
                    5
                } else {
                    2
                };
                let is_long_jump = (dist < -128 && dist > 127) || jmp.is_long_jump;

                match (jmp.opcode, is_long_jump) {
                    (X86Opcode::JL, false) => jmp_code.push(0x7c),
                    (X86Opcode::JL, true)  => {
                            jmp_code.push(0x0f);
                            jmp_code.push(0x8c);
                    }

                    (X86Opcode::JLE, false) => jmp_code.push(0x7e),
                    (X86Opcode::JLE, true)  => {
                        jmp_code.push(0x0f);
                        jmp_code.push(0x8e);
                    }

                    (X86Opcode::JG, false) => jmp_code.push(0x7f),
                    (X86Opcode::JG, true)  => {
                        jmp_code.push(0x0f);
                        jmp_code.push(0x8f);
                    }

                    (X86Opcode::JGE, false) => jmp_code.push(0x7d),
                    (X86Opcode::JGE, true)  => {
                        jmp_code.push(0x0f);
                        jmp_code.push(0x8d);
                    }

                    (X86Opcode::JE, false) => jmp_code.push(0x74),
                    (X86Opcode::JE, true)  => {
                        jmp_code.push(0x0f);
                        jmp_code.push(0x84);
                    }

                    (X86Opcode::JNE, false) => jmp_code.push(0x75),
                    (X86Opcode::JNE, true)  => {
                        jmp_code.push(0x0f);
                        jmp_code.push(0x85);
                    }

                    (X86Opcode::JMP, false) => jmp_code.push(0xeb),
                    (X86Opcode::JMP, true)  => jmp_code.push(0xe9),
                    (X86Opcode::CALL, true) => {
                        jmp_code.push(0xe8);
                    }

                    _ => panic!("x86: invalid relative jump instruction"),
                }

                if is_long_jump {
                    if x64_mode {
                        let v = dist as i32;
                        jmp_code.push(( v        & 0xFFi32) as u8);
                        jmp_code.push(((v >> 8)  & 0xFFi32) as u8);
                        jmp_code.push(((v >> 16) & 0xFFi32) as u8);
                        jmp_code.push(((v >> 24) & 0xFFi32) as u8);

                    } else {
                        let v = dist as i16;
                        jmp_code.push(( v        & 0xFFi16) as u8);
                        jmp_code.push(((v >> 8)  & 0xFFi16) as u8);
                    }
                    
                } else {
                    let v = dist as i8;
                    jmp_code.push(v as u8);
                }

                calculated_jumps.push((jmp.pos, jmp_code, pre_allocated_bytes));
                keep_active_jumps.push(false);
            } else {
                keep_active_jumps.push(true);
            }
        }

        return keep_active_jumps;
    }

    // Iterate in reverse order of jumps and jump targets, calculate the
    // relative jump distance, extend 8-bit jump to 32-bits if necessary.
    // Makes sure that overlapping jumps are accounted for when extending bits.
    // Loop invariant: all jump distances calulated so far are correct,
    //                 since adding bytes before the jump wont affect the
    //                 actual jump distance since they are relative.
    x86.relative_jumps.sort_by(|a, b| b.pos.max(b.target).cmp(&a.pos.max(a.target)));
    let mut calculated_jumps: Vec<(usize, Vec<u8>, u8)> = Vec::new();
    let mut active_jumps: Vec<X86RelJump> = Vec::new();
    for next_jmp in &x86.relative_jumps {
        let curr_pos = next_jmp.pos.max(next_jmp.target);
        let keep_active_jumps = calculate_jump_distance(&mut calculated_jumps,
                                                        &mut active_jumps,
                                                        curr_pos,
                                                        x86.x64_mode);

        let mut i = 0;
        active_jumps.retain(|_| (keep_active_jumps[i], i += 1).0);
        active_jumps.push(*next_jmp);
    }

    // Any remaining active jumps needs to be calcuated also
    calculate_jump_distance(&mut calculated_jumps, &mut active_jumps, 0, x86.x64_mode);

    // Now write the calculated jump distances
    for (index, bytes, pre_allocated_bytes) in calculated_jumps {
        if pre_allocated_bytes >= 2 {
            x86.machine_code[index] = bytes[0];
            x86.machine_code[index + 1] = bytes[1];
        }
        if pre_allocated_bytes >= 5 {
            x86.machine_code[index + 2] = bytes[2];
            x86.machine_code[index + 3] = bytes[3];
            x86.machine_code[index + 4] = bytes[4];
        }

        let mut i = pre_allocated_bytes as usize;
        for b in &bytes[pre_allocated_bytes as usize..] {
            x86.machine_code.insert(index + i, *b);
            i += 1;
        }
    }

    (x86.machine_code, x86.assembly)
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

    // Set enter label pos
    let base_pos = x86.machine_code.len();
    x86.label_byte_pos.insert(bb.enter_label, base_pos);

    // x86.machine_code.push(0xcc); // FIXME: debugging remove this

    // Prologue
    sprint_asm!(x86, "{}:\n", bb.enter_label);

    // push rbp
    x86.machine_code.push(0xff);
    x86.machine_code.push(modrm(6, reg_id(X86Reg::RBP)));
    sprint_asm!(x86, "    push  rbp\n");

    // mov rbp rsp
    push_instruction(x86,
                     X86Opcode::MOV,
                     IrType::I64,
                     X86Operand::Register(X86Reg::RBP),
                     X86Operand::Register(X86Reg::RSP));
    
    // sub rsp x (gets filled in later, if needed)
    let sub_rsp_byte_pos = x86.machine_code.len();

    // Function body
    let mut require_stack_frame = false;

    let num_insns = insns.len();

    fn alloca(x86: &mut X86Assembler, ty: IrType, ir_dst: IrOperand, src: X86Operand) {
        x86.curr_stack_offset -= size_of_ir_type(ty, x86.addr_size);
        let dst = X86Operand::Stack(X86Reg::RBP, x86.curr_stack_offset);
        push_instruction(x86, X86Opcode::MOV, ty, dst, src);
        insert_variable(x86, ty, ir_dst, dst);
    }

    for (i, insn) in insns.iter().enumerate() {
        let insn_index = bb.prologue_index + i + 1;

        match insn.opcode {
            IrOpcode::Nop => {
                x86.machine_code.push(0x90);
                sprint_asm!(x86, "    nop\n");
            }

            IrOpcode::Alloca => {
                let op = to_x86_operand(x86, insn.op2, insn.ty);
                alloca(x86, insn.ty, insn.op1, op);
            }

            IrOpcode::AllocParams => {
                // NOTE(alexander): internally we always use windows calling convention.
                let dst_reg: [X86Reg; 4] = [X86Reg::RCX, X86Reg::RDX, X86Reg::R8, X86Reg::R9];
                for i in 0..4 {
                    if let Some((op, ty)) = x86.argument_stack.pop_front() {
                        alloca(x86, ty, op, X86Operand::Register(dst_reg[i]));
                    } else {
                        break;
                    }
                }

                let mut arg_moves: Vec<(IrType, IrOperand, X86Operand)> = Vec::new();
                let mut stack_offset = 0isize;
                for (op, ty) in x86.argument_stack.iter().rev() {
                    arg_moves.push((*ty, *op, X86Operand::Stack(X86Reg::RBP, stack_offset)));
                    stack_offset += size_of_ir_type(*ty, x86.addr_size);
                }

                for (ty, dst, src) in arg_moves {
                    insert_variable(x86, ty, dst, src);
                }

                x86.argument_stack.clear();
            }

            IrOpcode::Copy => {
                let dst = to_x86_operand(x86, insn.op1, insn.ty);
                let src = to_x86_operand(x86, insn.op2, insn.ty);
                push_instruction(x86, X86Opcode::MOV, insn.ty, dst, src);
                insert_variable(x86, insn.ty, insn.op1, dst);
            }

            IrOpcode::CopyFromDeref => {
                let ref_ty = to_ref_type(insn.ty);
                let dst = to_x86_operand(x86, insn.op1, insn.ty);
                let src = to_x86_operand(x86, insn.op2, ref_ty);

                // Make sure that we access the data from register rather than stack.
                let ident = maybe_get_ir_ident(insn.op2);
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
                insert_variable(x86, insn.ty, insn.op1, dst);
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

                insert_variable(x86, ref_ty, insn.op1, dst);
            }

            IrOpcode::CopyToDeref => {
                let ref_ty = to_ref_type(insn.ty);
                let dst = to_x86_operand(x86, insn.op1, ref_ty);
                let src = to_x86_operand(x86, insn.op2, insn.ty);

                // Make sure that we access the data from register rather than stack.
                let ident = maybe_get_ir_ident(insn.op1);
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

                    X86Operand::Stack(sreg, disp) => {
                        // Move first source into auxiliary register
                        let src_reg = allocate_register(x86, None);
                        push_rex_prefix(x86, Some(src_reg), None, insn.ty);
                        x86.machine_code.push(0x8b);
                        x86.machine_code.push(modrm_disp(reg_id(src_reg), reg_id(sreg), disp));
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
                insert_variable(x86, insn.ty, insn.op1, op);
            }

            IrOpcode::Add |
            IrOpcode::Sub |
            IrOpcode::And |
            IrOpcode::Or  |
            IrOpcode::Xor => {
                let dst = to_x86_operand(x86, insn.op1, insn.ty);
                let lhs = to_x86_operand(x86, insn.op2, insn.ty);
                let rhs = to_x86_operand(x86, insn.op3, insn.ty);
                if dst != lhs {
                    push_instruction(x86, X86Opcode::MOV, insn.ty, dst, lhs);
                }
                
                let opcode = match insn.opcode {
                    IrOpcode::Add => X86Opcode::ADD,
                    IrOpcode::Sub => X86Opcode::SUB,
                    IrOpcode::And => X86Opcode::AND,
                    IrOpcode::Or  => X86Opcode::OR,
                    IrOpcode::Xor => X86Opcode::XOR,
                    _ => unreachable!(),
                };
                push_instruction(x86, opcode, insn.ty, dst, rhs);
                insert_variable(x86, insn.ty, insn.op1, dst);
            }

            IrOpcode::Mul => {
                let lhs = to_x86_operand(x86, insn.op2, insn.ty);
                let rhs = to_x86_operand(x86, insn.op3, insn.ty);

                // Make sure the left hand side is stored in register
                let ident = maybe_get_ir_ident(insn.op2);
                let (reg, lhs) = move_operand_to_register(x86, lhs, ident, insn.ty);

                match rhs {
                    X86Operand::Value(val) => {
                        push_rex_prefix(x86, Some(reg), Some(reg), insn.ty);
                        x86.machine_code.push(0x69); // RMI
                        x86.machine_code.push(modrm(reg_id(reg), reg_id(reg)));
                        push_immediate(x86, val);
                    }

                    X86Operand::Stack(sreg, disp) => {
                        push_rex_prefix(x86, Some(reg), None, insn.ty);
                        x86.machine_code.push(0x0f); // RM
                        x86.machine_code.push(0xaf);
                        x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(sreg), disp));
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
                insert_variable(x86, insn.ty, insn.op1, lhs);
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
                sprint_asm!(x86, "    cdq\n");

                match rhs {
                    X86Operand::Value(_) => {
                        // Move first to auxiliary register
                        let reg = allocate_register(x86, None);
                        push_instruction(x86, X86Opcode::MOV, insn.ty, X86Operand::Register(reg), rhs);

                        push_rex_prefix(x86, None, Some(reg), insn.ty);
                        x86.machine_code.push(0xf7); // M
                        x86.machine_code.push(modrm(7, reg_id(reg)));
                        free_register(x86, reg);
                        sprint_asm!(x86, "    idiv  {}\n", reg);
                    }

                    X86Operand::Stack(sreg, disp) => {
                        push_rex_prefix(x86, None, None, insn.ty);
                        x86.machine_code.push(0xf7); // M
                        x86.machine_code.push(modrm_disp(7, reg_id(sreg), disp));
                        push_displacement(x86, disp);
                        sprint_asm!(x86, "    idiv  dword ptr {}\n", rhs);
                    }

                    X86Operand::Register(reg) => {
                        push_rex_prefix(x86, None, Some(reg), insn.ty);
                        x86.machine_code.push(0xf7); // M
                        x86.machine_code.push(modrm(7, reg_id(reg)));
                        sprint_asm!(x86, "    idiv  {}\n", reg);
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
                            insert_variable(x86, insn.ty, insn.op1, dst);
                        }

                    }
                    IrOpcode::Mod => {
                        let mut dst_is_rdx = false;
                        if let X86Operand::Register(reg) = lhs {
                            if let X86Reg::RAX = reg { dst_is_rdx = true; }
                        }
                        if !dst_is_rdx {
                            push_instruction(x86, X86Opcode::MOV, insn.ty, dst, X86Operand::Register(X86Reg::RDX));
                            insert_variable(x86, insn.ty, insn.op1, dst);
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
                        sprint_asm!(x86, "    setl  {}\n", dst);
                    },

                    IrOpcode::Le => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9e);
                        sprint_asm!(x86, "    setle {}\n", dst);
                    },

                    IrOpcode::Gt => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9f);
                        sprint_asm!(x86, "    setg  {}\n", dst);
                    },

                    IrOpcode::Ge => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x9d);
                        sprint_asm!(x86, "    setge {}\n", dst);
                    },

                    IrOpcode::Eq => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x94);
                        sprint_asm!(x86, "    sete  {}\n", dst);
                    },

                    IrOpcode::Ne => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x95);
                        sprint_asm!(x86, "    setne {}\n", dst);
                    },
                    _ => unreachable!(),
                }

                match dst {
                    X86Operand::Value(_) => panic!("x86: cannot assign to value"),
                    X86Operand::Register(reg) => x86.machine_code.push(modrm(0, reg_id(reg))),
                    X86Operand::Stack(sreg, disp) => {
                        x86.machine_code.push(modrm_disp(0, reg_id(sreg), disp));
                        push_displacement(x86, disp);
                    }
                }
                insert_variable(x86, IrType::I8, insn.op1, dst);
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
                let opcode = match insn.opcode {
                    IrOpcode::IfLt => {
                        sprint_asm!(x86, "    jl    {}\n", label);
                        X86Opcode::JL
                    },

                    IrOpcode::IfLe => {
                        sprint_asm!(x86, "    jle   {}\n", label);
                        X86Opcode::JLE
                    },

                    IrOpcode::IfGt => {
                        sprint_asm!(x86, "    jg    {}\n", label);
                        X86Opcode::JG
                    },

                    IrOpcode::IfGe => {
                        sprint_asm!(x86, "    jge   {}\n", label);
                        X86Opcode::JGE
                    },

                    IrOpcode::IfEq => {
                        sprint_asm!(x86, "    je    {}\n", label);
                        X86Opcode::JE
                    },

                    IrOpcode::IfNe => {
                        sprint_asm!(x86, "    jne   {}\n", label);
                        X86Opcode::JNE
                    },
                    _ => unreachable!(),
                };

                push_relative_jump(x86, label, opcode, false);
            }

            IrOpcode::Jump => {
                let label = get_ir_ident(insn.op1);
                push_relative_jump(x86, label, X86Opcode::JMP, false);
                sprint_asm!(x86, "    jmp   {}\n", label);
            }

            IrOpcode::Label => {
                let label = get_ir_ident(insn.op1);
                let insn_pos = x86.machine_code.len();
                x86.label_byte_pos.insert(label, insn_pos);
                sprint_asm!(x86, "{}:\n", label);
            }
            
            IrOpcode::Param => {
                x86.argument_stack.push_back((insn.op1, insn.ty));
            }

            IrOpcode::Call => {
                let return_op = match insn.op2 {
                    IrOperand::Ident(ident) => {
                        if ident.symbol == x86.debug_break_symbol {
                            x86.machine_code.push(0xcc);
                            continue;
                        }
                        
                        // Setup based arguments, windows x64 calling convention
                        let return_op = windows_calling_convention(x86);

                        // Perform the call
                        push_relative_jump(x86, ident, X86Opcode::CALL, true);
                        sprint_asm!(x86, "    call  {}\n", ident);
                        return_op
                    }

                    IrOperand::Value(func_address) => {
                        // x86.machine_code.push(0xcc); // FIXME: REMOVE THIS
                        let reg = allocate_register(x86, None);
                        let dst = X86Operand::Register(reg);
                        let src = to_x86_operand(x86, insn.op2, insn.ty);

                        let return_op = windows_calling_convention(x86);
                        // armv64_calling_convention(x86); // TODO(alexander): implement this
                        // cdecl_calling_convention(x86); // TODO(alexander): implement this

                        if x86.x64_mode {
                            x86.machine_code.push(REX_W); // TODO(alexander): is it possible to use higher registers?
                        }
                        
                        x86.machine_code.push(0xb8 + reg_id(reg));
                        let val = match func_address {
                            IrValue::U32(v) => X86Value::Int32(v as i32),
                            IrValue::U64(v) => X86Value::Int64(v as i64),
                            _ => panic!("x86: unexpected func address value"),
                        };
                        push_immediate(x86, val);
                        sprint_asm!(x86, "    mov   {}, {}\n", dst, val);

                        x86.machine_code.push(0xff);
                        x86.machine_code.push(modrm(2, reg_id(reg)));
                        sprint_asm!(x86, "    call  {}\n", reg);

                        return_op
                    }

                    _ => panic!("x86: expected identifier or value as second operand to Call"),
                };
                
                require_stack_frame = true;
                x86.argument_stack.clear();
                insert_variable(x86, insn.ty, insn.op1, return_op);
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
                    push_relative_jump(x86, bb.exit_label, X86Opcode::JMP, false);
                    sprint_asm!(x86, "    jmp   {}\n", bb.exit_label);
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
    // sprint_asm!(x86, "{}:\n", bb.exit_label); // NOTE(alexander): automatically printed from IR

    // Setup return label byte pos
    let return_pos = x86.machine_code.len();
    x86.label_byte_pos.insert(bb.exit_label, return_pos);

    // Epilogue
    
    // Allocate some stack space for this function if needed
    if require_stack_frame {
        if x86.curr_stack_offset < x86.max_stack_requirement {
            x86.max_stack_requirement = x86.curr_stack_offset;
        }

        // Align 16-bytes
        let stack_misalignment = x86.max_stack_requirement % 16;
        if stack_misalignment < 0 {
            x86.max_stack_requirement -= 16 + stack_misalignment;
        }

        // sub rbp, stackspace
        let v = -x86.max_stack_requirement as i32;
        x86.machine_code.insert(sub_rsp_byte_pos, ((v >> 24) & 0xFFi32) as u8);
        x86.machine_code.insert(sub_rsp_byte_pos, ((v >> 16) & 0xFFi32) as u8);
        x86.machine_code.insert(sub_rsp_byte_pos, ((v >> 8)  & 0xFFi32) as u8);
        x86.machine_code.insert(sub_rsp_byte_pos, ( v        & 0xFFi32) as u8);
        x86.machine_code.insert(sub_rsp_byte_pos, modrm(5, reg_id(X86Reg::RSP)));
        x86.machine_code.insert(sub_rsp_byte_pos, 0x81);
        
        let bytes_added: usize;
        if x86.x64_mode {
            x86.machine_code.insert(sub_rsp_byte_pos, REX_W);
            bytes_added = 7;
        } else {
            bytes_added = 6;
        }
        sprint_asm!(x86, "    sub   rsp, {}\n", -x86.max_stack_requirement); // TODO(alexander): doesn't print to the correct place in the code.

        // 6-bytes have been inserted make sure to update all
        // stored indices pointing after sub_rsp_byte_pos
        for (_, pos) in x86.label_byte_pos.iter_mut() {
            if *pos > sub_rsp_byte_pos {
                *pos += bytes_added;
            }
        }

        for rel_jmp in x86.relative_jumps.iter_mut() {
            if rel_jmp.pos > sub_rsp_byte_pos {
                rel_jmp.pos += bytes_added;
            }
            if rel_jmp.next_pos > sub_rsp_byte_pos {
                rel_jmp.next_pos += bytes_added;
            }
        }

        // sub rbp, stackspace
        if x86.x64_mode {
            x86.machine_code.push(REX_W);
        }
        push_instruction(x86,
                         X86Opcode::ADD,
                         IrType::I32,
                         X86Operand::Register(X86Reg::RSP),
                         X86Operand::Value(X86Value::Int32(v)));
    }

    // pop rbp
    x86.machine_code.push(0x8f);
    x86.machine_code.push(modrm(0, reg_id(X86Reg::RBP)));
    sprint_asm!(x86, "    pop    rbp\n");

    // ret
    x86.machine_code.push(0xc3);
    sprint_asm!(x86, "    ret\n");
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
        (X86Operand::Stack(sreg1, disp1), X86Operand::Stack(sreg2, disp2)) => {
            // Move first source into auxiliary register
            let reg = allocate_register(x86, None);
            push_rex_prefix(x86, None, None, ty);
            x86.machine_code.push(get_rm_opcode(X86Opcode::MOV, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(sreg2), disp2));
            push_displacement(x86, disp2);
            print_instruction(x86, X86Opcode::MOV, ty, X86Operand::Register(reg), src, false);

            // Move the auxiliary register to the destination
            push_rex_prefix(x86, None, None, ty);
            x86.machine_code.push(get_mr_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(sreg1), disp1));
            push_displacement(x86, disp1);
            print_instruction(x86, opcode, ty, dst, X86Operand::Register(reg), false);

            free_register(x86, reg);
        }

        (X86Operand::Stack(sreg, disp), X86Operand::Register(reg)) => {
            push_rex_prefix(x86, Some(reg), None, ty);
            x86.machine_code.push(get_mr_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(sreg), disp));
            push_displacement(x86, disp);
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        (X86Operand::Stack(sreg, disp), X86Operand::Value(val)) => {
            let (opcode_byte, opcode_reg) = get_mi_opcode(opcode, opcode_offset);
            push_rex_prefix(x86, None, None, ty);
            x86.machine_code.push(opcode_byte);
            x86.machine_code.push(modrm_disp(opcode_reg, reg_id(sreg), disp));
            push_displacement(x86, disp);
            push_immediate(x86, val);
            print_instruction(x86, opcode, ty, dst, src, false);
        }

        (X86Operand::Register(reg), X86Operand::Stack(sreg, disp)) => {
            push_rex_prefix(x86, Some(reg), None, ty);
            x86.machine_code.push(get_rm_opcode(opcode, opcode_offset));
            x86.machine_code.push(modrm_disp(reg_id(reg), reg_id(sreg), disp));
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

fn windows_calling_convention(x86: &mut X86Assembler) -> X86Operand {
    let dst_reg: [X86Reg; 4] = [X86Reg::RCX, X86Reg::RDX, X86Reg::R8, X86Reg::R9];
    for i in 0..4 {
        if let Some((op, ty)) = x86.argument_stack.pop_front() {
            let src_op = to_x86_operand(x86, op, ty);
            let dst_op = X86Operand::Register(dst_reg[i]);
            push_instruction(x86, X86Opcode::MOV, ty, dst_op, src_op);
        } else {
            break;
        }
    }

    let prev_stack_offset = x86.curr_stack_offset;
    let mut arg_moves: Vec<(IrType, X86Operand, IrOperand)> = Vec::new();
    for (op, ty) in x86.argument_stack.iter().rev() {
        let dst_op = X86Operand::Stack(X86Reg::RBP, x86.curr_stack_offset);
        arg_moves.push((*ty, dst_op, *op));
        x86.curr_stack_offset += size_of_ir_type(*ty, x86.addr_size);
    }

    for (ty, dst, src) in arg_moves {
        let src_op = to_x86_operand(x86, src, ty);
        push_instruction(x86, X86Opcode::MOV, ty, dst, src_op);
    }
    if x86.curr_stack_offset < x86.max_stack_requirement {
        x86.max_stack_requirement = x86.curr_stack_offset;
    }
    x86.curr_stack_offset = prev_stack_offset;

    return X86Operand::Register(X86Reg::RAX)
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

fn allocate_register(x86: &mut X86Assembler, ident: Option<IrIdent>) -> X86Reg {
    if x86.free_registers.is_empty() {
        let (reg, opt_ident) = x86.allocated_registers.pop_front().unwrap();
        if let Some(prev_ident) = opt_ident {
            match x86.local_variables.get(&prev_ident) {
                Some(prev_variable) => {
                    // Spill out to stack
                    let (prev_operand, prev_ty) = *prev_variable;
                    x86.curr_stack_offset -= size_of_ir_type(prev_ty, x86.addr_size);
                    let stack_op = X86Operand::Stack(X86Reg::RBP, x86.curr_stack_offset);
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
fn insert_variable(x86: &mut X86Assembler, ty: IrType, dst: IrOperand, src: X86Operand) {
    let ident = get_ir_ident(dst);
    x86.local_variables.insert(ident, (src, ty));
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
        let reg = allocate_register(x86, ident);
        let new_operand = X86Operand::Register(reg);
        push_instruction(x86, X86Opcode::MOV, ty, new_operand, op);
        (reg, new_operand)
    }
}

fn move_operand_to_stack(x86: &mut X86Assembler, op: X86Operand, ty: IrType) -> (isize, X86Operand) {
    if let X86Operand::Stack(_, disp) = op {
        (disp, op)
    } else {
        x86.curr_stack_offset -= size_of_ir_type(ty, x86.addr_size);
        let stack_op = X86Operand::Stack(X86Reg::RBP, x86.curr_stack_offset);
        push_instruction(x86, X86Opcode::MOV, ty, stack_op, op);
        (x86.curr_stack_offset, stack_op)
    }
}

#[inline]
fn get_ir_ident(op: IrOperand) -> IrIdent {
    if let IrOperand::Ident(ident) = op {
        ident
    } else {
        panic!("x86: expected an identifier as operand")
    }
}

#[inline]
fn maybe_get_ir_ident(op: IrOperand) -> Option<IrIdent> {
    if let IrOperand::Ident(ident) = op {
        Some(ident)
    } else {
        None
    }
}

fn push_relative_jump(x86: &mut X86Assembler, ident: IrIdent, opcode: X86Opcode, is_long_jump: bool) {
    let pos = x86.machine_code.len();

    // NOTE(alexander): opcode + jump distance is filled after the entire program has compiled.
    x86.machine_code.push(0x90); // reserve one for opcode
    if is_long_jump {
        if x86.x64_mode {
            x86.machine_code.push(0x90);
            x86.machine_code.push(0x90);
            x86.machine_code.push(0x90);
            x86.machine_code.push(0x90);
        } else {
            x86.machine_code.push(0x90);
            x86.machine_code.push(0x90);
        }
    } else {
        x86.machine_code.push(0x90);
    }
    let next_pos = x86.machine_code.len();

    x86.relative_jumps.push(X86RelJump {
        ident,
        pos,
        next_pos,
        target: 0, // calculated afterwards
        opcode,
        is_long_jump
    });

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
            X86Operand::Stack(_, _) => x86.assembly.push_str(&format!("{} {}, ", ptr_str, op1)),
            _ => x86.assembly.push_str(&format!("{}, ", op1)),
        }

        match op2 {
            X86Operand::Stack(_, _) => x86.assembly.push_str(&format!("{} {}\n", ptr_str, op2)),
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
            X86Operand::Stack(sreg, disp) => if *disp > 0 {
                write!(f, "[{} + {}]", sreg, disp)
            } else {
                write!(f, "[{} - {}]", sreg, -disp)
            }
            X86Operand::Register(reg) => write!(f, "{}", reg),
            X86Operand::Value(val) => write!(f, "{}", val),
        }
    }
}
