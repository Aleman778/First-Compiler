use std::collections::HashMap;
// use std::convert::TryInto;
use crate::ast::intern_string;
use crate::ir::*;

struct X86Assembler {
    machine_code: Vec<u8>,
    functions: HashMap<IrIdent, X86Function>,
    jump_targets: HashMap<IrIdent, X86JumpTarget>,
}

struct X86Function {
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
enum X86Operand {
    Stack(isize),
    Register(X86Reg),
    Value(X86Value),
}

#[derive(Debug, Clone, Copy)]
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

pub fn compile_ir_to_x86_machine_code(
    instructions: Vec<IrInstruction>,
    functions: HashMap<IrIdent, IrBasicBlock>
) -> Vec<u8> {
    let mut x86 = X86Assembler {
        machine_code: Vec::new(),
        functions: HashMap::new(),
        jump_targets: HashMap::new(),
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

    return x86.machine_code;
}

fn push_function(x86: &mut X86Assembler, insns: &[IrInstruction], bb: &IrBasicBlock) {

    fn to_x86_operand(variables: &HashMap<IrIdent, X86Operand>, op: IrOperand) -> X86Operand {
        match op {
            IrOperand::Ident(ident) => match variables.get(&ident) {
                Some(operand) => *operand,
                None => {
                    // TODO(alexander): allocate register
                    X86Operand::Register(X86Reg::RAX)
                }
            }
            
            IrOperand::Value(value) => match value {
                IrValue::I32(v)  => X86Operand::Value(X86Value::Int32(v)),
                IrValue::Bool(v) => X86Operand::Value(X86Value::Int8(v as i8)),
            }
            
            IrOperand::None => panic!("x86: unexpected empty operand"),
        }
    }

    fn insert_variable(variables: &HashMap<IrIdent, X86Operand>, dst: IrOperand, src: X86Operand) {
        match dst {
            IrOperand::Ident(ident) => {
                variables.insert(ident, src);
            }
            _ => panic!("x86: expected identifier as first operand"),
        }
    }

    // Setup enter jump target
    let jt = get_mut_x86_jump_target(x86, bb.enter_label);
    jt.pos = x86.machine_code.len();

    // Prologue
    // push rbp
    x86.machine_code.push(0xff);
    x86.machine_code.push(modrm(6, reg_id(X86Reg::RBP)));

    // mov rbp rsp
    push_move(x86, IrType::I64, X86Operand::Register(X86Reg::RBP), X86Operand::Register(X86Reg::RSP));

    // sub rsp x (gets filled in later, if needed)
    let sub_byte_pos = x86.machine_code.len();

    // Function body
    let require_stack_frame = false;
    let curr_stack_offset = 0;
    let variables = HashMap::new();
    let num_insns = insns.len();

    for (i, insn) in insns.iter().enumerate() {
        match insn.opcode {
            IrOpCode::Nop => {
                x86.machine_code.push(0x90);
            }

            IrOpCode::Breakpoint => {
                x86.machine_code.push(0xcc);
            }

            IrOpCode::Alloca => {
                let dst = X86Operand::Stack(curr_stack_offset);
                let src = to_x86_operand(&variables, insn.op2);
                push_move(x86, insn.ty, dst, src);
                insert_variable(&mut variables, insn.op1, src);
            }

            IrOpCode::Copy => {
                let dst = to_x86_operand(&variables, insn.op1);
                let src = to_x86_operand(&variables, insn.op2);
                push_move(x86, insn.ty, dst, src);
                insert_variable(&mut variables, insn.op1, src);
            }

            IrOpCode::Return => {
                // Store return value in RAX
                if let IrOperand::None = insn.op1 {
                } else {
                    let src = to_x86_operand(&variables, insn.op1);
                    if let X86Operand::Register(X86Reg::RAX) = src {
                    } else {
                        push_move(x86, insn.ty, X86Operand::Register(X86Reg::RAX), src);
                    }
                }

                // Jump to the end of the function
                if i < num_insns - 1 {
                    let pos = x86.machine_code.len();
                    let jt = get_mut_x86_jump_target(x86, bb.exit_label);
                    jt.jumps.push(pos);
                }
            }
        }
    }

    // Setup exit jump target
    let jt = get_mut_x86_jump_target(x86, bb.exit_label);
    jt.pos = x86.machine_code.len();

    // Epilogue
    // TODO(alexander) add rsp x

    // pop rbp
    x86.machine_code.push(0x8f);
    x86.machine_code.push(modrm(0, reg_id(X86Reg::RBP)));

    // ret
    x86.machine_code.push(0xc3);
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

fn push_move(x86: &mut X86Assembler, ty: IrType, dst: X86Operand, src: X86Operand) {
    let opcode_offset = match ty {
        IrType::I8 => 1,
        _ => 0,
    };
    match (src, dst) {
        (X86Operand::Stack(disp1), X86Operand::Stack(disp2)) => {
            // TODO(alexander): move first to auxillary register
            unimplemented!();
        }
        
        (X86Operand::Stack(disp), X86Operand::Register(reg)) => {
            x86.machine_code.push(0x89 - opcode_offset); // MR
            x86.machine_code.push(modrm_disp(reg_id(X86Reg::RBP), reg_id(reg), disp));
            push_displacement(disp);
        }

        (X86Operand::Stack(disp), X86Operand::Value(val)) => {
            x86.machine_code.push(0xc7 - opcode_offset); // MI
            x86.machine_code.push(modrm_disp(reg_id(X86Reg::RBP), 0, disp));
            push_displacement(disp);
            push_immeidate(val);
        }

        (X86Operand::Register(reg), X86Operand::Stack(disp)) => {
            x86.machine_code.push(0x8b - opcode_offset); // RM
            x86.machine_code.push(modrm_disp(reg_id(X86Reg::RBP), reg_id(reg), disp));
            push_displacement(disp);
        }

        (X86Operand::Register(reg1), X86Operand::Register(reg2)) => {
            x86.machine_code.push(0x8b - opcode_offset); // RM
            x86.machine_code.push(modrm(reg_id(reg1), reg_id(reg2)));
        }

        (X86Operand::Register(reg), X86Operand::Value(val)) => {
            x86.machine_code.push(0xc7 - opcode_offset); // MI
            x86.machine_code.push(modrm(reg_id(X86Reg::RBP), 0));
            push_immediate(val);
        }

        _ => panic!("x86: cannot move to value operand"),
    }
}

fn push_displacement(x86: &mut X86Assembler, disp: isize) {
    if offset < -128 && offset > 127 {
        let v: i32 = disp.try_into(disp).unwrap();
        x86.machine_code.push((v         & 0xFFi32) as u8);
        x86.machine_code.push(((v >> 8)  & 0xFFi32) as u8);
        x86.machine_code.push(((v >> 16) & 0xFFi32) as u8);
        x86.machine_code.push(((v >> 24) & 0xFFi32) as u8);
    } else {
        let v: i8 = disp.try_into(disp).unwrap();
        x86.machine_code.push(v as u8);
    }
}

fn push_immediate(x86: &mut X86Assembler, immediate: X86Value) {
    match immediate {
        X86Value::Int8(v) => x86.machine_code.push(v as u8),
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

#[inline]
fn modrm(reg: u8, rm: u8) -> u8 {
    return (0b11u8 << 6) | (reg << 3) | rm;
}

#[inline]
fn modrm_disp(reg: u8, rm: u8, disp: isize) -> u8 {
    if offset < -128 && offset > 127 {
        return (0b10u8 << 6) | (reg << 3) | rm;
    } else {
        return (0b01u8 << 6) | (reg << 3) | rm;
    }
}
