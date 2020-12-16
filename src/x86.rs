use std::collections::HashMap;
use crate::ir::*;

pub struct X86Assembler {
    pub machine_code: Vec<u8>,
    pub labels: HashMap<IrLabel, usize>, // stores machine code byte positions
    pub x64_mode: bool, // 64-bit mode changes the instruction encoding
}

// NOTE(alexander): defines parts of an x86 instruction encoding, sib is not used!
struct X86Instruction {
    rex             : bool, // use REX prefix, ignored in 32-bit mode
    rex_w           : bool, // 64-bit mode instructions
    rex_r           : bool, // 64-bit mode mod_reg
    rex_b           : bool, // 64-bit mode mod_rm
    opcode          : X86OpCode,
    modrm_addr_mode : X86AddrMode,
    modrm_reg       : X86Reg,
    modrm_rm        : X86Reg,
    displacement    : X86Value,
    immediate       : X86Value,
    encoding        : X86OpEn,
}

#[derive(Debug, Clone, Copy)]
enum X86OpCode {
    Mov,
    Add,
    Push,
    Pop,
    Ret,
    Int3,
}

#[derive(Debug, Clone, Copy)]
enum X86AddrMode {
    Indirect,       // mod = 00
    IndirectDisp8,  // mod = 01
    IndirectDisp32, // mod = 10
    Direct,         // mod = 11
    None,           // don't use modrm
}

// NOTE(alexander): x86 does not care about signedness, let everything be signed since
// this compiler only has support for signed integers.
#[derive(Debug, Clone, Copy)]
enum X86Value {
    None,
    Int8(i8),
    Int16(i16),
    Int32(i32),
}

// NOTE(alexander): 64-bit register notation used, bitwidth is determined by the prefixes and/or opcode.
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
    R8, // only 64-bit mode registers
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

// NOTE(alexander): see intel's manual on instruction operand encoding for more info.
#[derive(Debug, Clone, Copy)]
enum X86OpEn {
    RM, // modrm_reg, modrm_rm
    MR, // modrm_rm, modrm_reg
    MI, // modrm_rm, immediate
    M,  // modrm_rm,
    O,  // opcode + modrm_reg
    OI, // opcode + modrm_reg, immediate
    I,  // immediate
    ZO, // NO OPERANDS
}

fn create_x86_instruction() -> X86Instruction {
    X86Instruction {
        rex             : false,
        rex_w           : false,
        rex_r           : false,
        rex_b           : false,
        opcode          : X86OpCode::Int3,
        modrm_addr_mode : X86AddrMode::None,
        modrm_reg       : X86Reg::RAX,
        modrm_rm        : X86Reg::RAX,
        displacement    : X86Value::None,
        immediate       : X86Value::None,
        encoding        : X86OpEn::RM,
    }
}

fn register_encoding(reg: X86Reg) -> u8 {
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

fn get_scratch_register(x86: &mut X86Assembler, ir_register: usize) -> X86Reg {
    if x86.x64_mode {
        match ir_register { // TODO(alexander): probably want to prioritize r8, r9, ... higher!
            0  => X86Reg::RAX,
            1  => X86Reg::RCX,
            2  => X86Reg::RDX,
            3  => X86Reg::RBX,
            4  => X86Reg::RSI,
            5  => X86Reg::RDI,
            6  => X86Reg::R8,
            7  => X86Reg::R9,
            8  => X86Reg::R10,
            9  => X86Reg::R11,
            10 => X86Reg::R12,
            11 => X86Reg::R13,
            12 => X86Reg::R14,
            13 => X86Reg::R15,
            _ => panic!("running out of registers!") // FIXME(alexander): spill out to stack
        }
    } else {
        match ir_register {
            0  => X86Reg::RAX,
            1  => X86Reg::RCX,
            2  => X86Reg::RDX,
            3  => X86Reg::RBX,
            4  => X86Reg::RSI,
            5  => X86Reg::RDI,
            _ => panic!("running out of registers!") // FIXME(alexander): spill out to stack
        }
    }
}

fn ir_value_to_x86_value(val: IrValue) -> X86Value {
    match val {
        IrValue::I32(v) => X86Value::Int32(v),
        IrValue::Bool(v) => if v {
            X86Value::Int8(1)
        } else {
            X86Value::Int8(0)
        }
    }
}

pub fn create_x86_assembler() -> X86Assembler {
    X86Assembler {
        machine_code: Vec::new(),
        labels: HashMap::new(),
        x64_mode: cfg!(target_arch="x86_64"),
    }
}

pub fn compile_x86_ir(x86: &mut X86Assembler, instructions: &Vec<IrInstruction>) {
    for insn in instructions {
        match insn.opcode {
          
            _ => compile_x86_ir_instruction(x86, insn),
        }
    }

    // FIXME(alexander): hardcoded ret instruction
}

fn compile_x64_ir_operand(x86: &mut X86Assembler, op: &mut X86Instruction, op1: IrOperand, op2: IrOperand) {
    fn stack_operand(op: &mut X86Instruction, offset: usize) {
        if offset > 127 {
            op.modrm_addr_mode = X86AddrMode::IndirectDisp32;
            op.displacement = X86Value::Int32(-(offset as i32));
        } else if offset > 0 {
            op.modrm_addr_mode = X86AddrMode::IndirectDisp8;
            op.displacement = X86Value::Int8(-(offset as i8));
        } else {
            op.modrm_addr_mode = X86AddrMode::Indirect;
        }
        op.modrm_rm = X86Reg::RBP;
    }

    match op1.kind {
        IrOperandKind::Stack(offset) => stack_operand(op, offset),
        IrOperandKind::Register(register) => {
            op.modrm_addr_mode = X86AddrMode::Direct;
            op.modrm_reg = get_scratch_register(x86, register);
        },

        IrOperandKind::Constant(_) => panic!("cannot encode constant as first operand"),

        _ => return,
    }

    match op2.kind {
        IrOperandKind::Stack(offset) => {
            op.encoding = X86OpEn::RM;
            match op1.kind {
                IrOperandKind::Stack(_) => panic!("cannot encode memory access for both operands!"),
                IrOperandKind::Register(_) => {},
                _ => panic!("unexpected first operand"),
            };

            stack_operand(op, offset);
        }
        IrOperandKind::Register(reg) => {
            match op1.kind {
                IrOperandKind::Stack(_) => {
                    op.modrm_reg = get_scratch_register(x86, reg);
                    op.encoding = X86OpEn::MR;
                }

                IrOperandKind::Register(_) => {
                    op.modrm_rm = get_scratch_register(x86, reg);
                    op.encoding = X86OpEn::RM;
                }

                _ => panic!("unexpected first operand"),
            };
        }

        IrOperandKind::Constant(val) => {
            match op1.kind {
                IrOperandKind::Register(_) => {
                    op.modrm_rm = op.modrm_reg;
                    // TODO(alexander): do we need to reset op.modrm_reg, I would think not.
                }
                _ => {}
            }
            
            op.encoding = X86OpEn::MI;
            op.immediate = ir_value_to_x86_value(val);
        }

        _ => return,
    }
}

/**
 * Compiles an ir instruction into x86 machine code.
 */
pub fn compile_x86_ir_instruction(x86: &mut X86Assembler, ir_insn: &IrInstruction) {
    match ir_insn.opcode {
        IrOpCode::Nop => {
            // NOTE(alexander): is it even worth pushing a NOP?
        },

        IrOpCode::Breakpoint => {
            let mut op = create_x86_instruction();
            op.opcode = X86OpCode::Int3;
            push_x86_instruction(x86, &op);
        },

        IrOpCode::Copy => {
            let mut op = create_x86_instruction();
            op.opcode = X86OpCode::Mov;
            compile_x64_ir_operand(x86, &mut op, ir_insn.op1, ir_insn.op2);
            push_x86_instruction(x86, &op);
        }

        IrOpCode::Add => {
            let mut op = create_x86_instruction();
            op.opcode = X86OpCode::Add;
            compile_x64_ir_operand(x86, &mut op, ir_insn.op1, ir_insn.op2);
            push_x86_instruction(x86, &op);
        }

        IrOpCode::Return => {
            match ir_insn.op1.kind {
                IrOperandKind::None |
                IrOperandKind::Register(0) => {},

                _ => {
                    let mut op = create_x86_instruction();
                    op.opcode = X86OpCode::Mov;
                    
                    let op1 = IrOperand {
                        ty: IrType::None,
                        kind: IrOperandKind::Register(0)
                    };
                    compile_x64_ir_operand(x86, &mut op, op1, ir_insn.op1);
                    push_x86_instruction(x86, &op);
                }
            }

            // TODO(alexander): handle returns that are not at the end of the function.
        }

        IrOpCode::Label => {
            // there is no concept of labels in machine code, just store the byte offset of this label.
            if let IrOperandKind::Label(label) = ir_insn.op1.kind {
                x86.labels.insert(label, x86.machine_code.len());
            }
        }

        IrOpCode::Prologue => {
            // push rbp
            let mut op = create_x86_instruction();
            op.modrm_reg = X86Reg::RBP;
            op.opcode = X86OpCode::Push;
            op.encoding = X86OpEn::O;
            push_x86_instruction(x86, &op);
            
            // mov rbp rsp
            let mut op = create_x86_instruction();
            op.rex = true;
            op.rex_w = true;
            op.opcode = X86OpCode::Mov;
            op.modrm_addr_mode = X86AddrMode::Direct;
            op.modrm_reg = X86Reg::RBP;
            op.modrm_rm  = X86Reg::RSP;
            op.encoding  = X86OpEn::RM;
            push_x86_instruction(x86, &op);
        }
        
        IrOpCode::Epilogue => {
            // pop rbp
            let mut op = create_x86_instruction();
            op.modrm_reg = X86Reg::RBP;
            op.opcode = X86OpCode::Pop;
            op.encoding = X86OpEn::O;
            push_x86_instruction(x86, &op);

            // ret
            let mut op = create_x86_instruction();
            op.opcode = X86OpCode::Ret;
            op.encoding = X86OpEn::ZO;
            push_x86_instruction(x86, &op);
        }

        _ => unimplemented!(),
    }
}

fn push_x86_instruction(x86: &mut X86Assembler, insn: &X86Instruction) {
    if x86.x64_mode && insn.rex {
        let mut rex = 0b01000000;
        if insn.rex_w { rex |= 1 << 3; }
        match insn.modrm_reg {
            X86Reg::R8  |
            X86Reg::R9  |
            X86Reg::R10 |
            X86Reg::R11 |
            X86Reg::R12 |
            X86Reg::R13 |
            X86Reg::R14 |
            X86Reg::R15 => rex |= 1 << 2, // rex.r
            _ => {},
        }

        match insn.modrm_rm {
            X86Reg::R8  |
            X86Reg::R9  |
            X86Reg::R10 |
            X86Reg::R11 |
            X86Reg::R12 |
            X86Reg::R13 |
            X86Reg::R14 |
            X86Reg::R15 => rex |= 1, // rex.b
            _ => {},
        }

        if insn.rex_b { rex |= 1 }
        x86.machine_code.push(rex);
    }

    if !x86.x64_mode {
        // prefix, 32-bit insnerand size, 32-bit address size.
        x86.machine_code.push(0x66);
        x86.machine_code.push(0x67);
    }

    match insn.opcode {
        X86OpCode::Int3 => x86.machine_code.push(0xcc),
        X86OpCode::Mov => match insn.encoding {
            X86OpEn::MR => x86.machine_code.push(0x89),
            X86OpEn::RM => x86.machine_code.push(0x8b),
            X86OpEn::MI => x86.machine_code.push(0xc7),
            _ => unimplemented!(),
        },

        X86OpCode::Add => match insn.encoding {
            X86OpEn::MR => x86.machine_code.push(0x01),
            X86OpEn::RM => x86.machine_code.push(0x03),
            X86OpEn::MI => x86.machine_code.push(0x81),
            _ => unimplemented!(),
        },

        X86OpCode::Push => match insn.encoding {
            X86OpEn::O => x86.machine_code.push(0x50 + register_encoding(insn.modrm_reg)),
            _ => unimplemented!(),
        }
        
        X86OpCode::Pop => match insn.encoding {
            X86OpEn::O => x86.machine_code.push(0x58 + register_encoding(insn.modrm_reg)),
            _ => unimplemented!(),
        }

        X86OpCode::Ret => match insn.encoding {
            X86OpEn::ZO => x86.machine_code.push(0xc3),
            _ => unimplemented!(),
        }
    }

    if let X86AddrMode::None = insn.modrm_addr_mode {
    } else {
        let mut modrm = match insn.modrm_addr_mode {
            X86AddrMode::Indirect => 0b00000000,
            X86AddrMode::IndirectDisp8 => 0b01000000,
            X86AddrMode::IndirectDisp32 => 0b10000000,
            _ => 0b11000000,
        };
        modrm |= register_encoding(insn.modrm_reg) << 3;
        modrm |= register_encoding(insn.modrm_rm);
        x86.machine_code.push(modrm);
    }

    fn push_x86_value(x86: &mut X86Assembler, val: X86Value) {
        match val {
            X86Value::Int8(v) => x86.machine_code.push(v as u8),
            X86Value::Int16(v) => {
                x86.machine_code.push((v        & 0xFFi16) as u8);
                x86.machine_code.push(((v >> 8) & 0xFFi16) as u8);
            }
            X86Value::Int32(v) => {
                x86.machine_code.push((v         & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 8)  & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 16) & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 24) & 0xFFi32) as u8);
            }
            _ => {},
        }
    }

    push_x86_value(x86, insn.displacement);
    push_x86_value(x86, insn.immediate);
}
