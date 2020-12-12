// use std::collections::HashMap;
use crate::lir::{LirInstruction, LirValue, LirOpCode, LirOperand};

pub struct X86Assembler {
    pub machine_code: Vec<u8>,

    // TODO(alexander): handle jump targets.
    // labels: HashMap<LirLabel, usize>, // stores machine code byte positions

    pub x64_mode: bool, // 64-bit mode changes the instruction encoding
}

// NOTE(alexander): defines parts of an x86 operation encoding 
struct X86Op {
    rex_prefix         : u8, // ignored in 32-bit mode
    opcode             : u64,
    opcode_bytes       : u8, // opcode is 1 - 3 bytes
    modrm              : u8, // memory + register access byte (with direct/ indirect addressing)
    use_modrm          : bool,
    sib                : u8, // scale index base byte
    use_sib            : bool,
    displacement       : i64,
    displacement_bytes : u8, // 0, 1, 2 or 4 bytes
    immediate          : i32,
    immediate_bytes    : u8, // 0, 1, 2 or 4 bytes
}

// NOTE(alexander): ST and XMM are not considered since here since no floating point numbers are supported!
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

// NOTE(alexander): operation encoding scheme
enum X86OpEn {
    RM, // reg, r/m
    MR, // r/m, reg
    MI, // r/m, imm
}

fn create_x86_op() -> X86Op {
    X86Op {
        rex_prefix         : 0b01000000,
        opcode             : 0,
        opcode_bytes       : 1,
        modrm              : 0,
        use_modrm          : false,
        sib                : 0,
        use_sib            : false,
        displacement       : 0,
        displacement_bytes : 0,
        immediate          : 0,
        immediate_bytes    : 0,
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
        X86Reg::R8  => 0, // NOTE(alexander): set REX.R = 1, only in 64-bit mode
        X86Reg::R9  => 1,
        X86Reg::R10 => 2,
        X86Reg::R11 => 3,
        X86Reg::R12 => 4,
        X86Reg::R13 => 5,
        X86Reg::R14 => 6,
        X86Reg::R15 => 7,
    }
}

fn get_scratch_register(_ic: &mut X86Assembler, lir_register: u64) -> X86Reg {
    match lir_register { // NOTE(alexander): RSP and RBP are never used as scratch register.
        0  => X86Reg::RAX,
        1  => X86Reg::RCX,
        2  => X86Reg::RDX,
        3  => X86Reg::RBX,
        4  => X86Reg::RSI,
        5  => X86Reg::RDI,
        6  => X86Reg::R8, // NOTE(alexander): 6 and above are only available in 64-bit mode
        7  => X86Reg::R9,
        8  => X86Reg::R10,
        9  => X86Reg::R11,
        10 => X86Reg::R12,
        11 => X86Reg::R13,
        12 => X86Reg::R14,
        13 => X86Reg::R15,
        _ => panic!("running out of registers!")
    }
}

fn to_immediate(val: LirValue) -> i32 {
    match val {
        LirValue::I32(v) => v,
        LirValue::Bool(v) => if v {
            1
        } else {
            0
        },
    }
}

fn push_encoded_x86_op(x86: &mut X86Assembler, op: &X86Op) {
    if x86.x64_mode {
        x86.machine_code.push(op.rex_prefix);
    }

    for i in 0..op.opcode_bytes {
        x86.machine_code.push(((op.opcode >> (8*i)) & 0xFFu64) as u8);
    }

    if op.use_modrm {
        x86.machine_code.push(op.modrm);
    }

    if op.use_sib {
        x86.machine_code.push(op.sib);
    }

    for i in 0..op.displacement_bytes {
        x86.machine_code.push(((op.displacement >> (8*i)) & 0xFFi64) as u8);
    }

    for i in 0..op.immediate_bytes {
        x86.machine_code.push(((op.immediate >> (8*i)) & 0xFFi32) as u8);
    }
}

pub fn create_x86_assembler() -> X86Assembler {
    X86Assembler {
        machine_code: Vec::new(),
        x64_mode: cfg!(target_arch="x86_64"),
    }
}

pub fn compile_x86_lir(x86: &mut X86Assembler, instructions: &Vec<LirInstruction>) {
    for insn in instructions {
        match insn.opcode {
            LirOpCode::Return => {
                match insn.op1 {
                    LirOperand::None |
                    LirOperand::Register(0) => { }

                    _ => {
                        let mov_insn = LirInstruction {
                            opcode: LirOpCode::Copy,
                            op1: LirOperand::Register(0),
                            op2: insn.op2,
                            span: insn.span,
                            ..Default::default()
                        };
                        compile_x86_basic_lir_instruction(x86, &mov_insn);
                    }
                }
                
                x86.machine_code.push(0b01000000);
                x86.machine_code.push(0xc3); //cb - ret far
            }
            
            _ => compile_x86_basic_lir_instruction(x86, insn),
        }
    }

    // FIXME(alexander): hardcoded ret instruction
}

fn compile_x86_basic_lir_instruction(x86: &mut X86Assembler, insn: &LirInstruction) {
    let mut op = create_x86_op();
    let operator_encoding: X86OpEn;

    match insn.op1 {
        LirOperand::Stack(offset) => {
            op.displacement = -offset; // rbp - displacement
            op.use_modrm = true;
            if offset > 127 {
                // indirect 32-bit displacement
                op.modrm = 0b10000000;
                op.displacement_bytes = 4;
            } else if offset > 0 {
                // indirect 8-bit displacement
                op.modrm = 0b01000000;
                op.displacement_bytes = 1;
            } else {
                // indirect with no displacement
                op.modrm = 0b00000000;
                op.displacement_bytes = 0;
            }
            op.modrm |= register_encoding(X86Reg::RBP); // modrm.rm = RBP
        },

        LirOperand::Register(register) => {
            op.use_modrm = true;
            op.modrm = 0b11000000; // as default use no displacement, op2 may change this though
            if register > 7 && !x86.x64_mode {
                panic!("running out of registers, x86 only has 8 GPRs")
            }
            op.modrm |= register_encoding(get_scratch_register(x86, register)) << 3; // modrm.reg = register
        },

        LirOperand::Constant(_) => panic!("cannot encode constant as first operand"),

        _ => return,
    }

    match insn.op2 {
        LirOperand::Constant(val) => {
            operator_encoding = X86OpEn::MI;
            op.opcode_bytes = 1;
            op.immediate = to_immediate(val);
            op.immediate_bytes = 4;
        }

        LirOperand::Stack(offset) => {
            operator_encoding = match insn.op1 {
                LirOperand::Stack(_) => panic!("cannot encode memory access for both operands!"),
                LirOperand::Register(_) => X86OpEn::RM,
                _ => return,
            };
            op.displacement = -offset; // rbp - displacement
            op.use_modrm = true;
            if offset > 127 {
                // indirect 32-bit displacement
                op.modrm = 0b10000000;
                op.displacement_bytes = 4;
            } else if offset > 0 {
                // indirect 8-bit displacement
                op.modrm = 0b01000000;
                op.displacement_bytes = 1;
            } else {
                // indirect with no displacement
                op.modrm = 0b00000000;
                op.displacement_bytes = 0;
            }
            op.modrm |= register_encoding(X86Reg::RBP); // modrm.rm = RBP
        },

        LirOperand::Register(reg) => {
            operator_encoding = match insn.op1 {
                LirOperand::Stack(_) => X86OpEn::MR,
                LirOperand::Register(_) => {
                    op.modrm |= 0b11 << 6; // use direct addressing
                    X86OpEn::MR // both regs MR/RM is the same
                }
                _ => return,
            };

            op.modrm |= register_encoding(get_scratch_register(x86, reg)) << 3; // modrm.reg = register
        }

        _ => return,
    }

    // get the corresponding opcode from the encoding scheme + lir opcode
    op.opcode = match insn.opcode {
        LirOpCode::Copy => match operator_encoding {
            X86OpEn::MR => 0x89,
            X86OpEn::RM => 0x8b,
            X86OpEn::MI => 0xc7,
        }

        LirOpCode::Add => match operator_encoding {
            X86OpEn::MR => 0x01,
            X86OpEn::RM => 0x03,
            X86OpEn::MI => 0x81,
        }

        _ => return,
    };

    push_encoded_x86_op(x86, &op);
}

