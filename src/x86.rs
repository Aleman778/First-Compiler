use std::collections::HashMap;
use crate::lir::{LirInstruction, LirLabel, LirValue, LirOpCode, LirOperand};

pub struct X86Assembler {
    pub machine_code: Vec<u8>,
    pub labels: HashMap<LirLabel, usize>, // stores machine code byte positions
    pub x64_mode: bool, // 64-bit mode changes the instruction encoding
}

// NOTE(alexander): defines parts of an x86 operation encoding
struct X86Operation {
    rex_w           : bool, // 64-bit mode instructions
    rex_r           : bool, // 64-bit mode mod_reg
    rex_b           : bool, // 64-bit mode mod_rm
    use_rex         : bool, // use REX prefix, ignored in 32-bit mode
    opcode          : [u8; 3],
    opcode_bytes    : u8,
    modrm_addr_mode : X86AddressingMode,
    modrm_reg       : X86Reg,
    modrm_rm        : X86Reg,
    use_modrm       : bool,
    displacement    : X86Value,
    immediate       : X86Value,
    encoding        : X86OpEncoding,
}

enum X86AddressingMode {
    Indirect,       // mod = 00
    IndirectDisp8,  // mod = 01
    IndirectDisp32, // mod = 10
    Direct,         // mod = 11
}

// NOTE(alexander): x86 does not care about signedness, let everything be signed since
// this compiler only has support for signed integers.
enum X86Value {
    None,
    Int8(i8),
    Int16(i16),
    Int32(i32),
}

// NOTE(alexander): 64-bit register notation used, bitwidth is determined by the prefix or opcode.
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

enum X86OpEncoding {
    RM, // modrm_reg, modrm_rm
    MR, // modrm_rm,  modrm_reg
    MI, // modrm_rm,  immediate
    O,  // modrm_reg
}

fn create_x86_operation() -> X86Operation {
    X86Operation {
        rex_w           : false,
        rex_r           : false,
        rex_b           : false,
        use_rex         : false,
        opcode          : [0; 3],
        opcode_bytes    : 1,
        modrm_addr_mode : X86AddressingMode::Direct,
        modrm_reg       : X86Reg::RAX,
        modrm_rm        : X86Reg::RAX,
        use_modrm       : false,
        displacement    : X86Value::None,
        immediate       : X86Value::None,
        encoding        : X86OpEncoding::RM,
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

fn get_scratch_register(x86: &mut X86Assembler, lir_register: usize) -> X86Reg {
    if x86.x64_mode {
        match lir_register { // TODO(alexander): probably want to prioritize r8, r9, ... higher!
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
        match lir_register {
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

// fn register_name(reg: &X86Reg, bits: u32) -> &'static str {
//     if bits == 64 {
//         match reg {
//             X86Reg::RAX => "rax",
//             X86Reg::RCX => "rcx",
//             X86Reg::RDX => "rdx",
//             X86Reg::RBX => "rbx",
//             X86Reg::RSP => "rsp",
//             X86Reg::RBP => "rbp",
//             X86Reg::RSI => "rsi",
//             X86Reg::RDI => "rdi",
//             X86Reg::R8  => "r8",
//             X86Reg::R9  => "r9",
//             X86Reg::R10 => "r10",
//             X86Reg::R11 => "r11",
//             X86Reg::R12 => "r12",
//             X86Reg::R13 => "r13",
//             X86Reg::R14 => "r14",
//             X86Reg::R15 => "r15",
//         }
//     } else if bits == 32 {
//         match reg {
//             X86Reg::RAX => "eax",
//             X86Reg::RCX => "ecx",
//             X86Reg::RDX => "edx",
//             X86Reg::RBX => "ebx",
//             X86Reg::RSP => "esp",
//             X86Reg::RBP => "ebp",
//             X86Reg::RSI => "esi",
//             X86Reg::RDI => "edi",
//             X86Reg::R8  => "r8d",
//             X86Reg::R9  => "r9d",
//             X86Reg::R10 => "r10d",
//             X86Reg::R11 => "r11d",
//             X86Reg::R12 => "r12d",
//             X86Reg::R13 => "r13d",
//             X86Reg::R14 => "r14d",
//             X86Reg::R15 => "r15d",
//         }
//     } else if bits == 8 {
//         match reg {
//             X86Reg::RAX => "al",
//             X86Reg::RCX => "cl",
//             X86Reg::RDX => "dl",
//             X86Reg::RBX => "bl",
//             X86Reg::RSP => "spl", // ah, ch, dh, bh used if REX is not set
//             X86Reg::RBP => "bpl",
//             X86Reg::RSI => "sil",
//             X86Reg::RDI => "dil",
//             X86Reg::R8  => "r8b",
//             X86Reg::R9  => "r9b",
//             X86Reg::R10 => "r10b",
//             X86Reg::R11 => "r11b",
//             X86Reg::R12 => "r12b",
//             X86Reg::R13 => "r13b",
//             X86Reg::R14 => "r14b",
//             X86Reg::R15 => "r15b",
//         }
//     }
// }

fn lir_value_to_x86_value(val: LirValue) -> X86Value {
    match val {
        LirValue::I32(v) => X86Value::Int32(v),
        LirValue::Bool(v) => if v {
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

pub fn compile_x86_lir(x86: &mut X86Assembler, instructions: &Vec<LirInstruction>) {
    for insn in instructions {
        match insn.opcode {
          
            _ => compile_x86_lir_instruction(x86, insn),
        }
    }

    // FIXME(alexander): hardcoded ret instruction
}

fn compile_x64_lir_operand(x86: &mut X86Assembler, op: &mut X86Operation, op1: LirOperand, op2: LirOperand) {
    fn stack_operand(op: &mut X86Operation, offset: usize) {
        op.use_modrm = true;
        if offset > 127 {
            op.modrm_addr_mode = X86AddressingMode::IndirectDisp32;
            op.displacement = X86Value::Int32(-(offset as i32));
        } else if offset > 0 {
            op.modrm_addr_mode = X86AddressingMode::IndirectDisp8;
            op.displacement = X86Value::Int8(-(offset as i8));
        } else {
            op.modrm_addr_mode = X86AddressingMode::Indirect;
        }
        op.modrm_rm = X86Reg::RBP;
    }

    match op1 {
        LirOperand::Stack(offset) => stack_operand(op, offset),
        LirOperand::Register(register) => {
            op.modrm_reg = get_scratch_register(x86, register);
            op.use_modrm = true;
        },

        LirOperand::Constant(_) => panic!("cannot encode constant as first operand"),

        _ => return,
    }

    match op2 {
        LirOperand::Stack(offset) => {
            op.encoding = X86OpEncoding::RM;
            match op1 {
                LirOperand::Stack(_) => panic!("cannot encode memory access for both operands!"),
                LirOperand::Register(_) => {},
                _ => panic!("unexpected first operand"),
            };

            stack_operand(op, offset);
        }
        LirOperand::Register(reg) => {
            match op1 {
                LirOperand::Stack(_) => {
                    op.modrm_reg = get_scratch_register(x86, reg);
                    op.encoding = X86OpEncoding::MR;
                }

                LirOperand::Register(_) => {
                    op.modrm_addr_mode = X86AddressingMode::Direct;
                    op.modrm_rm = get_scratch_register(x86, reg);
                    op.encoding = X86OpEncoding::RM;
                }

                _ => panic!("unexpected first operand"),
            };

            op.use_modrm = true;
        }

        LirOperand::Constant(val) => {
            match op1 {
                LirOperand::Register(reg) => {
                    // use r/m instead!
                    op.modrm_rm = op.modrm_reg;
                    op.modrm_reg = 0;
                }
                _ => {}
            }
            
            op.encoding = X86OpEncoding::MI;
            op.immediate = lir_value_to_x86_value(val);
        }

        _ => return,
    }
}

pub fn compile_x86_lir_instruction(x86: &mut X86Assembler, insn: &LirInstruction) {
    // First handle specfial lir instructions, usually expands into multiple x86 instructions.
    match insn.opcode {
        LirOpCode::Prologue => {
            let op = create_x86_operation();
            
            x86.machine_code.push(0x50 + register_encoding(X86Reg::RBP)); // push rbp
            let op = create_x86_operation();
            op.use_rex = true;
            op.rex_w = true;
            x86.machine_code.push();
            x86.machine_code.push(0x8b);
            x86.machine_code.push(0b11101100);
            push_encoded_x86_operation(x86, op); // mov rbp rsp
            return;
        }
        
        LirOpCode::Epilogue => {
            x86.machine_code.push(0x50 + register_encoding(X86Reg::RBP)); // pop rbp
            x86.machine_code.push(0xc3); // ret (near)
            return;
        }

        
        LirOpCode::Label => {
            // there is no concept of labels in machine code, just store the byte offset of this label.
            if let LirOperand::Label(label) = insn.op1 {
                x86.labels.insert(insn.op1, x86.machine_code.len());
            }
            return;
        }
        
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
                    compile_x86_lir_instruction(x86, &mov_insn);
                }
            }

            x86.machine_code.push(0xc3); //cb - ret far
            return;
        }

    }

    // Now handle the rest of the lir instructions, usually only requires a single x86 instruction.
    let mut op = create_x86_operation();
    compile_x64_lir_operand(x86, op, insn.op1, insn.op2);

    // get the corresponding opcode from the encoding scheme + lir opcode
    match insn.opcode {
        LirOpCode::Copy => {
            op.opcode_bytes = 1;
            match op.encoding {
                X86OpEncoding::MR => op.opcode[0] = 0x89,
                X86OpEncoding::RM => op.opcode[0] = 0x8b,
                X86OpEncoding::MI => op.opcode[0] = 0xc7,
            }
        }

        LirOpCode::Add => {
            op.opcode_bytes = 1;
            match op.encoding {
                X86OpEncoding::MR => op.opcode[0] = 0x01,
                X86OpEncoding::RM => op.opcode[0] = 0x03,
                X86OpEncoding::MI => op.opcode[0] = 0x81,
            }
        }

        _ => return,
    };

    push_encoded_x86_operation(x86, &op);
}

fn push_encoded_x86_operation(x86: &mut X86Assembler, op: &X86Operation) {
    if x86.x64_mode && op.use_rex {
        let rex = 0b01000000;
        if op.rex_w { rex |= 1 << 3; }
        match op.modrm_reg {
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

        match op.modrm_rm {
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

        { rex |= 1 << 2; }
        if op.rex_b { rex |= 1 }
        x86.machine_code.push(rex);
    }

    if !x86.x64_mode {
        // prefix, 32-bit operand size, 32-bit address size.
        x86.machine_code.push(0x66);
        x86.machine_code.push(0x67);
    }

    for i in 0..op.opcode_bytes {
        x86.machine_code.push(op.opcode[i]);
    }

    if op.use_modrm {
        let modrm = match op.modrm_addr_mode {
            Indirect => 0b11000000,
            IndirectDisp8 => 0b01000000,
            IndirectDisp32 => 0b10000000,
            Direct => 0b00000000,
        };
        modrm |= register_encoding(op.modrm_reg) << 3;
        modrm |= register_encoding(op.modrm_rm);
        x86.machine_code.push(modrm);
    }

    fn push_x86_value(x86: &mut X86Assembler, val: &X86Value) {
        match val {
            X86Value::Int8(v) => x86.machine_code.push(v),
            X86Value::Int16(v) => {
                x86.machine_code.push((v && 0xFFi16) as u8);
                x86.machine_code.push((v >> 8 && 0xFFi16) as u8);
            }
            X86Value::Int32(v) => {
                x86.machine_code.push((v && 0xFFi16) as u8);
                x86.machine_code.push((v >> 8 && 0xFFi32) as u8);
                x86.machine_code.push((v >> 16 && 0xFFi32) as u8);
                x86.machine_code.push((v >> 24 && 0xFFi32) as u8);
            }
            _ => {},
        }
    }

    push_x86_value(x86, op.displacement);
    push_x86_value(x86, op.immediate);
}
