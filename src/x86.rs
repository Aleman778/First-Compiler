use std::collections::HashMap;
use std::convert::TryInto;
use crate::ir::*;

pub struct X86Assembler {
    pub machine_code: Vec<u8>,
    pub instructions: Vec<X86Instruction>,
    pub jump_targets: HashMap<IrLabel, X86JumpTarget>,
    pub x64_mode: bool, // 64-bit mode changes the instruction encoding
}

pub struct X86JumpTarget {
    pub label: IrLabel, // the target label
    pub jumps: Vec<usize>, // jump instructions to this label
    pub pos: usize, // stores index into x86.instructions
}

// NOTE(alexander): defines parts of an x86 instruction encoding, sib is not used!
pub struct X86Instruction {
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
    byte_operands   : bool,
    cached_pos      : usize, // instructions byte pos in machine code
    cached_size     : usize,
}

#[derive(Debug, Clone, Copy)]
enum X86OpCode {
    NOP,
    MOV,
    MOVSX,
    ADD,
    SUB,
    IMUL,
    IDIV,
    XOR,
    CDQ,
    CMP,
    TEST,
    JL,
    JLE,
    JG,
    JGE,
    JE,
    JNE,
    JMP,
    PUSH,
    POP,
    RET,
    INT3,
}

#[derive(Debug, Clone, Copy)]
enum X86AddrMode {
    Indirect,       // mod = 00
    IndirectDisp8,  // mod = 01
    IndirectDisp32, // mod = 10
    Direct,         // mod = 11
    None,           // don't use modrm
}

#[derive(Debug, Clone, Copy)]
enum X86Value {
    None,
    Int8(i8),
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
    RM,  // modrm_reg, modrm_rm
    MR,  // modrm_rm, modrm_reg
    MI,  // modrm_rm, immediate
    RMI, // modrm_reg, modrm_rm, immediate
    M,   // modrm_rm,
    O,   // opcode + modrm_reg
    OI,  // opcode + modrm_reg, immediate
    I,   // immediate
    D,   // immediate (signed offset)
    ZO,  // NO OPERANDS
}

fn create_x86_instruction() -> X86Instruction {
    X86Instruction {
        rex             : false,
        rex_w           : false,
        rex_r           : false,
        rex_b           : false,
        opcode          : X86OpCode::INT3,
        modrm_addr_mode : X86AddrMode::None,
        modrm_reg       : X86Reg::RAX,
        modrm_rm        : X86Reg::RAX,
        displacement    : X86Value::None,
        immediate       : X86Value::None,
        encoding        : X86OpEn::ZO,
        byte_operands   : false,
        cached_pos      : 0,
        cached_size     : 0,
    }
}

fn create_x86_jump_target(label: IrLabel) -> X86JumpTarget {
    X86JumpTarget {
        label,
        jumps: Vec::new(),
        pos: 0,
    }
}

fn get_mut_x86_jump_target<'a>(x86: &'a mut X86Assembler, label: IrLabel) -> &'a mut X86JumpTarget {
    match x86.jump_targets.get(&label) {
        Some(_) => {}
        None => {
            x86.jump_targets.insert(label, create_x86_jump_target(label));
        }
    }

    x86.jump_targets.get_mut(&label).unwrap()
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
        instructions: Vec::new(),
        jump_targets: HashMap::new(),
        x64_mode: cfg!(target_arch="x86_64"),
    }
}

pub fn compile_x86_ir(x86: &mut X86Assembler, instructions: &Vec<IrInstruction>) {
    // Compile IR to x86 instruction encoding
    for insn in instructions {
        compile_x86_ir_instruction(x86, insn);
    }

    // Compile x86 assembly intructions into machine code
    compile_x86_machine_code(x86);

    // Calculate the relative jump distances
    for (_, jt) in &x86.jump_targets {
        let target_index = jt.pos;
        let target_insn = &x86.instructions[target_index];
        let target_byte_pos = target_insn.cached_pos as isize;

        for jump_index in &jt.jumps {
            let insn = &x86.instructions[*jump_index];
            let insn_byte_pos = insn.cached_pos as isize;
            let insn_size = insn.cached_size as isize;

            let rel32: i32 = (target_byte_pos - insn_byte_pos - insn_size).try_into().unwrap();
            let rel8_opt: Result<i8, _> = rel32.try_into();
            match rel8_opt {
                Ok(rel8) => {
                    let rel8_index = insn.cached_pos + insn.cached_size - 1;
                    x86.machine_code[rel8_index] = if rel8 < 0 {
                        (256i32 + rel8 as i32) as u8 // signed two's complement in u8
                    } else {
                        rel8 as u8
                    };
                },

                Err(_) => panic!("32-bit jumps is not implemented!"),
            };
        }
    }
}

fn compile_x64_ir_operand(x86: &mut X86Assembler, insn: &mut X86Instruction, op1: IrOperand, op2: IrOperand) {
    if let IrType::I8 = op1.ty {
        insn.byte_operands = true;
    }

    fn stack_operand(insn: &mut X86Instruction, offset: usize) {
        if offset > 127 {
            insn.modrm_addr_mode = X86AddrMode::IndirectDisp32;
            insn.displacement = X86Value::Int32(-(offset as i32));
        } else if offset > 0 {
            insn.modrm_addr_mode = X86AddrMode::IndirectDisp8;
            insn.displacement = X86Value::Int8(-(offset as i8));
        } else {
            insn.modrm_addr_mode = X86AddrMode::Indirect;
        }
        insn.modrm_rm = X86Reg::RBP;
    }

    match op1.kind {
        IrOperandKind::Stack(offset) => stack_operand(insn, offset),
        IrOperandKind::Register(register) => {
            insn.modrm_addr_mode = X86AddrMode::Direct;
            insn.modrm_reg = get_scratch_register(x86, register);
        },

        IrOperandKind::Constant(_) => panic!("cannot encode constant as first operand"),

        _ => return,
    }

    match op2.kind {
        IrOperandKind::Stack(offset) => {
            insn.encoding = X86OpEn::RM;
            match op1.kind {
                IrOperandKind::Stack(_) => panic!("cannot encode memory access for both operands!"),
                IrOperandKind::Register(_) => {},
                _ => panic!("unexpected first operand"),
            };

            stack_operand(insn, offset);
        }
        IrOperandKind::Register(reg) => {
            match op1.kind {
                IrOperandKind::Stack(_) => {
                    insn.modrm_reg = get_scratch_register(x86, reg);
                    insn.encoding = X86OpEn::MR;
                }

                IrOperandKind::Register(_) => {
                    insn.modrm_rm = get_scratch_register(x86, reg);
                    insn.encoding = X86OpEn::RM;
                }

                _ => panic!("unexpected first operand"),
            };
        }

        IrOperandKind::Constant(val) => {
            match op1.kind {
                IrOperandKind::Register(_) => {
                    insn.modrm_rm = insn.modrm_reg;
                    // NOTE(alexander): insn.modrm_reg gets set later.
                }
                _ => {}
            }

            insn.encoding = X86OpEn::MI;
            insn.immediate = ir_value_to_x86_value(val);
        }

        _ => return,
    }
}

/**
 * Compiles an ir instruction into one or many x86 instructions that are pushed
 * on the final x86 instructions list.
 */
pub fn compile_x86_ir_instruction(x86: &mut X86Assembler, ir_insn: &IrInstruction) {
    match ir_insn.opcode {
        IrOpCode::Nop => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::NOP;
            insn.encoding = X86OpEn::ZO;
            x86.instructions.push(insn);
            // NOTE(alexander): is it even worth pushing a NOP?
        },

        IrOpCode::Breakpoint => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::INT3;
            insn.encoding = X86OpEn::ZO;
            x86.instructions.push(insn);
        }

        IrOpCode::Copy => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::MOV;
            compile_x64_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Clear => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::XOR;
            compile_x64_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op1);
            x86.instructions.push(insn);
        }
        
        IrOpCode::Add => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::ADD;
            compile_x64_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Sub => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::SUB;
            compile_x64_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Mul => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::IMUL; // NOTE(alexander): we only support signed numbers atm.
            compile_x64_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            match insn.encoding {
                
                X86OpEn::MR => panic!("not possible"), // TODO(alexander): mov in to register first
                X86OpEn::MI => insn.encoding = X86OpEn::RMI,
                _ => { },
            }
            
            x86.instructions.push(insn);
        }

        IrOpCode::Div => {
            // save rdx & rax
            
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::IDIV; // NOTE(alexander): we only support signed numbers atm.
            compile_x64_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }
        
        IrOpCode::And => {

        }

        IrOpCode::Or => {

        }

        IrOpCode::IfLt |
        IrOpCode::IfGt |
        IrOpCode::IfLe |
        IrOpCode::IfGe |
        IrOpCode::IfEq |
        IrOpCode::IfNe => {
            let mut cmp_insn = create_x86_instruction();
            cmp_insn.opcode = X86OpCode::CMP;
            compile_x64_ir_operand(x86, &mut cmp_insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(cmp_insn);

            if let IrOperandKind::Label(label) = ir_insn.op3.kind {
                let insn_index = x86.instructions.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.jumps.push(insn_index);
            } else {
                panic!("missing label on third operand in ifcc ir instruction")
            }

            let mut jump_insn = create_x86_instruction();
            match ir_insn.opcode {
                IrOpCode::IfLt => jump_insn.opcode = X86OpCode::JL,
                IrOpCode::IfGt => jump_insn.opcode = X86OpCode::JG,
                IrOpCode::IfLe => jump_insn.opcode = X86OpCode::JLE,
                IrOpCode::IfGe => jump_insn.opcode = X86OpCode::JGE,
                IrOpCode::IfEq => jump_insn.opcode = X86OpCode::JE,
                IrOpCode::IfNe => jump_insn.opcode = X86OpCode::JNE,
                _ => panic!(),
            }
            jump_insn.immediate = X86Value::Int8(0); // jump distance gets calculated later
            jump_insn.encoding = X86OpEn::D;
            x86.instructions.push(jump_insn);
        },

        IrOpCode::Jump => {
            if let IrOperandKind::Label(label) = ir_insn.op1.kind {
                let insn_index = x86.instructions.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.jumps.push(insn_index);
            } else {
                panic!("missing label on first operand in jump instruction")
            }

            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::JMP;

            insn.immediate = X86Value::Int8(0); // jump distance gets calculated later
            insn.encoding = X86OpEn::D;
            x86.instructions.push(insn);
        }

        IrOpCode::Label => {
            if let IrOperandKind::Label(label) = ir_insn.op1.kind {
                let insn_index = x86.instructions.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.pos = insn_index;
            }
        }

        IrOpCode::Return => {
            match ir_insn.op1.kind {
                IrOperandKind::None => {},

                _ => {
                    if let IrOperandKind::Register(0) = ir_insn.op1.kind {
                        if let IrType::I32 = ir_insn.op1.ty {
                            return;
                        }
                    };

                    let mut insn = create_x86_instruction();
                    if let IrType::I8 = ir_insn.op1.ty {
                        insn.opcode = X86OpCode::MOVSX;
                    } else {
                        insn.opcode = X86OpCode::MOV;
                    }

                    let op1 = IrOperand {
                        ty: IrType::None,
                        kind: IrOperandKind::Register(0)
                    };
                    compile_x64_ir_operand(x86, &mut insn, op1, ir_insn.op1);
                    x86.instructions.push(insn);
                }
            }

            // TODO(alexander): handle returns that are not at the end of the function.
        }

        IrOpCode::Prologue => {
            // push rbp
            let mut insn = create_x86_instruction();
            insn.modrm_reg = X86Reg::RBP;
            insn.opcode = X86OpCode::PUSH;
            insn.encoding = X86OpEn::O;
            x86.instructions.push(insn);

            // mov rbp rsp
            let mut insn = create_x86_instruction();
            insn.rex = true;
            insn.rex_w = true;
            insn.opcode = X86OpCode::MOV;
            insn.modrm_addr_mode = X86AddrMode::Direct;
            insn.modrm_reg = X86Reg::RBP;
            insn.modrm_rm  = X86Reg::RSP;
            insn.encoding  = X86OpEn::RM;
            x86.instructions.push(insn);
        }

        IrOpCode::Epilogue => {
            // pop rbp
            let mut insn = create_x86_instruction();
            insn.modrm_reg = X86Reg::RBP;
            insn.opcode = X86OpCode::POP;
            insn.encoding = X86OpEn::O;
            x86.instructions.push(insn);

            // ret
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::RET;
            insn.encoding = X86OpEn::ZO;
            x86.instructions.push(insn);
        }

        _ => unimplemented!(),
    }
}

fn compile_x86_machine_code(x86: &mut X86Assembler) {
    for insn in &mut x86.instructions {
        insn.cached_pos = x86.machine_code.len();
        
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
            X86OpCode::NOP => x86.machine_code.push(0x90),
            X86OpCode::INT3 => x86.machine_code.push(0xcc),

            X86OpCode::MOV => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::MR, true)  => x86.machine_code.push(0x88),
                (X86OpEn::MR, false) => x86.machine_code.push(0x89),

                (X86OpEn::RM, true)  => x86.machine_code.push(0x8a),
                (X86OpEn::RM, false) => x86.machine_code.push(0x8b),

                (X86OpEn::MI, false) => x86.machine_code.push(0xc7),
                (X86OpEn::MI, true)  => x86.machine_code.push(0xc6),
                _ => unimplemented!(),
            }

            X86OpCode::MOVSX => match insn.encoding {
                X86OpEn::RM => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0xbe);
                }
                _ => unimplemented!(),
            }

            X86OpCode::ADD => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x05),
                X86OpEn::MI => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                X86OpEn::MR => x86.machine_code.push(0x01),
                X86OpEn::RM => x86.machine_code.push(0x03),
                _ => unimplemented!(),
            }

            X86OpCode::SUB => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x2d),
                X86OpEn::MI => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RBP; // reg = 5
                }
                X86OpEn::MR => x86.machine_code.push(0x29),
                X86OpEn::RM => x86.machine_code.push(0x2b),
                _ => unimplemented!(),
            }

            X86OpCode::IMUL => match insn.encoding {
                X86OpEn::M => {
                    x86.machine_code.push(0xf7);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                X86OpEn::RM => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0xaf);
                }
                X86OpEn::RMI => x86.machine_code.push(0x69),
                _ => unimplemented!(),
            }

            X86OpCode::IDIV => match insn.encoding {
                X86OpEn::M => {
                    x86.machine_code.push(0xf7);
                    insn.modrm_reg = X86Reg::RDI; // reg = 7
                }
                _ => unimplemented!(),
            }

            X86OpCode::XOR => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x35),
                X86OpEn::MI => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RSI; // reg = 6
                }
                X86OpEn::MR => x86.machine_code.push(0x31),
                X86OpEn::RM => x86.machine_code.push(0x33),
                _ => unimplemented!(),
            }

            X86OpCode::CDQ => match insn.encoding {
                X86OpEn::ZO => x86.machine_code.push(0x99),
                _ => unimplemented!(),
            }

            X86OpCode::CMP => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::I, true)   => x86.machine_code.push(0x3c),
                (X86OpEn::I, false)  => x86.machine_code.push(0x3d),

                (X86OpEn::MI, true)  => x86.machine_code.push(0x80),
                (X86OpEn::MI, false) => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RDI; // NOTE(alexander): this makes no sense!
                }

                (X86OpEn::MR, true)  => x86.machine_code.push(0x38),
                (X86OpEn::MR, false) => x86.machine_code.push(0x39),

                (X86OpEn::RM, true)  => x86.machine_code.push(0x3a),
                (X86OpEn::RM, false) => x86.machine_code.push(0x3b),
                _ => unimplemented!(),
            }

            X86OpCode::TEST => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0xa9),
                X86OpEn::MI => {
                    x86.machine_code.push(0xf7);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                X86OpEn::MR => x86.machine_code.push(0x85),
                _ => unimplemented!(),
            }

            X86OpCode::JL => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7c),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8c);
                    }
                    _ => unimplemented!(),
                }
                _ => unimplemented!(),
            }

            X86OpCode::JLE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7e),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8e);
                    }
                    _ => unimplemented!(),
                }
                _ => unimplemented!(),
            }

            X86OpCode::JG => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7f),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8f);
                    }
                    _ => unimplemented!(),
                }
                _ => unimplemented!(),
            }

            X86OpCode::JGE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7d),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8d);
                    }
                    _ => unimplemented!(),
                }
                _ => unimplemented!(),
            }

            X86OpCode::JE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x74),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x84);
                    }
                    _ => unimplemented!(),
                }
                _ => unimplemented!(),
            }

            X86OpCode::JNE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x75),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x85);
                    }
                    _ => unimplemented!(),
                }
                _ => unimplemented!(),
            }

            X86OpCode::JMP => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0xeb),
                    X86Value::Int32(_) => x86.machine_code.push(0xe9),
                    _ => unimplemented!(),
                }
                X86OpEn::M => x86.machine_code.push(0xff),
                _ => unimplemented!(),
            }

            X86OpCode::PUSH => match insn.encoding {
                X86OpEn::O => x86.machine_code.push(0x50 + register_encoding(insn.modrm_reg)),
                _ => unimplemented!(),
            }

            X86OpCode::POP => match insn.encoding {
                X86OpEn::O => x86.machine_code.push(0x58 + register_encoding(insn.modrm_reg)),
                _ => unimplemented!(),
            }

            X86OpCode::RET => match insn.encoding {
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

        match insn.displacement {
            X86Value::Int8(v) => x86.machine_code.push(v as u8),
            X86Value::Int32(v) => {
                x86.machine_code.push((v         & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 8)  & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 16) & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 24) & 0xFFi32) as u8);
            }
            _ => {},
        }

        match insn.immediate {
            X86Value::Int8(v) => x86.machine_code.push(v as u8),
            X86Value::Int32(v) => {
                x86.machine_code.push((v         & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 8)  & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 16) & 0xFFi32) as u8);
                x86.machine_code.push(((v >> 24) & 0xFFi32) as u8);
            }
            _ => {},
        }

        insn.cached_size = x86.machine_code.len() - insn.cached_pos;
    }
}
