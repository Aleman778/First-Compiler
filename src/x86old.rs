use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use crate::ast::intern_string;
use crate::ir::*;

pub struct X86Assembler {
    pub machine_code: Vec<u8>,
    pub instructions: Vec<X86Instruction>,
    pub functions: HashMap<IrIdent, IrBasicBlock>,
    pub jump_targets: HashMap<IrIdent, X86JumpTarget>,
    pub curr_call_frame: Option<X86CallFrame>,
    pub curr_function: IrIdent,
    pub x64_mode: bool,
}

pub struct X86CallFrame {
    pub size: isize,
    pub arguments: Vec<X86Instruction>,
}

pub struct X86JumpTarget {
    pub label: IrIdent, // the target label
    pub jumps: Vec<usize>, // jump instructions to this label
    pub pos: usize, // stores index into x86.instructions
}

// NOTE(alexander): defines parts of an x86 instruction encoding, sib is not used!
#[derive(Debug, Clone)]
pub struct X86Instruction {
    rex             : bool, // use REX prefix, ignored in 32-bit mode
    rex_w           : bool, // 64-bit mode instructions
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
enum X86AddrMode {
    Indirect,       // mod = 00 NOTE(alexander): never used don't care about SIB and RIP addressing!
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
    Int64(i64),
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

impl fmt::Display for X86OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86OpCode::NOP   => write!(f, "nop"),
            X86OpCode::MOV   => write!(f, "mov"),
            X86OpCode::MOVSX => write!(f, "movsx"),
            X86OpCode::LEA   => write!(f, "lea"),
            X86OpCode::ADD   => write!(f, "add"),
            X86OpCode::SUB   => write!(f, "sub"),
            X86OpCode::IMUL  => write!(f, "imul"),
            X86OpCode::IDIV  => write!(f, "idiv"),
            X86OpCode::AND   => write!(f, "and"),
            X86OpCode::OR    => write!(f, "or"),
            X86OpCode::XOR   => write!(f, "xor"),
            X86OpCode::CDQ   => write!(f, "cdq"),
            X86OpCode::CMP   => write!(f, "cmp"),
            X86OpCode::TEST  => write!(f, "test"),
            X86OpCode::SETL  => write!(f, "setl"),
            X86OpCode::SETG  => write!(f, "setg"),
            X86OpCode::SETLE => write!(f, "setle"),
            X86OpCode::SETGE => write!(f, "setge"),
            X86OpCode::SETE  => write!(f, "sete"),
            X86OpCode::SETNE => write!(f, "setne"),
            X86OpCode::JL    => write!(f, "jl"),
            X86OpCode::JLE   => write!(f, "jle"),
            X86OpCode::JG    => write!(f, "jg"),
            X86OpCode::JGE   => write!(f, "jge"),
            X86OpCode::JE    => write!(f, "je"),
            X86OpCode::JNE   => write!(f, "jne"),
            X86OpCode::JMP   => write!(f, "jmp"),
            X86OpCode::PUSH  => write!(f, "push"),
            X86OpCode::POP   => write!(f, "pop"),
            X86OpCode::CALL  => write!(f, "call"),
            X86OpCode::RET   => write!(f, "ret"),
            X86OpCode::INT3  => write!(f, "int3"),
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
            X86Value::None     => write!(f, ""),
        }
    }
}

impl fmt::Display for X86Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    {:<6} ", format!("{}", self.opcode))?;
        let ptr_str = if self.byte_operands {
            "byte ptr"
        } else {
            if self.rex_w {
                "qword ptr"
            } else {
                "dword ptr"
            }
        };

        let displacement_str = match self.displacement {
            X86Value::Int8(v) => if v >= 0 {
                format!("+ {}", v)
            } else {
                format!("- {}", -v)
            }

            X86Value::Int32(v) => if v >= 0 {
                format!("+ {}", v)
            } else {
                format!("- {}", -v)
            }

            X86Value::Int64(v) => if v >= 0 {
                format!("+ {}", v)
            } else {
                format!("- {}", -v)
            }
            
            X86Value::None => String::new(),
        };

        let modrm_rm_str = match self.modrm_addr_mode {
            X86AddrMode::Indirect => format!("{} [{}]", ptr_str, self.modrm_rm),
            X86AddrMode::IndirectDisp8 |
            X86AddrMode::IndirectDisp32 => format!("{} [{} {}]", ptr_str, self.modrm_rm, displacement_str),
            X86AddrMode::Direct => format!("{}", self.modrm_rm),
            X86AddrMode::None => String::new(),
        };

        match self.encoding {
            X86OpEn::RM  => write!(f, "{}, {}", self.modrm_reg, modrm_rm_str),
            X86OpEn::MR  => write!(f, "{}, {}", modrm_rm_str, self.modrm_reg),
            X86OpEn::MI  => write!(f, "{}, {}", modrm_rm_str, self.immediate),
            X86OpEn::RMI => write!(f, "{}, {}, {}", self.modrm_reg, modrm_rm_str, self.immediate),
            X86OpEn::M   => write!(f, "{}", modrm_rm_str),
            X86OpEn::O   => write!(f, "{}", self.modrm_reg),
            X86OpEn::OI  => write!(f, "{}, {}", self.modrm_reg, self.immediate),
            X86OpEn::I   => write!(f, "{}", self.immediate),
            X86OpEn::D   => write!(f, "{}", self.immediate),
            X86OpEn::ZO  => write!(f, ""),
        }
    }
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
        IrValue::U32(v) => X86Value::Int32(v as i32),
        IrValue::U64(v) => X86Value::Int64(v as i64),
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
        curr_call_frame: None,
        curr_function: IrIdent { symbol: intern_string("main"), index: 0 },
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
            let insn = &mut x86.instructions[*jump_index];
            let insn_byte_pos = insn.cached_pos as isize;
            let insn_size = insn.cached_size as isize;

            let rel32: i32 = (target_byte_pos - insn_byte_pos - insn_size).try_into().unwrap();
            if let X86Value::Int32(_) = insn.immediate {
                insn.immediate = X86Value::Int32(rel32);
                let rel32_index = insn.cached_pos + insn.cached_size - 4;
                x86.machine_code[rel32_index    ] = ( rel32        & 0xFFi32) as u8;
                x86.machine_code[rel32_index + 1] = ((rel32 >> 8)  & 0xFFi32) as u8;
                x86.machine_code[rel32_index + 2] = ((rel32 >> 16) & 0xFFi32) as u8;
                x86.machine_code[rel32_index + 3] = ((rel32 >> 24) & 0xFFi32) as u8;
            } else {
                let rel8_opt: Result<i8, _> = rel32.try_into();
                match rel8_opt {
                    Ok(rel8) => {
                        insn.immediate = X86Value::Int8(rel8);
                        let rel8_index = insn.cached_pos + insn.cached_size - 1;
                        let rel8 = if rel8 < 0 {
                            (256i32 + rel8 as i32) as u8 // signed two's complement in u8
                        } else {
                            rel8 as u8
                        };
                        x86.machine_code[rel8_index] = rel8;
                    },

                    Err(_) => panic!("32-bit jumps is not implemented!"),
                };
            }
        }
    }
}

fn compile_x86_stack_operand(insn: &mut X86Instruction, mut offset: isize) {
    offset = -offset; // NOTE(alexander): stack grows downwards since we start at rbp this has to be inverted.

    if offset < -128 && offset > 127 {
        insn.modrm_addr_mode = X86AddrMode::IndirectDisp32;
        insn.displacement = X86Value::Int32(offset as i32);
    } else {
        insn.modrm_addr_mode = X86AddrMode::IndirectDisp8;
        insn.displacement = X86Value::Int8(offset as i8);
    }
    insn.modrm_rm = X86Reg::RBP;
}

fn compile_x86_ir_operand(x86: &mut X86Assembler, insn: &mut X86Instruction, op1: IrOperand, op2: IrOperand) {
    match op1.ty {
        IrType::I8 => insn.byte_operands = true,
        IrType::PtrI8(_) |
        IrType::PtrI32(_) => {
            insn.rex = true;
            insn.rex_w = true;
        }
        _ => {}
    }

    match op1.kind {
        IrOperand::Stack(offset) => compile_x86_stack_operand(insn, offset),
        IrOperand::Register(register) => {
            insn.modrm_addr_mode = X86AddrMode::Direct;
            insn.modrm_reg = get_scratch_register(x86, register);
        },

        IrOperand::Value(_) => panic!("cannot encode constant as first operand"),

        _ => return,
    }

    match op2.kind {
        IrOperand::Stack(offset) => {
            insn.encoding = X86OpEn::RM;
            match op1.kind {
                IrOperand::Stack(_) => panic!("cannot encode memory access for both operands!"),
                IrOperand::Register(_) => {},
                _ => panic!("unexpected first operand"),
            };

            compile_x86_stack_operand(insn, offset);
        }
        IrOperand::Register(reg) => {
            match op1.kind {
                IrOperand::Stack(_) => {
                    insn.modrm_reg = get_scratch_register(x86, reg);
                    insn.encoding = X86OpEn::MR;
                }

                IrOperand::Register(_) => {
                    insn.modrm_rm = get_scratch_register(x86, reg);
                    insn.encoding = X86OpEn::RM;
                }

                _ => panic!("unexpected first operand"),
            };
        }

        IrOperand::Value(val) => {
            match op1.kind {
                IrOperand::Register(_) => {
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
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::CopyFromRef => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::LEA; // NOTE(alexander): only supports RM encoding!
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::CopyFromDeref => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::MOV;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            insn.rex = true;
            insn.rex_w = true;

            match insn.encoding { // TODO(alexander): better selection of scratch registers!
                X86OpEn::RM => {
                    if let X86AddrMode::Direct = insn.modrm_addr_mode {
                    } else {
                        let mut copy_insn = insn.clone();
                        copy_insn.modrm_reg = X86Reg::RAX;
                        x86.instructions.push(copy_insn);
                        insn.modrm_rm = X86Reg::RAX;
                    }
                }

                X86OpEn::MR => {
                    let mut copy_insn = insn.clone();
                    copy_insn.modrm_rm = X86Reg::RAX;
                    copy_insn.modrm_addr_mode = X86AddrMode::Direct;
                    x86.instructions.push(copy_insn);
                    insn.modrm_rm = X86Reg::RAX;
                }

                X86OpEn::MI => {
                    let mut copy_insn = insn.clone();
                    copy_insn.modrm_rm = X86Reg::RAX;
                    copy_insn.modrm_addr_mode = X86AddrMode::Direct;
                    x86.instructions.push(copy_insn);
                    insn.immediate = X86Value::None;
                    insn.modrm_rm = X86Reg::RAX;
                }

                _ => panic!("unexpected encoding"),
            }

            insn.modrm_addr_mode = X86AddrMode::Indirect;
            insn.encoding = X86OpEn::RM;
            x86.instructions.push(insn);
        }

        IrOpCode::CopyToDeref => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::MOV;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);

            match insn.encoding { // TODO(alexander): better selection of scratch registers!
                X86OpEn::RM => {
                    let mut copy_insn = insn.clone();
                    copy_insn.modrm_reg = X86Reg::RAX;
                    x86.instructions.push(copy_insn);
                    insn.modrm_rm = X86Reg::RAX;
                    insn.encoding = X86OpEn::MR;
                }

                X86OpEn::MR => {
                    let mut copy_insn = insn.clone();
                    copy_insn.modrm_rm = X86Reg::RAX;
                    copy_insn.modrm_addr_mode = X86AddrMode::Direct;
                    x86.instructions.push(copy_insn);
                    insn.modrm_rm = X86Reg::RAX;
                }

                X86OpEn::MI => {
                    // Copy address to register if indirect
                    let mut copy_insn = insn.clone();
                    copy_insn.modrm_reg = X86Reg::RAX;
                    copy_insn.encoding = X86OpEn::RM;
                    copy_insn.immediate = X86Value::None;
                    x86.instructions.push(copy_insn);
                    insn.modrm_rm = X86Reg::RAX;
                }

                _ => panic!("unexpected encoding"),
            }

            match ir_insn.op2.ty {
                IrType::I8 |
                IrType::I32 => {
                    insn.rex   = false; // NOTE(alexander): do not copy immediate as 64-bit value
                    insn.rex_w = false; // can causes the stack to become corrupted.
                }
                IrType::PtrI8(_) |
                IrType::PtrI32(_) => {
                    insn.rex   = true;
                    insn.rex_w = true;
                },
                _ => panic!("expected type"),
            }
            insn.modrm_addr_mode = X86AddrMode::Indirect;
            x86.instructions.push(insn);
        }

        IrOpCode::Clear => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::XOR;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op1);
            x86.instructions.push(insn);
        }

        IrOpCode::Add => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::ADD;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Sub => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::SUB;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Mul => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::IMUL; // NOTE(alexander): we only support signed numbers atm.
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            match insn.encoding {
                X86OpEn::MR => panic!("not possible"), // TODO(alexander): mov in to register first
                X86OpEn::MI => insn.encoding = X86OpEn::RMI,
                _ => { },
            }

            x86.instructions.push(insn);
        }

        IrOpCode::Div |
        IrOpCode::Mod => {
            // TODO(alexander): save rdx & rax

            let mut insn = create_x86_instruction();
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            let mut target_insn = insn.clone();

            // M has to be a register, specifically RAX
            if let X86AddrMode::Direct = insn.modrm_addr_mode {
            } else {
                // NOTE(alexander): this is unlikely to happen based on the ir
                let mut copy_insn = insn.clone();
                copy_insn.opcode = X86OpCode::MOV;
                copy_insn.encoding = X86OpEn::RM;
                copy_insn.modrm_reg = X86Reg::RAX;
                copy_insn.immediate = X86Value::None;
                x86.instructions.push(copy_insn);
                insn.modrm_addr_mode = X86AddrMode::Direct;
            }

            match insn.encoding {
                X86OpEn::MR => {
                    // Swap the encoding, denominator should be modrm_rm
                    insn.encoding = X86OpEn::RM;
                    let temp = insn.modrm_rm;
                    insn.modrm_rm = insn.modrm_reg;
                    insn.modrm_reg = temp;
                }

                X86OpEn::MI => {
                    // Copy immediate into scratch register
                    let mut copy_insn = insn.clone();
                    copy_insn.opcode = X86OpCode::MOV;
                    copy_insn.modrm_rm = X86Reg::RCX;
                    x86.instructions.push(copy_insn);

                    insn.immediate = X86Value::None; // No need for the immedate anymore
                    insn.modrm_reg = insn.modrm_rm;
                    insn.modrm_rm = X86Reg::RCX;
                }
                _ => { },
            }

            if let X86Reg::RAX = insn.modrm_reg {
            } else {
                let mut copy_insn = insn.clone();
                copy_insn.opcode = X86OpCode::MOV;
                copy_insn.encoding = X86OpEn::RM;
                copy_insn.modrm_rm = insn.modrm_reg;
                copy_insn.modrm_reg = X86Reg::RAX;
                x86.instructions.push(copy_insn);
            }

            let mut ext_insn = insn.clone();
            ext_insn.opcode = X86OpCode::CDQ; // NOTE(alexander): sign-extend RAX into RDX.
            ext_insn.encoding = X86OpEn::ZO;
            ext_insn.modrm_addr_mode = X86AddrMode::None;
            x86.instructions.push(ext_insn);

            insn.opcode = X86OpCode::IDIV; // NOTE(alexander): we only support signed numbers atm.
            insn.encoding = X86OpEn::M; // NOTE(alexander): IDIV only accepts RDX:RAX /= modrm_rm, why intel!?!?
            x86.instructions.push(insn.clone());

            // Now save the result back in the correct register
            let target = match ir_insn.opcode {
                IrOpCode::Div => X86Reg::RAX,
                IrOpCode::Mod => X86Reg::RDX,
                _ => unreachable!(),
            };
            target_insn.opcode = X86OpCode::MOV;
            match target_insn.encoding {
                X86OpEn::MR => {
                    if let X86Reg::RAX = target_insn.modrm_rm {
                        if let X86AddrMode::Direct = target_insn.modrm_addr_mode {
                            return;
                        }
                    }
                    target_insn.modrm_reg = target;
                }

                X86OpEn::RM => {
                    if let X86Reg::RAX = target_insn.modrm_reg {
                        return;
                    }
                    target_insn.modrm_addr_mode = X86AddrMode::Direct;
                    target_insn.modrm_rm = target;
                }

                X86OpEn::MI => {
                    if let X86Reg::RAX = target_insn.modrm_rm {
                        if let X86AddrMode::Direct = target_insn.modrm_addr_mode {
                            return;
                        }
                    }
                    target_insn.immediate = X86Value::None;
                    target_insn.encoding = X86OpEn::MR;
                    target_insn.modrm_reg = target;
                }

                _ => return,
            }
            x86.instructions.push(target_insn);
        }

        IrOpCode::Pow => {
            unimplemented!();
        },

        IrOpCode::And => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::AND;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Or => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::OR;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Xor => {
            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::XOR;
            compile_x86_ir_operand(x86, &mut insn, ir_insn.op1, ir_insn.op2);
            x86.instructions.push(insn);
        }

        IrOpCode::Lt |
        IrOpCode::Gt |
        IrOpCode::Le |
        IrOpCode::Ge |
        IrOpCode::Eq |
        IrOpCode::Ne => {
            let mut cmp_insn = create_x86_instruction();
            cmp_insn.opcode = X86OpCode::CMP;
            compile_x86_ir_operand(x86, &mut cmp_insn, ir_insn.op2, ir_insn.op3);
            x86.instructions.push(cmp_insn);

            let mut insn = create_x86_instruction();
            match ir_insn.opcode {
                IrOpCode::Lt => insn.opcode = X86OpCode::SETL,
                IrOpCode::Gt => insn.opcode = X86OpCode::SETG,
                IrOpCode::Le => insn.opcode = X86OpCode::SETLE,
                IrOpCode::Ge => insn.opcode = X86OpCode::SETGE,
                IrOpCode::Eq => insn.opcode = X86OpCode::SETE,
                IrOpCode::Ne => insn.opcode = X86OpCode::SETNE,
                _ => panic!(),
            }

            match ir_insn.op1.kind {
                IrOperand::Register(register) => {
                    insn.modrm_addr_mode = X86AddrMode::Direct;
                    insn.modrm_rm = get_scratch_register(x86, register);
                },
                IrOperand::Stack(offset) => compile_x86_stack_operand(&mut insn, offset),
                _ => panic!("unexpected operand"),
            }
            insn.byte_operands = true;
            insn.encoding = X86OpEn::M;
            x86.instructions.push(insn);

            // let mut insn = create_x86_instruction();
            // insn.opcode = X86OpCode::MOV;
            // insn.encoding = X86OpEn::MI;
            // insn.byte_operands = true;
            // match ir_insn.op1.kind {
            //     IrOperand::Register(register) => {
            //         insn.modrm_addr_mode = X86AddrMode::Direct;
            //         insn.modrm_rm = get_scratch_register(x86, register);
            //     },
            //     _ => panic!("unexpected operand"),
            // }

            // insn.immediate = X86Value::Int8(0);
            // x86.instructions.push(insn.clone());

            // let mut jump_insn = create_x86_instruction();
            // jump_insn.opcode = X86OpCode::JMP;
            // jump_insn.encoding = X86OpEn::D;
            // jump_insn.immediate = X86Value::Int8(3); // opcode + modrm + i8 immediate
            // x86.instructions.push(jump_insn);

            // insn.immediate = X86Value::Int8(1);
            // x86.instructions.push(insn);
        }

        IrOpCode::IfLt |
        IrOpCode::IfGt |
        IrOpCode::IfLe |
        IrOpCode::IfGe |
        IrOpCode::IfEq |
        IrOpCode::IfNe => {
            let mut cmp_insn = create_x86_instruction();
            cmp_insn.opcode = X86OpCode::CMP;
            compile_x86_ir_operand(x86, &mut cmp_insn, ir_insn.op1, ir_insn.op2);
            if let X86OpEn::ZO = cmp_insn.encoding {
                panic!("failed to encode instruction:\n{:#?}", ir_insn);
            }
            x86.instructions.push(cmp_insn);

            if let IrOperand::Ident(label) = ir_insn.op3.kind {
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
        }

        IrOpCode::Jump => {
            if let IrOperand::Ident(label) = ir_insn.op1.kind {
                let insn_index = x86.instructions.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.jumps.push(insn_index);
            } else {
                panic!("missing label on first operand in jump instruction");
            }

            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::JMP;

            insn.immediate = X86Value::Int8(0); // jump distance gets calculated later
            insn.encoding = X86OpEn::D;
            x86.instructions.push(insn);
        }

        IrOpCode::Label => {
            if let IrOperand::Ident(label) = ir_insn.op1.kind {
                let insn_index = x86.instructions.len();
                let jt = get_mut_x86_jump_target(x86, label);
                jt.pos = insn_index;

                if label.function {
                    x86.curr_function = label;
                }
            }
        }

        IrOpCode::Param => {
            let addr_size = if x86.x64_mode {
                8isize
            } else {
                4isize
            };
            let type_size = size_of_ir_type(ir_insn.op1.ty, addr_size);
            let assign_op = IrOperand::None; // FIXME(alexander)
            // let assign_op = IrOperand {
                // ty: ir_insn.op1.ty,
                // kind: IrOperand::Stack(type_size), // fill out displacement later
            // };

            let mut insn = create_x86_instruction();
            insn.opcode = X86OpCode::MOV;
            compile_x86_ir_operand(x86, &mut insn, assign_op, ir_insn.op1);
            match &mut x86.curr_call_frame {
                Some(frame) => {
                    frame.size += type_size;
                    frame.arguments.push(insn); // push to x86.instructions later at call instruction.
                }
                None => {
                    let new_frame = X86CallFrame {
                        size: type_size,
                        arguments: vec![insn],
                    };
                    x86.curr_call_frame = Some(new_frame);
                }
            }
        }

        IrOpCode::Call => {
            let is_intrinsic_call = match ir_insn.op1.kind {
                IrOperand::Value(_) => true,
                _ => false,
            };

            // Begin function call (cdecl or C calling convention)
            let mut stack_space_used = 0isize;
            match &x86.curr_call_frame {
                Some(frame) => {
                    if is_intrinsic_call {
                        // mov rbp rsp
                        // NOTE(alexander): this seams to be a requirement however not sure if it's documented.
                        let mut insn = create_x86_instruction();
                        insn.opcode = X86OpCode::MOV;
                        insn.modrm_addr_mode = X86AddrMode::Direct;
                        insn.modrm_reg = X86Reg::RBP;
                        insn.modrm_rm  = X86Reg::RSP;
                        insn.encoding  = X86OpEn::RM;
                        insn.rex = true;
                        insn.rex_w = true;
                        x86.instructions.push(insn);

                        // Push arguments according to the C calling convention
                        let mut curr_disp = 0;
                        let registers = [X86Reg::RCX, X86Reg::RDX, X86Reg::R8, X86Reg::R9];
                        for (i, insn) in frame.arguments.iter().enumerate() {
                            let mut insn = insn.clone();
                            insn.opcode = X86OpCode::MOV;

                            if i < registers.len() {
                                insn.modrm_rm = registers[i];
                                insn.modrm_addr_mode = X86AddrMode::Direct;
                                x86.instructions.push(insn);
                            } else {
                                // Push onto stack
                                let disp = match insn.displacement {
                                    X86Value::Int8(v) => v as isize,
                                    X86Value::Int32(v) => v as isize,
                                    _ => 0isize,
                                };
                                let mut insn = insn.clone();
                                compile_x86_stack_operand(&mut insn, curr_disp);
                                insn.modrm_rm = X86Reg::RSP;
                                x86.instructions.push(insn);

                                curr_disp += disp;
                            }
                        }
                        stack_space_used = curr_disp;

                    } else {
                        let mut insn = create_x86_instruction();
                        insn.opcode = X86OpCode::SUB;
                        insn.encoding = X86OpEn::MI;
                        insn.modrm_addr_mode = X86AddrMode::Direct;
                        insn.modrm_rm = X86Reg::RSP;
                        insn.rex = true;
                        insn.rex_w = true;
                        insn.immediate = X86Value::Int32(frame.size as i32);
                        x86.instructions.push(insn);

                        // Push arguments according to the cdecl calling convention
                        let mut curr_disp = 0;
                        for insn in frame.arguments.iter().rev() {
                            let disp = match insn.displacement {
                                X86Value::Int8(v) => v as isize,
                                X86Value::Int32(v) => v as isize,
                                _ => 0isize,
                            };
                            let mut insn = insn.clone();
                            compile_x86_stack_operand(&mut insn, curr_disp);
                            insn.modrm_rm = X86Reg::RSP;
                            x86.instructions.push(insn);
                            curr_disp += disp;
                        }
                        stack_space_used = curr_disp;
                    }
                }
                _  => {}
            }

            // Call the function
            match ir_insn.op1.kind {
                IrOperand::Ident(label) => {
                    let mut insn = create_x86_instruction();
                    insn.opcode = X86OpCode::CALL;
                    insn.encoding = X86OpEn::D;
                    insn.immediate = X86Value::Int32(0);

                    let insn_index = x86.instructions.len();
                    let jt = get_mut_x86_jump_target(x86, label);
                    jt.jumps.push(insn_index);

                    x86.instructions.push(insn);
                }

                IrOperand::Value(val) => {
                    let mut insn = create_x86_instruction();
                    insn.opcode = X86OpCode::MOV;
                    insn.modrm_rm = X86Reg::RAX;
                    insn.modrm_reg = X86Reg::RAX;
                    insn.modrm_addr_mode = X86AddrMode::Direct;
                    insn.immediate = ir_value_to_x86_value(val);

                    if let IrValue::U64(_) = val {
                        insn.encoding = X86OpEn::OI;
                        insn.rex = true;
                        insn.rex_w = true;
                    }
                    x86.instructions.push(insn);

                    let mut insn = create_x86_instruction();
                    insn.opcode = X86OpCode::CALL;
                    insn.encoding = X86OpEn::M;
                    insn.modrm_rm = X86Reg::RAX;
                    insn.modrm_addr_mode = X86AddrMode::Direct;
                    x86.instructions.push(insn);
                }
                
                _ => panic!("expected label or register in call instruction"),
            }

            // Cleanup call frame
            if stack_space_used > 0 {
                let mut insn = create_x86_instruction();
                insn.opcode = X86OpCode::ADD;
                insn.encoding = X86OpEn::MI;
                insn.modrm_addr_mode = X86AddrMode::Direct;
                insn.modrm_rm = X86Reg::RSP;
                insn.rex = true;
                insn.rex_w = true;
                insn.immediate = X86Value::Int32(stack_space_used as i32);
                x86.instructions.push(insn);
            }
            x86.curr_call_frame = None;
        }

        IrOpCode::Return => {
            let assign_rax: bool;
            match (ir_insn.op1.kind, ir_insn.op1.ty) {
                (IrOperand::None, _) => assign_rax = false,
                (IrOperand::Register(0), IrType::I32) => assign_rax = false,
                _ => assign_rax = true,
            };

            if assign_rax {
                let op1 = IrOperand::None; // TODO(alexander): move to RAX
                let mut insn = create_x86_instruction();
                compile_x86_ir_operand(x86, &mut insn, op1, ir_insn.op1);
                
                if let IrType::I8 = ir_insn.op1.ty {
                    if let IrOperand::Value(_) = ir_insn.op1.kind {
                        insn.opcode = X86OpCode::MOV;
                    } else {
                        insn.opcode = X86OpCode::MOVSX;
                    }
                } else {
                    insn.opcode = X86OpCode::MOV;
                }
                x86.instructions.push(insn);
            }

            // TODO(alexander): this instruction is not needed if return is at the bottom of a function!
            let mut jump_insn = create_x86_instruction();
            jump_insn.opcode = X86OpCode::JMP;
            jump_insn.encoding = X86OpEn::D;
            jump_insn.immediate = X86Value::Int8(0);
            let label = create_ir_ident(x86.curr_function.symbol, 1);
            let insn_index = x86.instructions.len();
            let jt = get_mut_x86_jump_target(x86, label);
            jt.jumps.push(insn_index);
            x86.instructions.push(jump_insn);
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
            insn.opcode = X86OpCode::MOV;
            insn.modrm_addr_mode = X86AddrMode::Direct;
            insn.modrm_reg = X86Reg::RBP;
            insn.modrm_rm  = X86Reg::RSP;
            insn.encoding  = X86OpEn::RM;
            insn.rex = true;
            insn.rex_w = true;
            x86.instructions.push(insn);

            // sub rsp x
            match x86.functions.get(&x86.curr_function) {
                Some(block) => {
                    let mut insn = create_x86_instruction();
                    insn.opcode = X86OpCode::SUB;
                    insn.encoding = X86OpEn::MI;
                    insn.modrm_addr_mode = X86AddrMode::Direct;
                    insn.modrm_rm = X86Reg::RSP;
                    insn.rex = true;
                    insn.rex_w = true;
                    insn.immediate = X86Value::Int32(block.required_stack_size.try_into().unwrap());
                    x86.instructions.push(insn);
                }
                _ => {}
            }
        }

        IrOpCode::Epilogue => {
            // add rsp x
            match x86.functions.get(&x86.curr_function) {
                Some(block) => {
                    let mut insn = create_x86_instruction();
                    insn.opcode = X86OpCode::ADD;
                    insn.encoding = X86OpEn::MI;
                    insn.modrm_addr_mode = X86AddrMode::Direct;
                    insn.modrm_rm = X86Reg::RSP;
                    insn.rex = true;
                    insn.rex_w = true;
                    insn.immediate = X86Value::Int32(block.required_stack_size.try_into().unwrap());
                    x86.instructions.push(insn);
                }
                _ => {}
            }
            
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
    }
}

fn compile_x86_machine_code(x86: &mut X86Assembler) {
    for insn in &mut x86.instructions {
        insn.cached_pos = x86.machine_code.len();

        let mut rex = 0;
        if x86.x64_mode {
            if insn.rex_w {
                rex |= 1 << 3;
            }

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

            if rex > 0 {
                rex |= 0b01000000;
            }
        }

        if rex != 0 {
            x86.machine_code.push(rex);
        }

        if !x86.x64_mode {
            // prefix, 32-bit insn size, 32-bit address size.
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

                (X86OpEn::OI, false) => {
                    x86.machine_code.push(0xb8 + register_encoding(insn.modrm_reg));
                    insn.modrm_addr_mode = X86AddrMode::None;
                }

                (X86OpEn::MI, true)  => {
                    x86.machine_code.push(0xc6);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                (X86OpEn::MI, false) => {
                    // TODO(alexander): use X86OpEn::OI for U64 bit values.
                    x86.machine_code.push(0xc7);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::MOVSX => match insn.encoding {
                X86OpEn::RM => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0xbe);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::LEA => match insn.encoding {
                X86OpEn::RM => x86.machine_code.push(0x8d),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::ADD => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x05),
                X86OpEn::MI => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                X86OpEn::MR => x86.machine_code.push(0x01),
                X86OpEn::RM => x86.machine_code.push(0x03),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SUB => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x2d),
                X86OpEn::MI => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RBP; // reg = 5
                }
                X86OpEn::MR => x86.machine_code.push(0x29),
                X86OpEn::RM => x86.machine_code.push(0x2b),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
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
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::IDIV => match insn.encoding {
                X86OpEn::M => {
                    x86.machine_code.push(0xf7);
                    insn.modrm_reg = X86Reg::RDI; // reg = 7
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::AND => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x24),
                X86OpEn::MI => {
                    x86.machine_code.push(0x80);
                    insn.modrm_reg = X86Reg::RSP; // reg = 4
                }
                X86OpEn::MR => x86.machine_code.push(0x20),
                X86OpEn::RM => x86.machine_code.push(0x22),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::OR => match insn.encoding {
                X86OpEn::I  => x86.machine_code.push(0x0c),
                X86OpEn::MI => {
                    x86.machine_code.push(0x80);
                    insn.modrm_reg = X86Reg::RCX; // reg = 1
                }
                X86OpEn::MR => x86.machine_code.push(0x08),
                X86OpEn::RM => x86.machine_code.push(0x0a),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::XOR => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::I, true)  => x86.machine_code.push(0x34),
                (X86OpEn::I, false) => x86.machine_code.push(0x35),

                (X86OpEn::MI, true) => {
                    x86.machine_code.push(0x80);
                    insn.modrm_reg = X86Reg::RSI; // reg = 6
                }
                (X86OpEn::MI, false) => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RSI; // reg = 6
                }

                (X86OpEn::MR, true)  => x86.machine_code.push(0x30),
                (X86OpEn::MR, false) => x86.machine_code.push(0x31),

                (X86OpEn::RM, true)  => x86.machine_code.push(0x32),
                (X86OpEn::RM, false) => x86.machine_code.push(0x33),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::CDQ => match insn.encoding {
                X86OpEn::ZO => x86.machine_code.push(0x99),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::CMP => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::I, true)   => x86.machine_code.push(0x3c),
                (X86OpEn::I, false)  => x86.machine_code.push(0x3d),

                (X86OpEn::MI, true)  => {
                    x86.machine_code.push(0x80);
                    insn.modrm_reg = X86Reg::RDI; // reg = 7
                }
                (X86OpEn::MI, false) => {
                    x86.machine_code.push(0x81);
                    insn.modrm_reg = X86Reg::RDI; // reg = 7
                }

                (X86OpEn::MR, true)  => x86.machine_code.push(0x38),
                (X86OpEn::MR, false) => x86.machine_code.push(0x39),

                (X86OpEn::RM, true)  => x86.machine_code.push(0x3a),
                (X86OpEn::RM, false) => x86.machine_code.push(0x3b),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::TEST => match (insn.encoding, insn.byte_operands) { // NOTE(alexander): only needed for bytes
                (X86OpEn::I, true)  => x86.machine_code.push(0xa8),
                (X86OpEn::MI, true) => {
                    x86.machine_code.push(0xf6);
                    insn.modrm_reg = X86Reg::RAX; // reg = 0
                }
                (X86OpEn::MR, true) => x86.machine_code.push(0x84),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SETL => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::M, true) => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0x9c);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SETG => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::M, true) => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0x9f);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SETLE => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::M, true) => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0x9e);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SETGE => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::M, true) => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0x9d);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SETE => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::M, true) => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0x94);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::SETNE => match (insn.encoding, insn.byte_operands) {
                (X86OpEn::M, true) => {
                    x86.machine_code.push(0x0f);
                    x86.machine_code.push(0x95);
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JL => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7c),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8c);
                    }
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JLE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7e),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8e);
                    }
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JG => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7f),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8f);
                    }
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JGE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x7d),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x8d);
                    }
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x74),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x84);
                    }
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JNE => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0x75),
                    X86Value::Int32(_) => {
                        x86.machine_code.push(0x0f);
                        x86.machine_code.push(0x85);
                    }
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::JMP => match insn.encoding {
                X86OpEn::D => match insn.immediate {
                    X86Value::Int8(_) => x86.machine_code.push(0xeb),
                    X86Value::Int32(_) => x86.machine_code.push(0xe9),
                    _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
                }
                X86OpEn::M => x86.machine_code.push(0xff),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::PUSH => match insn.encoding {
                X86OpEn::O => x86.machine_code.push(0x50 + register_encoding(insn.modrm_reg)),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::POP => match insn.encoding {
                X86OpEn::O => x86.machine_code.push(0x58 + register_encoding(insn.modrm_reg)),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::CALL => match insn.encoding {
                X86OpEn::D => x86.machine_code.push(0xe8),
                X86OpEn::M => {
                    x86.machine_code.push(0xff);
                    insn.modrm_reg = X86Reg::RDX;
                }
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
            }

            X86OpCode::RET => match insn.encoding {
                X86OpEn::ZO => x86.machine_code.push(0xc3),
                _ => unimplemented!("X86OpEn::{:#?}", insn.encoding),
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

        // NOTE(alexander): special case set SIB to RSP
        if let X86Reg::RSP = insn.modrm_rm {
            match insn.modrm_addr_mode {
                X86AddrMode::IndirectDisp8 |
                X86AddrMode::IndirectDisp32 => {
                    x86.machine_code.push(0b00100100);
                }
                _ => {}
            }
        }

        match insn.modrm_addr_mode {
            X86AddrMode::IndirectDisp8 |
            X86AddrMode::IndirectDisp32 => {
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
            },

            _ => {}
        }

        match insn.immediate {
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
            _ => {},
        }

        insn.cached_size = x86.machine_code.len() - insn.cached_pos;
    }
}
