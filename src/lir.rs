use std::collections::HashMap;
use std::fmt;
use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum LirOpCode {
    Nop,
    Mov,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    Deref,
    IfLt,
    IfGt,
    IfLe,
    IfGe,
    IfEq,
    IfNe,
    Param,
    Call,
    Jump,
    Label,
    Prologue, // marks beginning of function
    Epilogue, // marks end of function
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LirOperand {
    Address(usize), // memory address
    StackOffset(usize), // same as address except relative to its residing stack frame, e.g. $sp + 0x4
    DerefStackOffset(usize), // dereferencing the stack offset e.g. [$sp + 0x4]
    Label(Symbol),
    ConstantInt(i32),
    ConstantBool(bool),
    None,
}

/**
 * Three address code instruction, is defined an op code and up to three operands.
 * Span is also used for debugging to retrieve the source location of a given instruction.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LirInstruction {
    pub opcode: LirOpCode,
    pub op1: LirOperand,
    pub op2: LirOperand,
    pub op3: LirOperand,
    pub span: Span,
}

/**
 * Represents a block containing multiple instructions, this
 * is used to manage stack offsets.
 */
struct LirBasicBlock {
    pub stack_offsets: HashMap<Symbol, usize>,
    pub current_stack_offset: usize,
}

/**
 * Used for building low-level intermediate representation.
 */
pub struct LirContext<'a> {
    pub file: Option<&'a File>,
    pub instructions: Vec<LirInstruction>,
    basic_blocks: Vec<LirBasicBlock>,
}

impl Default for LirInstruction {
    fn default() -> Self {
        LirInstruction {
            opcode: LirOpCode::Nop,
            op1: LirOperand::None,
            op2: LirOperand::None,
            op3: LirOperand::None,
            span: Span::new(),
        }
    }
}

impl fmt::Display for LirContext<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for insn in &self.instructions {
            write!(f, "    {}\n", insn)?;
        }
        Ok(())
    }
}

impl fmt::Display for LirInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:<10} {}\t {}\t {}", format!("{}", self.opcode), self.op1, self.op2, self.op3)
    }
}

impl fmt::Display for LirOpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match self {
            LirOpCode::Nop       => "nop",
            LirOpCode::Mov      => "mov",
            LirOpCode::Add       => "add",
            LirOpCode::Sub       => "sub",
            LirOpCode::Mul       => "mul",
            LirOpCode::Div       => "div",
            LirOpCode::Pow       => "pow",
            LirOpCode::Mod       => "mod",
            LirOpCode::And       => "and",
            LirOpCode::Or        => "or",
            LirOpCode::Eq        => "eq",
            LirOpCode::Ne        => "ne",
            LirOpCode::Lt        => "lt",
            LirOpCode::Le        => "le",
            LirOpCode::Gt        => "gt",
            LirOpCode::Ge        => "ge",
            LirOpCode::Not       => "not",
            LirOpCode::Deref     => "deref",
            LirOpCode::IfLt      => "iflt",
            LirOpCode::IfGt      => "ifgt",
            LirOpCode::IfLe      => "ifle",
            LirOpCode::IfGe      => "ifge",
            LirOpCode::IfEq      => "ifeq",
            LirOpCode::IfNe      => "ifne",
            LirOpCode::Param     => "param",
            LirOpCode::Call      => "call",
            LirOpCode::Jump      => "jump",
            LirOpCode::Label     => "label",
            LirOpCode::Prologue  => "prologue",
            LirOpCode::Epilogue  => "epilogue\n",
        };
        write!(f, "{}", result)
    }
}

impl fmt::Display for LirOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LirOperand::Address(addr) => write!(f, "{:#X}", addr),
            LirOperand::StackOffset(offset) => write!(f, "$sp + {:#X}", offset),
            LirOperand::DerefStackOffset(offset) => write!(f, "[$sp + {:#X}]", offset),
            LirOperand::Label(label) => write!(f, "{}", resolve_symbol(*label)),
            LirOperand::ConstantInt(val) => write!(f, "{}", val),
            LirOperand::ConstantBool(val) => write!(f, "{}", val),
            LirOperand::None => write!(f, ""),
        }
    }
}

pub fn create_lir_context<'a>() -> LirContext<'a> {
    LirContext {
        file: None,
        instructions: Vec::new(),
        basic_blocks: Vec::new(),
    }
}

fn create_lir_basic_block() -> LirBasicBlock {
    LirBasicBlock {
        stack_offsets: HashMap::new(),
        current_stack_offset: 0,
    }
}

fn store_local_variable<'a>(lc: &mut LirContext<'a>, symbol: Option<Symbol>, reserve_space: bool) -> LirOperand {
    if lc.basic_blocks.len() == 0 {
        return LirOperand::None // NOTE(alexander): this should never occur!
    }

    let len = lc.basic_blocks.len();
    let frame = &mut lc.basic_blocks[len - 1];
    let offset = frame.current_stack_offset;
    if let Some(sym) = symbol {
        frame.stack_offsets.insert(sym, offset);
    }
    if reserve_space {
        frame.current_stack_offset += 4; // NOTE(alexander): i32/ bool are both 4-bits.
    }
    LirOperand::DerefStackOffset(offset)
}

fn load_local_variable<'a>(lc: &mut LirContext<'a>, symbol: Symbol) -> LirOperand {
    for frame in &lc.basic_blocks {
        if let Some(offset) = frame.stack_offsets.get(&symbol) {
            return LirOperand::DerefStackOffset(*offset);
        }
    }
    LirOperand::None
}

pub fn build_lir_from_ast<'a>(lc: &mut LirContext<'a>, file: &'a File) {
    lc.file = Some(file);

    for item in &file.items {
        build_lir_from_item(lc, &item);
    }
}

pub fn build_lir_from_item<'a>(lc: &mut LirContext<'a>, item: &Item) {
    match item {
        Item::Fn(func) => {
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Label,
                op1: LirOperand::Label(func.ident.sym),
                span: func.span,
                ..Default::default()
            });
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Prologue,
                ..Default::default()
            });

            build_lir_from_block(lc, &func.block);

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Epilogue,
                ..Default::default()
            });
        }
        _ => { }
    }
}

pub fn build_lir_from_block<'a>(lc: &mut LirContext<'a>, block: &Block) -> LirOperand {
    let bblen = lc.basic_blocks.len();
    let mut current_stack_offset = 0;
    if bblen > 0 {
        current_stack_offset = lc.basic_blocks[bblen - 1].current_stack_offset;
    }

    let mut basic_block = create_lir_basic_block();
    basic_block.current_stack_offset = current_stack_offset;
    lc.basic_blocks.push(basic_block);

    let mut last_op = LirOperand::None;
    for stmt in &block.stmts {
        last_op = build_lir_from_stmt(lc, &stmt);
    }

    lc.basic_blocks.pop();

    if let Some(Stmt::Expr(_)) = block.stmts.last() {
        if let LirOperand::None = last_op {
            LirOperand::None
        } else {
            let op1 = store_local_variable(lc, Some(intern_string("result")), false); // TODO(alexander): handle duplicate names!!!
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Mov,
                op1,
                op2: last_op,
                span: block.span,
                ..Default::default()
            });
            op1
        }
    } else {
        LirOperand::None
    }
}

pub fn build_lir_from_stmt<'a>(lc: &mut LirContext<'a>, stmt: &Stmt) -> LirOperand {
    match stmt {
        Stmt::Local(local) => {
            let operand = match &*local.init {
                Some(expr) => build_lir_from_expr(lc, expr),
                None => {
                    LirOperand::None // temp, handle uninitialized case
                }
            };

            let op1 = store_local_variable(lc, Some(local.ident.sym), true);
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Mov,
                op1,
                op2: operand,
                span: local.span,
                ..Default::default()
            });

            LirOperand::None
        }

        Stmt::Item(_) => LirOperand::None,
        Stmt::Semi(expr) => { build_lir_from_expr(lc, expr) }
        Stmt::Expr(expr) => { build_lir_from_expr(lc, expr) }
    }
}

pub fn build_lir_from_expr<'a>(lc: &mut LirContext<'a>, expr: &Expr) -> LirOperand {
    match expr {
        Expr::Assign(assign) => {
            let op2 = build_lir_from_expr(lc, &assign.right);
            let op1 = build_lir_from_expr(lc, &assign.left);

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Mov, // store: op1 = op2
                op1,
                op2,
                span: assign.span,
                ..Default::default()
            });

            op1
        }

        Expr::Binary(binary) => {
            let op3 = build_lir_from_expr(lc, &binary.right);
            let op2 = build_lir_from_expr(lc, &binary.left);
            let op1 = store_local_variable(lc, None, true);
            let opcode = match binary.op {
                BinOp::Add => LirOpCode::Add,
                BinOp::Sub => LirOpCode::Sub,
                BinOp::Mul => LirOpCode::Mul,
                BinOp::Div => LirOpCode::Div,
                BinOp::Pow => LirOpCode::Pow,
                BinOp::Mod => LirOpCode::Mod,
                BinOp::And => LirOpCode::And,
                BinOp::Or  => LirOpCode::Or,
                BinOp::Eq  => LirOpCode::Eq,
                BinOp::Ne  => LirOpCode::Ne,
                BinOp::Lt  => LirOpCode::Lt,
                BinOp::Le  => LirOpCode::Le,
                BinOp::Gt  => LirOpCode::Gt,
                BinOp::Ge  => LirOpCode::Ge,
            };

            lc.instructions.push(LirInstruction {
                opcode,
                op1,
                op2,
                op3,
                span: binary.span,
            });

            op1
        }

        Expr::Block(block) => build_lir_from_block(lc, &block.block),

        Expr::Break(_break_expr) => {
            LirOperand::None
        }

        Expr::Call(call) => {
            let mut param_size = 0;
            for arg in &call.args {
                let op1 = build_lir_from_expr(lc, &arg);
                lc.instructions.push(LirInstruction {
                    opcode: LirOpCode::Param,
                    op1,
                    span: arg.get_span(),
                    ..Default::default()
                });
                
                param_size += 4; // NOTE(alexander): fixed-bitwidth i32/bool
            }

            let op1 = LirOperand::Label(call.ident.sym);
            let op2 = LirOperand::ConstantInt(param_size);
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Call,
                op1,
                op2,
                span: call.span,
                ..Default::default()
            });
            LirOperand::None
        }

        Expr::Continue(_continue_expr) => {
            LirOperand::None
        }

        Expr::Ident(ident) => load_local_variable(lc, ident.sym),

        Expr::If(if_expr) => {
            let exit_label = intern_string("if_exit"); // TODO(alexander): handle duplicate labels!
            let mut else_label: Option<Symbol> = None;

            let mut op1 = LirOperand::None;
            let mut op2 = LirOperand::None;
            let op3 = match if_expr.else_block {
                Some(_) => {
                    let sym = intern_string("if_else"); // TODO(alexander): handle duplicate labels!
                    else_label = Some(sym);
                    LirOperand::Label(sym)
                }

                None => LirOperand::Label(exit_label),
            };
            let mut opcode = match &*if_expr.cond {
                Expr::Binary(binary) => {
                    let opcode = match binary.op {
                        BinOp::Lt => LirOpCode::IfGe,
                        BinOp::Gt => LirOpCode::IfLe,
                        BinOp::Le => LirOpCode::IfGt,
                        BinOp::Ge => LirOpCode::IfLt,
                        BinOp::Eq => LirOpCode::IfNe,
                        BinOp::Ne => LirOpCode::IfEq,
                        _ => LirOpCode::Nop,
                    };

                    op1 = build_lir_from_expr(lc, &binary.left);
                    op2 = build_lir_from_expr(lc, &binary.right);
                    opcode
                }
                _ => LirOpCode::Nop,
            };

            if let LirOpCode::Nop = opcode {
                opcode = LirOpCode::IfEq;
                op1 = build_lir_from_expr(lc, &if_expr.cond);
                op2 = LirOperand::ConstantBool(true);
            };

            lc.instructions.push(LirInstruction {
                opcode,
                op1,
                op2,
                op3,
                span: if_expr.span,
            });

            build_lir_from_block(lc, &if_expr.then_block);

            if let Some(label) = else_label {
                lc.instructions.push(LirInstruction {
                    opcode: LirOpCode::Jump,
                    op1: LirOperand::Label(exit_label),
                    ..Default::default()
                });
  
                lc.instructions.push(LirInstruction {
                    opcode: LirOpCode::Label,
                    op1: LirOperand::Label(label),
                    ..Default::default()
                });
            }

            if let Some(block) = &if_expr.else_block {
                build_lir_from_block(lc, &block);
            }

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Label,
                op1: LirOperand::Label(exit_label),
                ..Default::default()
            });

            LirOperand::None
        }

        Expr::Lit(literal) => match literal.lit {
            Lit::Int(val)  => LirOperand::ConstantInt(val),
            Lit::Bool(val) => LirOperand::ConstantBool(val),
        },

        Expr::Paren(paren) => build_lir_from_expr(lc, &paren.expr),

        Expr::Reference(reference) => {
            let op1 = store_local_variable(lc, None, true);
            let mut op2 = build_lir_from_expr(lc, &reference.expr);
            if let LirOperand::DerefStackOffset(offset) = op2 {
                op2 = LirOperand::StackOffset(offset);
            } else {
                let op1 = store_local_variable(lc, None, true);
                lc.instructions.push(LirInstruction {
                    opcode: LirOpCode::Mov, // store: op1 = op2
                    op1,
                    op2,
                    span: reference.span,
                    ..Default::default()
                });

                op2 = op1;
                if let LirOperand::DerefStackOffset(offset) = op2 {
                    op2 = LirOperand::StackOffset(offset);
                }
            };

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Mov, // store: op1 = op2
                op1,
                op2,
                span: reference.span,
                ..Default::default()
            });
            
            op1
        }

        Expr::Return(_return_expr) => {
            LirOperand::None
        }

        Expr::Unary(unary) => {
            match unary.op {
                UnOp::Neg => {
                    let op3 = build_lir_from_expr(lc, &unary.expr);
                    let op1 = store_local_variable(lc, None, true);
                    lc.instructions.push(LirInstruction {
                        opcode: LirOpCode::Sub, // store: op1 = op2
                        op1,
                        op2: LirOperand::ConstantInt(0),
                        op3,
                        span: unary.span,
                    });

                    op1
                },

                UnOp::Not => {
                    let op2 = build_lir_from_expr(lc, &unary.expr);
                    let op1 = store_local_variable(lc, None, true);
                    lc.instructions.push(LirInstruction {
                        opcode: LirOpCode::Not, // store: op1 = op2
                        op1,
                        op2,
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                },

                UnOp::Deref => {
                    let op2 = build_lir_from_expr(lc, &unary.expr);
                    let op1 = store_local_variable(lc, None, true);
                    lc.instructions.push(LirInstruction {
                        opcode: LirOpCode::Deref, // store: op1 = op2
                        op1,
                        op2,
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                }
            }
        }

        Expr::While(_while_expr) => {
            LirOperand::None
        }
    }
}
