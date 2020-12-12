use std::collections::HashMap;
use std::fmt;
use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum LirOpCode {
    Nop,
    Copy, // op1 = op2
    CopyFromDeref, // op1 = *op2
    CopyFromRef, // op1 = &op2 (these are always mutable refs)
    CopyToDeref, // *op1 = op2
    Add, // op1 = op1 binop op2
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
    Not, // op1 = !op1
    IfLt, // jump op3 (if op1 binop op2 equals true)
    IfGt,
    IfLe,
    IfGe,
    IfEq,
    IfNe,
    Param,    // params (assigned left-to-right)
    Call,     // jump op1 with op2 = the number of params
    Return,   // return op1 (where op1 is optional, may be None)
    Label,    // label op1
    Jump,     // jump op1
    Prologue, // marks beginning of function, op1 holds the required stack space
    Epilogue, // marks end of function
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LirOperand {
    Stack(i64),
    Register(u64),
    Label(LirLabel),
    Constant(LirValue),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LirValue {
    I32(i32),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LirLabel {
    pub symbol: Symbol,
    pub index: u32, // used to distinguish labels with the same symbol.
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
struct LirBasicBlock { // TODO(alexander): store these in context, extend to mark where in insn vec it is stored.
    pub stack_offsets: HashMap<Symbol, i64>,
    pub next_stack_offset: i64,
    pub next_register: u64,
    pub largest_stack_offset: i64, // needed for pre allocating stack space in prologue
}

/**
 * Used for building low-level intermediate representation.
 */
pub struct LirContext<'a> {
    pub file: Option<&'a File>,
    pub instructions: Vec<LirInstruction>,
    pub jump_table: HashMap<LirLabel, usize>, // location of labels in instructions
    basic_blocks: Vec<LirBasicBlock>,
    unique_label_index: u32,
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
        write!(f, "{:<15} {}", format!("{}", self.opcode), self.op1)?;
        match self.op2 {
            LirOperand::None => { }
            _ => {
                write!(f, ", {}", self.op2)?;
                match self.op3 {
                    LirOperand::None => { }
                    _ => { write!(f, ", {}", self.op3)?; }
                }
            }
        }
        
        Ok(())
    }
}

impl fmt::Display for LirOpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match self {
            LirOpCode::Nop           => "nop",
            LirOpCode::Copy          => "copy",
            LirOpCode::CopyFromRef   => "copy_from_ref",
            LirOpCode::CopyFromDeref => "copy_from_deref",
            LirOpCode::CopyToDeref   => "copy_to_deref",
            LirOpCode::Add           => "add",
            LirOpCode::Sub           => "sub",
            LirOpCode::Mul           => "mul",
            LirOpCode::Div           => "div",
            LirOpCode::Pow           => "pow",
            LirOpCode::Mod           => "mod",
            LirOpCode::And           => "and",
            LirOpCode::Or            => "or",
            LirOpCode::Eq            => "eq",
            LirOpCode::Ne            => "ne",
            LirOpCode::Lt            => "lt",
            LirOpCode::Le            => "le",
            LirOpCode::Gt            => "gt",
            LirOpCode::Ge            => "ge",
            LirOpCode::Not           => "not",
            LirOpCode::IfLt          => "iflt",
            LirOpCode::IfGt          => "ifgt",
            LirOpCode::IfLe          => "ifle",
            LirOpCode::IfGe          => "ifge",
            LirOpCode::IfEq          => "ifeq",
            LirOpCode::IfNe          => "ifne",
            LirOpCode::Param         => "param",
            LirOpCode::Call          => "call",
            LirOpCode::Return        => "return",
            LirOpCode::Label         => "label",
            LirOpCode::Jump          => "jump",
            LirOpCode::Prologue      => "prologue",
            LirOpCode::Epilogue      => "epilogue\n",
        };
        write!(f, "{}", result)
    }
}

impl fmt::Display for LirOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LirOperand::Stack(addr) => write!(f, "$sp + {:#x}", addr),
            LirOperand::Register(index) => write!(f, "r{}", index),
            LirOperand::Label(label) => {
                if label.index > 0 {
                    write!(f, "{}{}", resolve_symbol(label.symbol), label.index)
                } else {
                    write!(f, "{}", resolve_symbol(label.symbol))
                }
            }
            LirOperand::Constant(val) => match val {
                LirValue::I32(v) => write!(f, "{}", v),
                LirValue::Bool(v) => write!(f, "{}", v),
            }
            LirOperand::None => write!(f, ""),
        }
    }
}

pub fn create_lir_context<'a>() -> LirContext<'a> {
    LirContext {
        file: None,
        instructions: Vec::new(),
        jump_table: HashMap::new(),
        basic_blocks: Vec::new(),
        unique_label_index: 0,
    }
}

fn create_lir_basic_block() -> LirBasicBlock {
    LirBasicBlock {
        stack_offsets: HashMap::new(),
        next_stack_offset: 0,
        next_register: 0,
        largest_stack_offset: 0,
    }
}

fn create_lir_label(symbol: Symbol, i: &mut u32) -> LirLabel {
    let label = LirLabel {
        symbol,
        index: *i,
    };
    *i += 1;
    label
}


fn allocate_stack<'a>(lc: &mut LirContext<'a>, symbol: Symbol) -> LirOperand {
    if lc.basic_blocks.len() == 0 {
        panic!("not inside any scope");
    }

    let len = lc.basic_blocks.len();
    let frame = &mut lc.basic_blocks[len - 1];
    let offset = frame.next_stack_offset;
    frame.stack_offsets.insert(symbol, offset);
    frame.next_stack_offset += 4; // NOTE(alexander): all types are 4-bits in size
    if frame.next_stack_offset > frame.largest_stack_offset {
        frame.largest_stack_offset = frame.next_stack_offset;
    }
    LirOperand::Stack(offset)
}

fn lookup_stack<'a>(lc: &mut LirContext<'a>, symbol: Symbol) -> LirOperand {
    for frame in &lc.basic_blocks {
        if let Some(offset) = frame.stack_offsets.get(&symbol) {
            return LirOperand::Stack(*offset);
        }
    }

    LirOperand::None
}

fn allocate_register<'a>(lc: &mut LirContext<'a>) -> LirOperand {
    if lc.basic_blocks.len() == 0 {
        panic!("not inside any scope");
    }

    let len = lc.basic_blocks.len();
    let frame = &mut lc.basic_blocks[len - 1];
    let register = frame.next_register;
    frame.next_register += 1;
    LirOperand::Register(register)
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
                op1: LirOperand::Label(create_lir_label(func.ident.sym, &mut 0u32)),
                span: func.span,
                ..Default::default()
            });
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Prologue,
                op1: LirOperand::Constant(LirValue::I32()),
                ..Default::default()
            });

            let bb = build_lir_from_block(lc, &func.block);
            

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Epilogue,
                ..Default::default()
            });
        }
        _ => { }
    }
}

// NOTE(alexander): this function is responsible for storing to immediate register whenever the
// two operands are both accessing memory since this is not allowed on some processors e.g. x86.
// TODO(alexander): this could be implemented in the codegen backend instead, it is architecture dependant!
// pub fn resolve_stack_to_stack_operands<'a>(
//     lc: &mut LirContext<'a>,
//     op1: &LirOperand,
//     op2: &LirOperand
// ) -> (LirOperand, LirOperand) {
//     let (op1, op2, op3) = match op1 {
//         LirOperand::Stack(_) => match op2 {
//             LirOperand::Stack(_) => (allocate_register(lc), op1, op2),
//             LirOperand::Register(_) => (op2, op1, LirOperand::None),
            
//             _ => panic!("only registers or stack operands allowed here!"),
//         }

//         LirOperand::Register(_) => (op1, op2, LirOperand::None),

//         _ => panic!("only registers or stack operands allowed here!"),
//     }

//     if let LirOperand::None == op3 {
//         let 

//     } else {
//         lc.instructions.push(LirInstruction {
//             opcode: LirOpCode::Copy,
//             op1,
//             op2,
//             span: assign.span,
//             ..Default::default()
//         });
        
//     }
// }

pub fn build_lir_from_block<'a>(lc: &mut LirContext<'a>, block: &Block) -> LirOperand {
    let bblen = lc.basic_blocks.len();
    let mut next_stack_offset = 0;
    let mut next_register = 0;
    let mut current_largest_stack_offset = 0;
    if bblen >= 1 {
        next_stack_offset = lc.basic_blocks[bblen - 1].next_stack_offset;
        next_register = lc.basic_blocks[bblen - 1].next_register;
        current_largest_stack_offset = lc.basic_blocks[bblen - 1].largest_stack_offset;
    }

    let mut basic_block = create_lir_basic_block();
    basic_block.next_stack_offset = next_stack_offset;
    basic_block.next_register = next_register;
    lc.basic_blocks.push(basic_block);

    let mut last_op = LirOperand::None;
    for stmt in &block.stmts {
        last_op = build_lir_from_stmt(lc, &stmt);
    }

    let bblen = lc.basic_blocks.len();
    if bblen >= 2 {
        if lc.basic_blocks[bblen - 1].largest_stack_offset > current_largest_stack_offset {
            lc.basic_blocks[bblen - 2].largest_stack_offset = lc.basic_blocks[bblen - 1].largest_stack_offset;
        }
    }

    lc.basic_blocks.pop();

    if let Some(Stmt::Expr(_)) = block.stmts.last() {
        if let LirOperand::None = last_op {
            LirOperand::None
        } else {
            let op1 = allocate_register(lc);
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Copy,
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
            let op2 = match &*local.init {
                Some(expr) => build_lir_from_expr(lc, expr),
                None => {
                    LirOperand::None // temp, handle uninitialized case
                }
            };

            let op1 = allocate_stack(lc, local.ident.sym);
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Copy,
                op1,
                op2,
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
            let op1 = match &*assign.left {
                Expr::Ident(ident) => lookup_stack(lc, ident.sym),
                _ => panic!("expected identifier"),
            };
            // let (op1, op2) = resolve_stack_to_stack_operands(lc, op1, op2);


            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Copy,
                op1,
                op2,
                span: assign.span,
                ..Default::default()
            });

            op1
        }

        Expr::Binary(binary) => {
            let op2 = build_lir_from_expr(lc, &binary.right);
            let op1 = build_lir_from_expr(lc, &binary.left);
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
                span: binary.span,
                ..Default::default()
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
                
                param_size += 1;
            }

            let op1 = LirOperand::Label(create_lir_label(call.ident.sym, &mut 0u32));
            let op2 = LirOperand::Constant(LirValue::I32(param_size));
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

        Expr::Ident(ident) => {
            let op1 = allocate_register(lc);
            let op2 = lookup_stack(lc, ident.sym);

            if let LirOperand::None = op2 {
                panic!("failed to lookup identifier `{}`", resolve_symbol(ident.sym));
            } 
            
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Copy,
                op1,
                op2,
                span: ident.span,
                ..Default::default()
            });
            
            op1
        }

        Expr::If(if_expr) => {
            let exit_label = intern_string("if_exit"); // TODO(alexander): handle duplicate labels!
            let mut else_label: Option<Symbol> = None;

            let mut op1 = LirOperand::None;
            let mut op2 = LirOperand::None;
            let op3 = match if_expr.else_block {
                Some(_) => {
                    let sym = intern_string("if_else"); // TODO(alexander): handle duplicate labels!
                    else_label = Some(sym);
                    LirOperand::Label(create_lir_label(sym, &mut lc.unique_label_index))
                }

                None => LirOperand::Label(create_lir_label(exit_label, &mut lc.unique_label_index))
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
                op2 = LirOperand::Constant(LirValue::Bool(true));
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
                    op1: LirOperand::Label(create_lir_label(exit_label, &mut lc.unique_label_index)),
                    ..Default::default()
                });
  
                lc.instructions.push(LirInstruction {
                    opcode: LirOpCode::Label,
                    op1: LirOperand::Label(create_lir_label(label, &mut lc.unique_label_index)),
                    ..Default::default()
                });
            }

            if let Some(block) = &if_expr.else_block {
                build_lir_from_block(lc, &block);
            }

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Label,
                op1: LirOperand::Label(create_lir_label(exit_label, &mut lc.unique_label_index)),
                ..Default::default()
            });

            LirOperand::None
        }

        Expr::Lit(literal) => match literal.lit {
            Lit::Int(val)  => LirOperand::Constant(LirValue::I32(val)),
            Lit::Bool(val) => LirOperand::Constant(LirValue::Bool(val)),
        }

        Expr::Paren(paren) => build_lir_from_expr(lc, &paren.expr),

        Expr::Reference(reference) => {
            let op1 = allocate_register(lc);
            let mut op2 = build_lir_from_expr(lc, &reference.expr);
            if let LirOperand::Register(offset) = op2 {
                op2 = LirOperand::Register(offset);
            } else {
                let op1 = allocate_register(lc);
                lc.instructions.push(LirInstruction {
                    opcode: LirOpCode::Copy,
                    op1,
                    op2,
                    span: reference.span,
                    ..Default::default()
                });

                op2 = op1;
                if let LirOperand::Register(offset) = op2 {
                    op2 = LirOperand::Register(offset);
                }
            };

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Copy,
                op1,
                op2,
                span: reference.span,
                ..Default::default()
            });
            
            op1
        }

        Expr::Return(return_expr) => {
            let op1 = match &*return_expr.expr {
                Some(expr) => build_lir_from_expr(lc, expr),
                None => LirOperand::None,
            };
            
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Return,
                op1,
                span: return_expr.span,
                ..Default::default()
            });

            LirOperand::None // NOTE(alexander): return should not be used as an atom in expression
        }

        Expr::Unary(unary) => {
            match unary.op {
                UnOp::Neg => {
                    let op3 = build_lir_from_expr(lc, &unary.expr);
                    let op1 = allocate_register(lc);
                    lc.instructions.push(LirInstruction {
                        opcode: LirOpCode::Sub,
                        op1,
                        op2: LirOperand::Constant(LirValue::I32(0)),
                        op3,
                        span: unary.span,
                    });

                    op1
                },

                UnOp::Not => {
                    let op2 = build_lir_from_expr(lc, &unary.expr);
                    let op1 = allocate_register(lc);
                    lc.instructions.push(LirInstruction {
                        opcode: LirOpCode::Not,
                        op1,
                        op2,
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                },

                UnOp::Deref => {
                    let op2 = build_lir_from_expr(lc, &unary.expr);
                    let op1 = allocate_register(lc);
                    lc.instructions.push(LirInstruction {
                        opcode: LirOpCode::CopyFromDeref,
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
