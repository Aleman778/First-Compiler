use std::collections::HashMap;
use std::fmt;
use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum LirOpCode {
    Nop,
    Load,
    Store,
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
    IfLt,
    IfGt,
    IfLe,
    IfGe,
    IfEq,
    IfNe,
    Push,
    Pop,
    Call,
    Jump,
    Label,
    Prologue, // marks beginning of function
    Epilogue, // marks end of function
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirOperand {
    Address(usize),
    StackOffset(u32), // same as address except relative to its residing stack frame
    Label(String), // TODO(alexander): should not use strings!!! string use interning
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
 * Represents a given stack frame, used to manage stack offsets.
 */
struct LirStackFrame {
    pub stack_offsets: HashMap<String, u32>,
    pub current_stack_offset: u32,
}

/**
 * Used for building low-level intermediate representation.
 */
pub struct LirContext<'a> {
    pub file: Option<&'a File>,
    pub instructions: Vec<LirInstruction>,
    stack_frames: Vec<LirStackFrame>,
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
            write!(f, "{}\n", insn)?;
        }
        Ok(())
    }
}

impl fmt::Display for LirInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{: <10}\t {: <10}\t {: <10}\t {}", self.opcode, self.op1, self.op2, self.op3)
    }
}

impl fmt::Display for LirOpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match self {
            LirOpCode::Nop       => "nop",
            LirOpCode::Load      => "load",
            LirOpCode::Store     => "store",
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
            LirOpCode::IfLt      => "iflt",
            LirOpCode::IfGt      => "ifgt",
            LirOpCode::IfLe      => "ifle",
            LirOpCode::IfGe      => "ifge",
            LirOpCode::IfEq      => "ifeq",
            LirOpCode::IfNe      => "ifne",
            LirOpCode::Push      => "push",
            LirOpCode::Pop       => "pop",
            LirOpCode::Call      => "call",
            LirOpCode::Jump      => "jump",
            LirOpCode::Label     => "label",
            LirOpCode::Prologue  => "prologue",
            LirOpCode::Epilogue  => "epilogue",
        };
        write!(f, "{}", result)
    }
}

impl fmt::Display for LirOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LirOperand::Address(addr) => write!(f, "{:#X}", addr),
            LirOperand::StackOffset(offset) => write!(f, "-{:#X}", offset),
            LirOperand::Label(label) => write!(f, "{}", label),
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
        stack_frames: Vec::new(),
    }
}

fn create_lir_stack_frame() -> LirStackFrame {
    LirStackFrame {
        stack_offsets: HashMap::new(),
        current_stack_offset: 0,
    }
}

fn store_local_variable<'a>(lc: &mut LirContext<'a>, symbol: Option<String>) -> LirOperand {
    if lc.stack_frames.len() == 0 {
        return LirOperand::None // NOTE(alexander): this should never occur!
    }

    let len = lc.stack_frames.len();
    let frame = &mut lc.stack_frames[len - 1];
    let offset = frame.current_stack_offset;
    if let Some(name) = symbol {
        frame.stack_offsets.insert(name, offset);
    }
    frame.current_stack_offset += 4; // NOTE(alexander): i32/ bool are both 4-bits.
    LirOperand::StackOffset(offset)
}

fn load_local_variable<'a>(lc: &mut LirContext<'a>, symbol: String) -> LirOperand {
    for frame in &lc.stack_frames {
        if let Some(offset) = frame.stack_offsets.get(&symbol) {
            return LirOperand::StackOffset(*offset);
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
                op1: LirOperand::Label(func.ident.to_string.clone()),
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

pub fn build_lir_from_block<'a>(lc: &mut LirContext<'a>, block: &Block) {
    lc.stack_frames.push(create_lir_stack_frame());
    for stmt in &block.stmts {
        build_lir_from_stmt(lc, &stmt);
    }
    lc.stack_frames.pop();
}

pub fn build_lir_from_stmt<'a>(lc: &mut LirContext<'a>, stmt: &Stmt) {
    match stmt {
        Stmt::Local(local) => {
            let operand = match &*local.init {
                Some(expr) => build_lir_from_expr(lc, expr),
                None => {

                    LirOperand::None // temp, handle uninitialized case
                }
            };

            let op1 = store_local_variable(lc, Some(local.ident.to_string.clone()));
            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Store,
                op1,
                op2: operand,
                span: local.span,
                ..Default::default()
            });
        }
        Stmt::Item(item) => { }

        Stmt::Semi(expr) => { build_lir_from_expr(lc, expr); }
        Stmt::Expr(expr) => { build_lir_from_expr(lc, expr); }
    }
}

pub fn build_lir_from_expr<'a>(lc: &mut LirContext<'a>, expr: &Expr) -> LirOperand {
    match expr {
        Expr::Assign(assign) => {
            let op1 = build_lir_from_expr(lc, &assign.left);
            let op2 = build_lir_from_expr(lc, &assign.right);

            lc.instructions.push(LirInstruction {
                opcode: LirOpCode::Store, // store: op1 = op2
                op1: op1.clone(),
                op2,
                span: assign.span,
                ..Default::default()
            });

            op1
        }

        Expr::Binary(binary) => {
            let op1 = store_local_variable(lc, None);
            let op2 = build_lir_from_expr(lc, &binary.left);
            let op3 = build_lir_from_expr(lc, &binary.right);
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
                op1: op1.clone(),
                op2,
                op3,
                span: binary.span,
                ..Default::default()
            });

            op1
        }

        Expr::Block(block) => {
            build_lir_from_block(lc, &block.block);
            LirOperand::None
        }

        Expr::Break(ident) => {
            LirOperand::None
        }

        Expr::Call(call) => {
            LirOperand::None
        }

        Expr::Continue(ident) => {
            LirOperand::None
        }

        Expr::Ident(ident) => load_local_variable(lc, ident.to_string.clone()),

        Expr::If(if_expr) => {
            LirOperand::None
        }

        Expr::Lit(literal) => match literal.lit {
            Lit::Int(val)  => LirOperand::ConstantInt(val),
            Lit::Bool(val) => LirOperand::ConstantBool(val),
        },

        Expr::Paren(paren) => build_lir_from_expr(lc, &paren.expr),

        Expr::Reference(reference) => {
            LirOperand::None
        }

        Expr::Return(return_expr) => {
            LirOperand::None
        }

        Expr::Unary(unary) => {
            LirOperand::None
        }

        Expr::While(while_expr) => {
            LirOperand::None
        }
    }
}
