use std::collections::HashMap;
use std::fmt;
use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum IrOpCode {
    Nop,
    Breakpoint,
    Copy, // op1 = op2
    CopyFromDeref, // op1 = *op2
    CopyFromRef, // op1 = &op2 (these are always mutable refs)
    CopyToDeref, // *op1 = op2
    Clear, // op1 = 0
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
    Jump,     // jump op1
    Label,    // label op1
    Param,    // params (assigned left-to-right)
    Call,     // jump op1 with op2 = the number of params
    Return,   // return op1 (where op1 is optional, may be None)
    Prologue, // marks beginning of function, op1 holds the required stack space
    Epilogue, // marks end of function
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IrOperand {
    pub ty: IrType,
    pub kind: IrOperandKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrOperandKind {
    Stack(usize),
    Register(usize),
    Label(IrLabel),
    Constant(IrValue),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrValue {
    I32(i32),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrType {
    I8,
    I32,
    PtrI8(usize), // NOTE(alexander): argument defines the numbers of indirections
    PtrI32(usize),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrLabel {
    pub symbol: Symbol,
    pub index: u32, // used to distinguish labels with the same symbol.
}

/**
 * Three address code instruction, is defined an op code and up to three operands.
 * Span is also used for debugging to retrieve the source location of a given instruction.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct IrInstruction {
    pub opcode: IrOpCode,
    pub op1: IrOperand,
    pub op2: IrOperand,
    pub op3: IrOperand,
    pub span: Span,
}

/**
 * Represents a block containing multiple instructions, this
 * is used to manage stack offsets. Only used for function blocks.
 */
pub struct IrBasicBlock {
    pub prologue: usize, // index into IrBuilder instruction vector
    pub epilogue: usize,
    pub required_stack_size: usize, // needed for pre allocating stack space in prologue
}

/**
 * Used to maintain information about the stack offsets and available registers.
 */
pub struct IrBlockContext {
    stack_entries: HashMap<Symbol, (IrType, usize)>,
    next_stack_offset: usize,
    next_register: usize,
    largest_stack_size: usize,
}

/**
 * Used for building low-level intermediate representation.
 */
pub struct IrBuilder<'a> {
    pub file: Option<&'a File>,
    pub instructions: Vec<IrInstruction>,
    pub functions: HashMap<IrLabel, IrBasicBlock>,
    pub addr_size: usize, // address size in bytes on target architecture
    pub unique_index: HashMap<Symbol, u32>,
    blocks: Vec<IrBlockContext>,
}

impl Default for IrInstruction {
    fn default() -> Self {
        IrInstruction {
            opcode: IrOpCode::Nop,
            op1: create_ir_operand(),
            op2: create_ir_operand(),
            op3: create_ir_operand(),
            span: Span::new(),
        }
    }
}

impl fmt::Display for IrBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for insn in &self.instructions {
            match insn.opcode {
                IrOpCode::Label => write!(f, "{}:\n", insn.op1)?,
                _ => write!(f, "    {}\n", insn)?,
            }
        }
        Ok(())
    }
}

impl fmt::Display for IrInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:<15} {}", format!("{}", self.opcode), self.op1)?;
        match self.op2.kind {
            IrOperandKind::None => { }
            _ => {
                write!(f, ", {}", self.op2)?;
                match self.op3.kind {
                    IrOperandKind::None => { }
                    _ => { write!(f, ", {}", self.op3)?; }
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for IrOpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match self {
            IrOpCode::Nop           => "nop",
            IrOpCode::Breakpoint    => "breakpoint",
            IrOpCode::Copy          => "copy",
            IrOpCode::CopyFromRef   => "copy_from_ref",
            IrOpCode::CopyFromDeref => "copy_from_deref",
            IrOpCode::CopyToDeref   => "copy_to_deref",
            IrOpCode::Clear         => "clear",
            IrOpCode::Add           => "add",
            IrOpCode::Sub           => "sub",
            IrOpCode::Mul           => "mul",
            IrOpCode::Div           => "div",
            IrOpCode::Pow           => "pow",
            IrOpCode::Mod           => "mod",
            IrOpCode::And           => "and",
            IrOpCode::Or            => "or",
            IrOpCode::Eq            => "eq",
            IrOpCode::Ne            => "ne",
            IrOpCode::Lt            => "lt",
            IrOpCode::Le            => "le",
            IrOpCode::Gt            => "gt",
            IrOpCode::Ge            => "ge",
            IrOpCode::Not           => "not",
            IrOpCode::IfLt          => "iflt",
            IrOpCode::IfGt          => "ifgt",
            IrOpCode::IfLe          => "ifle",
            IrOpCode::IfGe          => "ifge",
            IrOpCode::IfEq          => "ifeq",
            IrOpCode::IfNe          => "ifne",
            IrOpCode::Param         => "param",
            IrOpCode::Call          => "call",
            IrOpCode::Return        => "return",
            IrOpCode::Label         => "label",
            IrOpCode::Jump          => "jump",
            IrOpCode::Prologue      => "prologue",
            IrOpCode::Epilogue      => "epilogue\n",
        };
        write!(f, "{}", result)
    }
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::I8        => write!(f, "i8"),
            IrType::I32       => write!(f, "i32"),
            IrType::PtrI8(i)  => write!(f, "i8{}", "*".repeat(*i as usize)),
            IrType::PtrI32(i) => write!(f, "i32{}", "*".repeat(*i as usize)),
            IrType::None      => write!(f, ""),
        }
    }
}

impl fmt::Display for IrLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.index > 0 {
            write!(f, "{}{}", resolve_symbol(self.symbol), self.index)
        } else {
            write!(f, "{}", resolve_symbol(self.symbol))
        }
    }
}

impl fmt::Display for IrOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let IrType::None = self.ty {
        } else {
            write!(f, "{} ", self.ty)?;
        }
        match &self.kind {
            IrOperandKind::Stack(addr) => write!(f, "$sp + {:#x}", addr),
            IrOperandKind::Register(index) => write!(f, "r{}", index),
            IrOperandKind::Label(label) => write!(f, "{}", label),
            IrOperandKind::Constant(val) => match val {
                IrValue::I32(v) => write!(f, "{}", v),
                IrValue::Bool(v) => write!(f, "{}", v),
            }
            IrOperandKind::None => write!(f, ""),
        }
    }
}

pub fn create_ir_builder<'a>() -> IrBuilder<'a> {
    IrBuilder {
        file: None,
        instructions: Vec::new(),
        functions: HashMap::new(),
        addr_size: std::mem::size_of::<usize>(),
        blocks: Vec::new(),
        unique_index: HashMap::new(),
    }
}

pub fn create_ir_operand() -> IrOperand {
    IrOperand {
        ty: IrType::None,
        kind: IrOperandKind::None,
    }
}

fn create_ir_block_context() -> IrBlockContext {
    IrBlockContext {
        stack_entries: HashMap::new(),
        next_stack_offset: 0,
        next_register: 0,
        largest_stack_size: 0,
    }
}

fn create_ir_label<'a>(ib: &mut IrBuilder<'a>, symbol: Symbol, label_index: Option<u32>) -> IrLabel {
    let index = match label_index {
        Some(index) => index,
        None => {
            match ib.unique_index.get_mut(&symbol) {
                Some(index) => {
                    let curr_index = *index;
                    *index += 1;
                    curr_index
                },

                None => {
                    ib.unique_index.insert(symbol, 1);
                    0
                }
            }
        }
    };
    
    IrLabel {
        symbol,
        index,
    }
}

fn allocate_stack<'a>(ib: &mut IrBuilder<'a>, symbol: Symbol, ty: IrType) -> IrOperand {
    if ib.blocks.len() == 0 {
        panic!("not inside any scope");
    }

    let len = ib.blocks.len();
    let frame = &mut ib.blocks[len - 1];
    frame.next_stack_offset += match ty {
        IrType::I8 => 1,
        IrType::I32 => 4,
        IrType::PtrI8(_) |
        IrType::PtrI32(_) => ib.addr_size,
        IrType::None => panic!("missing type"),
    };
    let offset = frame.next_stack_offset;
    frame.stack_entries.insert(symbol, (ty, offset));
    if frame.next_stack_offset > frame.largest_stack_size {
        frame.largest_stack_size = frame.next_stack_offset;
    }

    IrOperand {
        ty,
        kind: IrOperandKind::Stack(offset),
    }
}

fn lookup_stack<'a>(ib: &mut IrBuilder<'a>, symbol: Symbol) -> IrOperand {
    for frame in &ib.blocks {
        if let Some((ty, offset)) = frame.stack_entries.get(&symbol) {
            let op = IrOperand {
                ty: *ty,
                kind: IrOperandKind::Stack(*offset),
            };
            return op;
        }
    }

    create_ir_operand()
}

fn allocate_register<'a>(ib: &mut IrBuilder<'a>, ty: IrType) -> IrOperand {
    if ib.blocks.len() == 0 {
        panic!("not inside any scope");
    }

    let len = ib.blocks.len();
    let frame = &mut ib.blocks[len - 1];
    let register = frame.next_register;
    frame.next_register += 1;
    IrOperand {
        ty,
        kind: IrOperandKind::Register(register),
    }
}

fn to_ir_ptr_type(ty: &Ty, indirections: &mut usize) -> IrType {
    match &ty.kind {
        TyKind::Int => IrType::PtrI32(*indirections),
        TyKind::Bool => IrType::PtrI8(*indirections),
        TyKind::Ref(type_ref) => {
            *indirections += 1;
            to_ir_ptr_type(&type_ref.elem, indirections)
        }
        
        _ => panic!("unexpected type"),
    }
}

fn to_ir_type(ty: &Ty) -> IrType {
    match &ty.kind {
        TyKind::Int => IrType::I32,
        TyKind::Bool => IrType::I8,
        TyKind::Ref(type_ref) => to_ir_ptr_type(&type_ref.elem, &mut 1),
        TyKind::None => IrType::None,
    }
}

pub fn push_ir_breakpoint<'a>(ib: &mut IrBuilder<'a>) {
    ib.instructions.push(IrInstruction {
        opcode: IrOpCode::Breakpoint,
        ..Default::default()
    });
}

pub fn build_ir_from_ast<'a>(ib: &mut IrBuilder<'a>, file: &'a File) {
    ib.file = Some(file);

    for item in &file.items {
        build_ir_from_item(ib, &item);
    }
}

pub fn build_ir_from_item<'a>(ib: &mut IrBuilder<'a>, item: &Item) {
    match item {
        Item::Fn(func) => {
            let label = create_ir_label(ib, func.ident.sym, Some(0));
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Label,
                op1: create_label_ir_operand(label),
                span: func.span,
                ..Default::default()
            });

            let prologue = ib.instructions.len();
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Prologue,
                op1: create_i32_ir_operand(0),
                ..Default::default()
            });

            let (_, block_context) = build_ir_from_block(ib, &func.block);

            let epilogue = ib.instructions.len();
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Epilogue,
                ..Default::default()
            });

            let basic_block = IrBasicBlock {
                prologue,
                epilogue,
                required_stack_size: block_context.largest_stack_size,
            };

            ib.functions.insert(label, basic_block);
        }
        _ => { }
    }
}

pub fn build_ir_from_block<'a>(ib: &mut IrBuilder<'a>, block: &Block) -> (IrOperand, IrBlockContext) {
    let blocks_len = ib.blocks.len();
    let mut next_stack_offset = 0;
    let mut next_register = 0;
    let mut current_largest_stack_size = 0;
    if blocks_len >= 1 {
        next_stack_offset = ib.blocks[blocks_len - 1].next_stack_offset;
        next_register = ib.blocks[blocks_len - 1].next_register;
        current_largest_stack_size = ib.blocks[blocks_len - 1].largest_stack_size;
    }

    let mut block_context = create_ir_block_context();
    block_context.next_stack_offset = next_stack_offset;
    block_context.next_register = next_register;
    ib.blocks.push(block_context);

    let mut last_op = create_ir_operand();
    for stmt in &block.stmts {
        last_op = build_ir_from_stmt(ib, &stmt);
    }

    let blocks_len = ib.blocks.len();
    if blocks_len >= 2 {
        if ib.blocks[blocks_len - 1].largest_stack_size > current_largest_stack_size {
            ib.blocks[blocks_len - 2].largest_stack_size = ib.blocks[blocks_len - 1].largest_stack_size;
        }
    }

    let block_context = ib.blocks.pop().unwrap();

    if let Some(Stmt::Expr(_)) = block.stmts.last() {
        if let IrOperandKind::None = last_op.kind {
            (create_ir_operand(), block_context)
        } else {
            let op1 = allocate_register(ib, last_op.ty);
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Copy,
                op1: op1,
                op2: last_op,
                span: block.span,
                ..Default::default()
            });
            (op1, block_context)
        }
    } else {
        (create_ir_operand(), block_context)
    }
}

pub fn build_ir_from_stmt<'a>(ib: &mut IrBuilder<'a>, stmt: &Stmt) -> IrOperand {
    match stmt {
        Stmt::Local(local) => {
            let init_type = to_ir_type(&local.ty);
            let op1 = allocate_stack(ib, local.ident.sym, init_type);
            let op2 = match &*local.init {
                Some(expr) => build_ir_from_expr(ib, expr),
                None => {
                    return create_ir_operand();
                }
            };

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Copy,
                op1,
                op2,
                span: local.span,
                ..Default::default()
            });

            create_ir_operand()
        }

        Stmt::Item(_) => create_ir_operand(),
        Stmt::Semi(expr) => { build_ir_from_expr(ib, expr) }
        Stmt::Expr(expr) => { build_ir_from_expr(ib, expr) }
    }
}

pub fn build_ir_from_expr<'a>(ib: &mut IrBuilder<'a>, expr: &Expr) -> IrOperand {

    match expr {
        Expr::Assign(assign) => {
            let mut opcode = IrOpCode::Copy;
            
            let op2 = build_ir_from_expr(ib, &assign.right);
            let op1 = match &*assign.left {
                Expr::Ident(ident) => lookup_stack(ib, ident.sym),
                Expr::Unary(unary) => {
                    if let UnOp::Deref = unary.op {
                        opcode = IrOpCode::CopyToDeref;
                    } else {
                        panic!("expected dereference")
                    }
                    build_ir_from_expr(ib, &unary.expr)
                }
                _ => panic!("expected identifier or dereference"),
            };

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                span: assign.span,
                ..Default::default()
            });

            op1
        }

        Expr::Binary(binary) => {
            let op1 = build_ir_from_expr(ib, &binary.left);
            let op2 = build_ir_from_expr(ib, &binary.right);
            let opcode = match binary.op {
                BinOp::Add => IrOpCode::Add,
                BinOp::Sub => IrOpCode::Sub,
                BinOp::Mul => IrOpCode::Mul,
                BinOp::Div => IrOpCode::Div,
                BinOp::Pow => IrOpCode::Pow,
                BinOp::Mod => IrOpCode::Mod,
                BinOp::And => IrOpCode::And,
                BinOp::Or  => IrOpCode::Or,
                BinOp::Eq  => IrOpCode::Eq,
                BinOp::Ne  => IrOpCode::Ne,
                BinOp::Lt  => IrOpCode::Lt,
                BinOp::Le  => IrOpCode::Le,
                BinOp::Gt  => IrOpCode::Gt,
                BinOp::Ge  => IrOpCode::Ge,
            };

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                span: binary.span,
                ..Default::default()
            });

            op1
        }

        Expr::Block(block) => build_ir_from_block(ib, &block.block).0,

        Expr::Break(_break_expr) => {
            create_ir_operand()
        }

        Expr::Call(call) => {
            let mut param_size = 0;
            for arg in &call.args {
                let op1 = build_ir_from_expr(ib, &arg);
                ib.instructions.push(IrInstruction {
                    opcode: IrOpCode::Param,
                    op1,
                    span: arg.get_span(),
                    ..Default::default()
                });

                param_size += 1;
            }

            let op1 = create_label_ir_operand(create_ir_label(ib, call.ident.sym, Some(0)));
            
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Call,
                op1,
                op2: create_i32_ir_operand(param_size),
                span: call.span,
                ..Default::default()
            });
            create_ir_operand()
        }

        Expr::Continue(_continue_expr) => {
            create_ir_operand()
        }

        Expr::Ident(ident) => {
            lookup_stack(ib, ident.sym)
            // let op2 = lookup_stack(ib, ident.sym);
            // let op1 = allocate_register(ib, op2.ty);

            // if let IrOperandKind::None = op2.kind {
                // panic!("failed to lookup identifier `{}`", resolve_symbol(ident.sym));
            // }

            // ib.instructions.push(IrInstruction {
                // opcode: IrOpCode::Copy,
                // op1,
                // op2,
                // span: ident.span,
                // ..Default::default()
            // });
            // 
            // op1
        }

        Expr::If(if_expr) => {
            let exit_label = create_ir_label(ib, intern_string("if_exit"), None);
            let mut else_label: Option<IrLabel> = None;

            let mut op1 = create_ir_operand();
            let mut op2 = create_ir_operand();
            let op3 = match if_expr.else_block {
                Some(_) => {
                    let label = create_ir_label(ib, intern_string("if_else"), None);
                    else_label = Some(label);
                    create_label_ir_operand(label)
                }

                None => create_label_ir_operand(exit_label),
            };
            
            let mut opcode = match &*if_expr.cond {
                Expr::Binary(binary) => {
                    let opcode = match binary.op {
                        BinOp::Lt => IrOpCode::IfGe,
                        BinOp::Gt => IrOpCode::IfLe,
                        BinOp::Le => IrOpCode::IfGt,
                        BinOp::Ge => IrOpCode::IfLt,
                        BinOp::Eq => IrOpCode::IfNe,
                        BinOp::Ne => IrOpCode::IfEq,
                        _ => IrOpCode::Nop,
                    };

                    op1 = build_ir_from_expr(ib, &binary.left);
                    op2 = build_ir_from_expr(ib, &binary.right);
                    opcode
                }
                _ => IrOpCode::Nop,
            };

            if let IrOpCode::Nop = opcode {
                opcode = IrOpCode::IfEq;
                op1 = build_ir_from_expr(ib, &if_expr.cond);
                op2 = IrOperand {
                    ty: IrType::I8,
                    kind: IrOperandKind::Constant(IrValue::Bool(false)),
                };
            }

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                op3,
                span: if_expr.span,
            });

            build_ir_from_block(ib, &if_expr.then_block);

            if let Some(label) = else_label {
                ib.instructions.push(IrInstruction {
                    opcode: IrOpCode::Jump,
                    op1: create_label_ir_operand(exit_label),
                    ..Default::default()
                });

                ib.instructions.push(IrInstruction {
                    opcode: IrOpCode::Label,
                    op1: create_label_ir_operand(label),
                    ..Default::default()
                });
            }

            if let Some(block) = &if_expr.else_block {
                build_ir_from_block(ib, &block);
            }

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Label,
                op1: create_label_ir_operand(exit_label),
                ..Default::default()
            });

            create_ir_operand()
        }

        Expr::Lit(literal) => match literal.lit {
            Lit::Int(val)  => create_i32_ir_operand(val),
            Lit::Bool(val) => create_bool_ir_operand(val),
        }

        Expr::Paren(paren) => build_ir_from_expr(ib, &paren.expr),

        Expr::Reference(reference) => {
            let op2 = build_ir_from_expr(ib, &reference.expr);
            let mut op1 = allocate_register(ib, op2.ty);
            
            // Copy from reference needs to be performed through a register.
            // let op2 = if let IrOperandKind::Register(_) = op2.kind {
            //     op2
            // } else {
            //     let op1 = allocate_register(ib, op2.ty);
            //     ib.instructions.push(IrInstruction {
            //         opcode: IrOpCode::Copy,
            //         op1,
            //         op2,
            //         span: reference.span,
            //         ..Default::default()
            //     });
            //     op1
            // };
            
            op1.ty = match op1.ty {
                IrType::I8        => IrType::PtrI8(1),
                IrType::I32       => IrType::PtrI32(1),
                IrType::PtrI8(i)  => IrType::PtrI8(i + 1),
                IrType::PtrI32(i) => IrType::PtrI32(i + 1),
                IrType::None      => panic!("missing type info"),
            };
            
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::CopyFromRef,
                op1,
                op2,
                span: reference.span,
                ..Default::default()
            });

            op1
        }

        Expr::Return(return_expr) => {
            let op1 = match &*return_expr.expr {
                Some(expr) => build_ir_from_expr(ib, expr),
                None => create_ir_operand(),
            };

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Return,
                op1,
                span: return_expr.span,
                ..Default::default()
            });

            create_ir_operand() // NOTE(alexander): return should not be used as an atom in expression
        }

        Expr::Unary(unary) => {
            match unary.op {
                UnOp::Neg => {
                    let op2 = build_ir_from_expr(ib, &unary.expr);
                    let op1 = allocate_register(ib, op2.ty);

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::Clear,
                        op1,
                        ..Default::default()
                    });

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::Sub,
                        op1,
                        op2, 
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                },

                UnOp::Not => {
                    let op2 = build_ir_from_expr(ib, &unary.expr);
                    let op1 = allocate_register(ib, op2.ty);
                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::Not,
                        op1,
                        op2,
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                },

                UnOp::Deref => {
                    let op2 = build_ir_from_expr(ib, &unary.expr);
                    let mut op1 = allocate_register(ib, op2.ty);
                    
                    op1.ty = match op1.ty {
                        IrType::I8 | 
                        IrType::I32 => panic!("cannot dereference non ref type"),
                        
                        IrType::PtrI8(i) => if i > 1 {
                            IrType::PtrI8(i - 1)
                        } else {
                            IrType::I8
                        }
                        
                        IrType::PtrI32(i) => if i > 1 {
                            IrType::PtrI32(i - 1)
                        } else {
                            IrType::I32
                        }
                        
                        IrType::None => panic!("missing type info"),
                    };
                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::CopyFromDeref,
                        op1,
                        op2,
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                }
            }
        }

        Expr::While(while_expr) => {
            let enter_label = create_ir_label(ib, intern_string("while_enter"), None);
            let exit_label = create_ir_label(ib, intern_string("while_exit"),  None);
            

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Label,
                op1: create_label_ir_operand(enter_label),
                ..Default::default()
            });

            let mut op1 = create_ir_operand();
            let mut op2 = create_ir_operand();
            let mut opcode = match &*while_expr.cond {
                Expr::Binary(binary) => {
                    let opcode = match binary.op {
                        BinOp::Lt => IrOpCode::IfGe,
                        BinOp::Gt => IrOpCode::IfLe,
                        BinOp::Le => IrOpCode::IfGt,
                        BinOp::Ge => IrOpCode::IfLt,
                        BinOp::Eq => IrOpCode::IfNe,
                        BinOp::Ne => IrOpCode::IfEq,
                        _ => IrOpCode::Nop,
                    };

                    op1 = build_ir_from_expr(ib, &binary.left);
                    op2 = build_ir_from_expr(ib, &binary.right);
                    opcode
                }
                _ => IrOpCode::Nop,
            };

            if let IrOpCode::Nop = opcode {
                opcode = IrOpCode::IfEq;
                op1 = build_ir_from_expr(ib, &while_expr.cond);
                op2 = IrOperand {
                    ty: IrType::I8,
                    kind: IrOperandKind::Constant(IrValue::Bool(false)),
                };
            }

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                op3: create_label_ir_operand(exit_label),
                span: while_expr.span,
            });

            build_ir_from_block(ib, &while_expr.block);

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Jump,
                op1: create_label_ir_operand(enter_label),
                ..Default::default()
            });

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Label,
                op1: create_label_ir_operand(exit_label),
                ..Default::default()
            });

            create_ir_operand()
        }
    }
}

fn create_i32_ir_operand(val: i32) -> IrOperand {
    IrOperand {
        ty: IrType::I32,
        kind: IrOperandKind::Constant(IrValue::I32(val)),
    }
}

fn create_bool_ir_operand(val: bool) -> IrOperand {
    IrOperand {
        ty: IrType::I8,
        kind: IrOperandKind::Constant(IrValue::Bool(val)),
    }
}

fn create_label_ir_operand(label: IrLabel) -> IrOperand {
    IrOperand {
        ty: IrType::None,
        kind: IrOperandKind::Label(label),
    }
}
