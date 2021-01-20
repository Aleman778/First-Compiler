use std::collections::HashMap;
use std::fmt;
use std::mem;
use crate::ast::*;
use crate::intrinsics;

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
    Xor,
    Eq, // op1 = op2 binop op3 (op1 always boolean)
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    IfLt, // jump op3 (if op1 binop op2 equals true)
    IfGt,
    IfLe,
    IfGe,
    IfEq,
    IfNe,
    Jump,     // jump op1
    Label,    // label op1
    Param,    // param op1 (each param are insn. ordered left-to-right)
    Call,     // jump op1 with op2 = the number of params, return value stored in r0
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
    Stack(isize),
    Register(usize),
    Label(IrLabel),
    Constant(IrValue),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrValue {
    I32(i32),
    U32(u32),
    U64(u64),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrType {
    I8,
    I32,
    U32,
    U64,
    PtrI8(usize), // NOTE(alexander): argument defines the numbers of indirections
    PtrI32(usize),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrLabel {
    pub symbol: Symbol,
    pub index: u32, // used to distinguish labels with the same symbol.
    pub function: bool, // separate function labels from regular labels
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
    pub required_stack_size: isize, // needed for pre allocating stack space in prologue
    pub return_type: IrType,
}

/**
 * Used to maintain information about the stack offsets and available registers.
 */
pub struct IrBlockContext {
    stack_entries: HashMap<Symbol, (IrType, isize)>,
    next_register: usize,
    next_stack_offset: isize,
    largest_stack_size: isize,
    enter_label: Option<IrLabel>,
    exit_label: Option<IrLabel>,
    return_type: IrType,
}

/**
 * Used for building low-level intermediate representation.
 */
pub struct IrBuilder<'a> {
    pub file: Option<&'a File>,
    pub instructions: Vec<IrInstruction>,
    pub functions: HashMap<IrLabel, IrBasicBlock>,
    pub intrinsics: HashMap<Symbol, usize>, // NOTE(alexander): function pointer address.
    pub addr_size: isize, // address size in bytes on target architecture
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
            IrOperandKind::None => {}
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
            IrOpCode::Xor           => "xor",
            IrOpCode::Eq            => "eq",
            IrOpCode::Ne            => "ne",
            IrOpCode::Lt            => "lt",
            IrOpCode::Le            => "le",
            IrOpCode::Gt            => "gt",
            IrOpCode::Ge            => "ge",
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
            IrType::U32       => write!(f, "u32"),
            IrType::U64       => write!(f, "u64"),
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
                IrValue::U32(v) => write!(f, "{}", v),
                IrValue::U64(v) => write!(f, "{}", v),
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
        intrinsics: HashMap::new(),
        addr_size: std::mem::size_of::<usize>() as isize,
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

fn create_ir_block_context(
    enter_label: Option<IrLabel>,
    exit_label: Option<IrLabel>,
    return_type: IrType
) -> IrBlockContext {

    IrBlockContext {
        stack_entries: HashMap::new(),
        next_stack_offset: 0,
        next_register: 0,
        largest_stack_size: 0,
        enter_label,
        exit_label,
        return_type,
    }
}

fn create_ir_basic_block(return_type: IrType) -> IrBasicBlock {
    IrBasicBlock {
        prologue: 0,
        epilogue: 0,
        required_stack_size: 0,
        return_type,
    }
}

fn create_ir_label<'a>(
    ib: &mut IrBuilder<'a>,
    symbol: Symbol,
    label_index: Option<u32>,
    function: bool
) -> IrLabel {

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
        function,
    }
}

fn allocate_stack<'a>(ib: &mut IrBuilder<'a>, symbol: Symbol, ty: IrType) -> IrOperand {
    if ib.blocks.len() == 0 {
        panic!("not inside any scope");
    }

    let len = ib.blocks.len();
    let frame = &mut ib.blocks[len - 1];
    frame.next_stack_offset += size_of_ir_type(ty, ib.addr_size);
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
        TyKind::Error => IrType::None,
        TyKind::None => IrType::None,
    }
}

pub fn size_of_ir_type(ty: IrType, addr_size: isize) -> isize {
    match ty {
        IrType::I8 => 1,
        IrType::I32 => 4,
        IrType::U32 => 4,
        IrType::U64 => 8,
        IrType::PtrI8(_) |
        IrType::PtrI32(_) => addr_size,
        IrType::None => 0,
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

    fn register_ast_items<'a>(ib: &mut IrBuilder<'a>, items: &Vec<Item>) {
        for item in items {
            match item {
                Item::Fn(func) => {
                    let basic_block = create_ir_basic_block(to_ir_type(&func.decl.output));
                    let enter_label = create_ir_label(ib, func.ident.sym, Some(0), true);
                    ib.functions.insert(enter_label, basic_block);
                }

                Item::ForeignFn(func) => {
                    let func_ident = resolve_symbol(func.ident.sym);
                    let func_address = match func_ident {
                        "print_int" => {
                            unsafe {
                                mem::transmute(intrinsics::print_int as extern "cdecl" fn(i32))
                            }
                            // intrinsics::print_int as *const () as usize
                        }

                        "print_bool" => {
                            intrinsics::print_bool as *const () as usize
                        }

                        "assert" => {
                            intrinsics::assert as *const () as usize
                        }

                        "assert_eq_int" => {
                            intrinsics::assert_eq_int as *const () as usize
                        }

                        "assert_eq_bool" => {
                            intrinsics::assert_eq_bool as *const () as usize
                        }

                        "trace" => {
                            0usize // NOTE(alexander): ignore trace, interpreter only function!
                        }

                        _ => panic!("unknown foreign function {}", func_ident),
                    };
                    ib.intrinsics.insert(func.ident.sym, func_address);
                }

                Item::ForeignMod(module) => register_ast_items(ib, &module.items),
            }
        }
    }

    // First register all items made available from the AST
    register_ast_items(ib, &file.items);

    for item in &file.items {
        build_ir_from_item(ib, &item);
    }
}

pub fn build_ir_from_item<'a>(ib: &mut IrBuilder<'a>, item: &Item) {
    match item {
        Item::Fn(func) => {
            let block_context = create_ir_block_context(None, None, to_ir_type(&func.decl.output));
            ib.blocks.push(block_context);

            let enter_label = create_ir_label(ib, func.ident.sym, Some(0), true);
            let exit_label = create_ir_label(ib, func.ident.sym, Some(1), true);

            // Setup function arguments
            {
                let len = ib.blocks.len();
                let frame = &mut ib.blocks[len - 1];
                let mut offset = -ib.addr_size*2; // NOTE(alexander): jump over return addr and base pointer.
                for arg in &func.decl.inputs {
                    let ty = to_ir_type(&arg.ty);
                    let size = size_of_ir_type(ty, ib.addr_size);
                    frame.stack_entries.insert(arg.ident.sym, (ty, offset));
                    offset -= size;
                }
            }

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Label,
                op1: create_label_ir_operand(enter_label),
                span: func.span,
                ..Default::default()
            });

            let prologue = ib.instructions.len();
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Prologue,
                op1: create_i32_ir_operand(0),
                ..Default::default()
            });

            let (_, block_context) = build_ir_from_block(ib, &func.block, Some(enter_label), Some(exit_label));

            let epilogue = ib.instructions.len();
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Label,
                op1: create_label_ir_operand(exit_label),
                span: func.span,
                ..Default::default()
            });

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Epilogue,
                ..Default::default()
            });

            match ib.functions.get_mut(&enter_label) {
                Some(block) => {
                    block.prologue = prologue;
                    block.epilogue = epilogue;
                    block.required_stack_size = block_context.largest_stack_size;
                }
                None => {
                    let basic_block = IrBasicBlock {
                        prologue,
                        epilogue,
                        required_stack_size: block_context.largest_stack_size,
                        return_type: to_ir_type(&func.decl.output),
                    };
                    ib.functions.insert(enter_label, basic_block);
                }
            }

            ib.blocks.pop();
        }
        _ => { }
    }
}

pub fn build_ir_from_block<'a>(
    ib: &mut IrBuilder<'a>,
    block: &Block,
    enter_label: Option<IrLabel>,
    exit_label: Option<IrLabel>
) -> (IrOperand, IrBlockContext) {

    let blocks_len = ib.blocks.len();
    let mut next_stack_offset = 0;
    let mut next_register = 0;
    let mut current_largest_stack_size = 0;
    if blocks_len >= 1 {
        next_stack_offset = ib.blocks[blocks_len - 1].next_stack_offset;
        next_register = ib.blocks[blocks_len - 1].next_register;
        current_largest_stack_size = ib.blocks[blocks_len - 1].largest_stack_size;
    }

    let return_type = ib.blocks.last().map(|ibc| ibc.return_type).unwrap_or(IrType::None);
    let mut block_context = create_ir_block_context(enter_label, exit_label, return_type);
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
            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Return,
                op1: last_op,
                span: block.span,
                ..Default::default()
            });
            (last_op, block_context)
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

            match binary.op {
                BinOp::Eq |
                BinOp::Ne |
                BinOp::Lt |
                BinOp::Le |
                BinOp::Gt |
                BinOp::Ge => {
                    let op3 = op2;
                    let op2 = op1;
                    let op1 = allocate_register(ib, IrType::I8);
                    ib.instructions.push(IrInstruction {
                        opcode,
                        op1,
                        op2,
                        op3,
                        span: binary.span,
                    });

                    op1
                }

                _ => {
                    ib.instructions.push(IrInstruction {
                        opcode,
                        op1,
                        op2,
                        span: binary.span,
                        ..Default::default()
                    });

                    op1
                }
            }
        }

        Expr::Block(block) => build_ir_from_block(ib, &block.block, None, None).0,

        Expr::Break(_) |
        Expr::Continue(_) => {
            let entry_symbol = intern_string("while_enter");
            for block in ib.blocks.iter().rev() {
                if let Some(enter_label) = block.enter_label {
                    if entry_symbol == enter_label.symbol {
                        if let Expr::Continue(cont_expr) = expr {
                            ib.instructions.push(IrInstruction {
                                opcode: IrOpCode::Jump,
                                op1: create_label_ir_operand(enter_label),
                                span: cont_expr.span,
                                ..Default::default()
                            });
                        } else if let Expr::Break(break_expr) = expr {
                            ib.instructions.push(IrInstruction {
                                opcode: IrOpCode::Jump,
                                op1: create_label_ir_operand(block.exit_label.unwrap()),
                                span: break_expr.span,
                                ..Default::default()
                            });
                        }
                        break;
                    }
                }
            }
            create_ir_operand()
        }

        Expr::Call(call) => {
            // let mut param_size = 0;
            // for arg in &call.args {
            //     let op1 = build_ir_from_expr(ib, &arg);
            //     ib.instructions.push(IrInstruction {
            //         opcode: IrOpCode::Param,
            //         op1,
            //         span: arg.get_span(),
            //         ..Default::default()
            //     });

            //     param_size += 1;
            // }

            match ib.intrinsics.get(&call.ident.sym) {
                Some(func_address) => {
                    let (ty, val) = if ib.addr_size == 4 {
                        (IrType::U32, IrValue::U32(*func_address as u32))
                    } else if ib.addr_size == 8 {
                        (IrType::U64, IrValue::U64(*func_address as u64))
                    } else {
                        panic!("unsupported address size: `{}-bit`, expected 32- or 64-bit", ib.addr_size*8);
                    };

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
                    
                    let op1 = IrOperand {
                        ty,
                        kind: IrOperandKind::Constant(val),
                    };

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::Call,
                        op1,
                        op2: create_i32_ir_operand(param_size),
                        span: call.span,
                        ..Default::default()
                    });

                    IrOperand {
                        ty: IrType::None,
                        kind: IrOperandKind::Register(0),
                    }
                }

                None => {
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

                    
                    let function_label = create_ir_label(ib, call.ident.sym, Some(0), true);
                    let op1 = create_label_ir_operand(function_label);

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::Call,
                        op1,
                        op2: create_i32_ir_operand(param_size),
                        span: call.span,
                        ..Default::default()
                    });

                    IrOperand {
                        ty: ib.functions.get(&function_label).map(|ibc| ibc.return_type).unwrap_or(IrType::I32),
                        kind: IrOperandKind::Register(0),
                    }
                }
            }
        }

        Expr::Ident(ident) => {
            // lookup_stack(ib, ident.sym) // NOTE(alexander): this may cause stack read/write in same insn
            let op2 = lookup_stack(ib, ident.sym);
            let op1 = allocate_register(ib, op2.ty);

            if let IrOperandKind::None = op2.kind {
                panic!("failed to lookup identifier `{}`", resolve_symbol(ident.sym));
            }

            ib.instructions.push(IrInstruction {
                opcode: IrOpCode::Copy,
                op1,
                op2,
                span: ident.span,
                ..Default::default()
            });

            op1
        }

        Expr::If(if_expr) => {
            let exit_label = create_ir_label(ib, intern_string("if_exit"), None, false);
            let mut else_label: Option<IrLabel> = None;

            let mut op1 = create_ir_operand();
            let mut op2 = create_ir_operand();
            let op3 = match if_expr.else_block {
                Some(_) => {
                    let label = create_ir_label(ib, intern_string("if_else"), None, false);
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

                    if let IrOpCode::Nop = opcode {
                    } else {
                        op1 = build_ir_from_expr(ib, &binary.left);
                        op2 = build_ir_from_expr(ib, &binary.right);
                    }
                    opcode
                }
                _ => IrOpCode::Nop,
            };

            if let IrOpCode::Nop = opcode {
                opcode = IrOpCode::IfEq;
                op1 = build_ir_from_expr(ib, &if_expr.cond);
                op2 = create_bool_ir_operand(false);
            }

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                op3,
                span: if_expr.span,
            });

            build_ir_from_block(ib, &if_expr.then_block, None, Some(else_label.unwrap_or(exit_label)));

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
                build_ir_from_block(ib, &block, else_label, Some(exit_label));
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

            // NOTE(alexander): better handled by the actual backend implementation
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
                _                 => panic!("unsupported type"),
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
                    let op1 = build_ir_from_expr(ib, &unary.expr);
                    ib.instructions.push(IrInstruction {
                        opcode: IrOpCode::Xor,
                        op1,
                        op2: create_bool_ir_operand(true),
                        span: unary.span,
                        ..Default::default()
                    });

                    op1
                },

                UnOp::Deref => {
                    let op2 = build_ir_from_expr(ib, &unary.expr);
                    let mut op1 = allocate_register(ib, op2.ty);

                    op1.ty = match op1.ty {
                        IrType::I8  |
                        IrType::I32 |
                        IrType::U32 |
                        IrType::U64 => panic!("cannot dereference non ref type"),

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
            let enter_label = create_ir_label(ib, intern_string("while_enter"), None, false);
            let exit_label = create_ir_label(ib, intern_string("while_exit"),  None, false);

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

                    if let IrOpCode::Nop = opcode {
                    } else {
                        op1 = build_ir_from_expr(ib, &binary.left);
                        op2 = build_ir_from_expr(ib, &binary.right);
                    }
                    opcode
                }
                _ => IrOpCode::Nop,
            };

            if let IrOpCode::Nop = opcode {
                opcode = IrOpCode::IfEq;
                op1 = build_ir_from_expr(ib, &while_expr.cond);
                op2 = create_bool_ir_operand(false);
            }

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                op3: create_label_ir_operand(exit_label),
                span: while_expr.span,
            });

            build_ir_from_block(ib, &while_expr.block, Some(enter_label), Some(exit_label));

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
