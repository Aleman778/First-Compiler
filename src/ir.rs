use std::collections::HashMap;
use std::fmt;
use crate::ast::*;
use crate::intrinsics;

/**
 * Used for building low-level intermediate representation.
 */
pub struct IrBuilder<'a> {
    pub file: Option<&'a File>,
    pub instructions: Vec<IrInstruction>,
    pub functions: HashMap<IrIdent, IrBasicBlock>,
    pub addr_size: isize, // address size in bytes on target architecture

    scopes: Vec<IrScope>,
    live_intervals: HashMap<IrIdent, IrLiveInterval>, // used per function moves to its basic block

    // Unique identifier generators
    register_symbol: Symbol,
    register_index: u32,
    basic_block_symbol: Symbol,
    basic_block_index: u32,
    if_exit_symbol: Symbol,
    if_exit_index: u32,
    if_else_symbol: Symbol,
    if_else_index: u32,
    while_enter_symbol: Symbol,
    while_enter_index: u32,
    while_exit_symbol: Symbol,
    while_exit_index: u32,
}

/**
 * IR scopes are defined as regular blocks that contains some
 * helper information about variables that are active.
 * Only for internal use to help build the IR.
 */
struct IrScope {
    enter_label: Option<IrIdent>,
    exit_label: Option<IrIdent>,
    locals: HashMap<IrIdent, IrType>,
}

/**
 * Basic block is defined by a sequence of instructions and is used
 * to store context information about a particular block scope.
 */
pub struct IrBasicBlock {
    pub prologue_index: usize,
    pub epilogue_index: usize,
    pub enter_label: IrIdent,
    pub exit_label: IrIdent,
    pub return_type: IrType,
    pub func_address: Option<usize>, // used by jitter to call foreign functions
    pub is_foreign: bool,
    pub live_intervals: HashMap<IrIdent, IrLiveInterval>
}

/**
 * Denotes an interval in the IR where a particular variable is alive.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct IrLiveInterval {
    pub begin: usize,
    pub end: usize,
}

/**
 * Three address code instruction, is defined an op code and up to three operands.
 * Span is also used for debugging to retrieve the source location of a given instruction.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct IrInstruction {
    pub opcode: IrOpcode,
    pub op1: IrOperand,
    pub op2: IrOperand,
    pub op3: IrOperand,
    pub ty: IrType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrOpcode {
    Nop,
    Alloca, // op1 = alloca ty
    AllocParams, // (no operands) allocates all defined parameters
    Copy, // op1 = op2
    CopyFromDeref, // op1 = *op2
    CopyFromRef, // op1 = &op2 (these are always mutable refs)
    CopyToDeref, // *op1 = op2
    Clear, // op1 = 0
    Add, // op1 = op2 + op3
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    And,
    Or,
    Xor,
    Lt, // op1 = op2 < op3 (op1 always boolean)
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    IfLt, // jump op3 (if op1 binop op2 equals true)
    IfGt,
    IfLe,
    IfGe,
    IfEq,
    IfNe,
    Jump,     // jump op1
    Label,    // label op1
    Param,    // param op1 (each param are insn. ordered left-to-right)
    Call,     // op1 := op2(...) (with number of parameter stored in op3)
    Return,   // return op1 (where op1 is optional, may be None)
    Prologue, // marks beginning of function, op1 holds the required stack space
    Epilogue, // marks end of function
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrOperand {
    Ident(IrIdent),
    Value(IrValue),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrIdent {
    pub symbol: Symbol,
    pub index: u32, // used to distinguish identifiers with the same symbol.
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
    I64,
    U32,
    U64,
    PtrI8(usize), // NOTE(alexander): argument defines the numbers of indirections
    PtrI32(usize),
    None,
}

pub fn create_ir_builder<'a>() -> IrBuilder<'a> {
    IrBuilder {
        file: None,
        instructions: Vec::new(),
        functions: HashMap::new(),
        addr_size: std::mem::size_of::<usize>() as isize,

        scopes: Vec::new(),
        live_intervals: HashMap::new(),

        register_symbol: intern_string(""),
        register_index: 0,
        basic_block_symbol: intern_string(".bb"),
        basic_block_index: 0,
        if_else_symbol: intern_string(".if_else"),
        if_else_index: 0,
        if_exit_symbol: intern_string(".if_exit"),
        if_exit_index: 0,
        while_enter_symbol: intern_string(".while_enter"),
        while_enter_index: 0,
        while_exit_symbol: intern_string(".while_exit"),
        while_exit_index: 0,
    }
}

pub fn create_ir_ident(symbol: Symbol, index: u32) -> IrIdent {
    IrIdent { symbol, index }
}

fn create_ir_basic_block<'a>(
    ib: &mut IrBuilder<'a>,
    enter: Option<IrIdent>,
    exit: Option<IrIdent>,
    is_foreign: bool
) -> IrBasicBlock {

    fn unique_bb_label<'a>(ib: &mut IrBuilder<'a>) -> IrIdent {
        let label = IrIdent {
            symbol: ib.basic_block_symbol,
            index: ib.basic_block_index,
        };
        ib.basic_block_index += 1;
        return label;
    }

    IrBasicBlock {
        prologue_index: 0,
        epilogue_index: 0,
        enter_label: enter.unwrap_or(unique_bb_label(ib)),
        exit_label: exit.unwrap_or(unique_bb_label(ib)),
        return_type: IrType::None,
        func_address: None,
        is_foreign,
        live_intervals: HashMap::new(),
    }
}

fn create_ir_live_interval(begin: usize) -> IrLiveInterval {
    IrLiveInterval {
        begin,
        end: begin + 1,
    }
}

fn update_ir_live_interval<'a>(ib: &mut IrBuilder<'a>, op: IrOperand) {
    let insn_len = ib.instructions.len();
    if let IrOperand::Ident(ident) = op {
        match ib.live_intervals.get_mut(&ident) {
            Some(live_interval) => live_interval.end = insn_len,
            None => {},
        }
    }
}

#[inline]
pub fn is_alive(insn: usize, interval: IrLiveInterval) -> bool {
    return insn >= interval.begin && insn < interval.end;
}

fn allocate_register<'a>(ib: &mut IrBuilder<'a>) -> IrOperand {
    let ident = IrIdent {
        symbol: ib.register_symbol,
        index: ib.register_index
    };
    ib.register_index += 1;
    ib.live_intervals.insert(ident, create_ir_live_interval(ib.instructions.len()));
    IrOperand::Ident(ident)
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
        IrType::I64 => 8,
        IrType::U32 => 4,
        IrType::U64 => 8,
        IrType::PtrI8(_) |
        IrType::PtrI32(_) => addr_size,
        IrType::None => 0,
    }
}

pub fn build_ir_from_ast<'a>(ib: &mut IrBuilder<'a>, file: &'a File) {
    ib.file = Some(file);

    fn register_ast_items<'a>(ib: &mut IrBuilder<'a>, items: &Vec<Item>) {
        for item in items {
            match item {
                Item::Fn(func) => {
                    let enter_label = create_ir_ident(func.ident.sym, 0);
                    let exit_label = create_ir_ident(func.ident.sym, 1);
                    let ident = create_ir_ident(func.ident.sym, 0);
                    let mut block = create_ir_basic_block(ib, Some(enter_label), Some(exit_label), false);
                    block.return_type = to_ir_type(&func.decl.output);
                    ib.functions.insert(ident, block);
                }

                Item::ForeignFn(func) => {
                    let func_ident = resolve_symbol(func.ident.sym);
                    let func_address = match func_ident {
                        "print_int" => {
                            intrinsics::print_int as *const () as usize
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

                        // NOTE: debug_break - backend level intrinsic
                        "debug_break" => {
                            continue; // dont't generate NULL function address, should be callable
                        }

                        // NOTE: trace - interpreter level intrinsic
                        "trace" => {
                            0usize // should never be called, NULL function address
                        }

                        _ => panic!("unknown foreign function {}", func_ident),
                    };

                    let ident = create_ir_ident(func.ident.sym, 0);
                    let mut block = create_ir_basic_block(ib, None, None, true);
                    block.return_type = to_ir_type(&func.decl.output);
                    block.func_address = Some(func_address);
                    ib.functions.insert(ident, block);
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
            let enter_label = create_ir_ident(func.ident.sym, 0);
            let exit_label = create_ir_ident(func.ident.sym, 1);

            // NOTE(alexander): new function reset register index
            ib.register_index = 0;

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Label,
                op1: IrOperand::Ident(enter_label),
                span: func.decl.span,
                ..Default::default()
            });

            let prologue_index = ib.instructions.len();
            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Prologue,
                ..Default::default()
            });

            // Create the function scope, and setup arguments
            let mut scope = IrScope {
                enter_label: None,
                exit_label: None,
                locals: HashMap::new(),
            };

            for arg in &func.decl.inputs {
                let ty = to_ir_type(&arg.ty);
                let ident = create_ir_ident(arg.ident.sym, 0);
                scope.locals.insert(ident, ty);
                ib.instructions.push(IrInstruction {
                    opcode: IrOpcode::Param,
                    op1: IrOperand::Ident(ident),
                    ty,
                    ..Default::default()
                });
            }

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::AllocParams,
                ..Default::default()
            });

            ib.scopes.push(scope);

            build_ir_from_block(ib, &func.block, Some(enter_label), Some(exit_label), None);

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Label,
                op1: IrOperand::Ident(exit_label),
                span: func.span,
                ..Default::default()
            });

            let epilogue_index = ib.instructions.len();
            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Epilogue,
                ..Default::default()
            });

            let func_label = create_ir_ident(func.ident.sym, 0);
            match ib.functions.get_mut(&func_label) {
                Some(bb) => {
                    bb.prologue_index = prologue_index;
                    bb.epilogue_index = epilogue_index;
                    bb.live_intervals = ib.live_intervals.clone();
                }

                None => panic!("`{}` is not a registered function", func_label),
            }
            ib.live_intervals.clear();

            ib.scopes.pop();
        }
        _ => {}
    }
}

pub fn build_ir_from_block<'a>(
    ib: &mut IrBuilder<'a>,
    block: &Block,
    enter_label: Option<IrIdent>,
    exit_label: Option<IrIdent>,
    assign_op: Option<IrOperand>
) -> (IrOperand, IrType) {

    let scope = IrScope {
        enter_label,
        exit_label,
        locals: HashMap::new(),
    };

    ib.scopes.push(scope);

    let mut last_op = IrOperand::None;
    let mut last_ty = IrType::None;
    for stmt in &block.stmts {
        let (op, ty) = build_ir_from_stmt(ib, &stmt);
        last_op = op;
        last_ty = ty;
    }

    let ret = if let Some(Stmt::Expr(_)) = block.stmts.last() {
        if let IrOperand::None = last_op {
            (IrOperand::None, IrType::None)
        } else {
            if ib.scopes.len() <= 2 { // Outermost scope, safe to return
                ib.instructions.push(IrInstruction {
                    opcode: IrOpcode::Return,
                    op1: last_op,
                    ty: last_ty,
                    span: block.span,
                    ..Default::default()
                });
                (last_op, last_ty)
            } else if let Some(op1) = assign_op { // Not outermost scope, store to register instead
                ib.instructions.push(IrInstruction {
                    opcode: IrOpcode::Copy,
                    op1,
                    op2: last_op,
                    ty: last_ty,
                    span: block.span,
                    ..Default::default()
                });
                (op1, last_ty)
            } else {
                (IrOperand::None, IrType::None)
            }
        }
    } else {
        (IrOperand::None, IrType::None)
    };

    ib.scopes.pop();
    return ret;
}

pub fn build_ir_from_stmt<'a>(ib: &mut IrBuilder<'a>, stmt: &Stmt) -> (IrOperand, IrType) {
    match stmt {
        Stmt::Local(local) => {
            let init_type = to_ir_type(&local.ty);
            let ident = create_ir_ident(local.ident.sym, 0);
            let op1 = IrOperand::Ident(ident);

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Alloca,
                op1,
                ty: init_type,
                span: local.span,
                ..Default::default()
            });

            match &*local.init {
                Some(expr) => {
                    if let Expr::If(if_expr) = expr {
                        build_ir_if_expr(ib, if_expr, Some(op1));
                    } else {
                        let op2 = build_ir_from_expr(ib, expr).0;
                        update_ir_live_interval(ib, op2);
                        ib.instructions.push(IrInstruction {
                            opcode: IrOpcode::Copy,
                            op1,
                            op2,
                            ty: init_type,
                            span: local.span,
                            ..Default::default()
                        });
                    }
                }
                None => return (IrOperand::None, IrType::None)
            };

            ib.scopes[0].locals.insert(ident, init_type);
            ib.live_intervals.insert(ident, create_ir_live_interval(ib.instructions.len()));

            (IrOperand::None, IrType::None)
        }

        Stmt::Item(_)    => (IrOperand::None, IrType::None),
        Stmt::Semi(expr) => (build_ir_from_expr(ib, expr).0, IrType::None),
        Stmt::Expr(expr) => build_ir_from_expr(ib, expr),
    }
}

fn build_ir_conditional_if<'a>(ib: &mut IrBuilder<'a>, cond: &Expr, span: Span, false_target: IrIdent) {

    fn binary_if_condition<'a>(ib: &mut IrBuilder<'a>, cond: &Expr) -> (IrOpcode, IrOperand, IrOperand, IrType) {
        match cond {
            Expr::Binary(binary) => {
                let opcode = match binary.op {
                    BinOp::Lt => IrOpcode::IfGe,
                    BinOp::Gt => IrOpcode::IfLe,
                    BinOp::Le => IrOpcode::IfGt,
                    BinOp::Ge => IrOpcode::IfLt,
                    BinOp::Eq => IrOpcode::IfNe,
                    BinOp::Ne => IrOpcode::IfEq,
                    _ => IrOpcode::Nop,
                };

                if let IrOpcode::Nop = opcode {
                    (IrOpcode::Nop, IrOperand::None, IrOperand::None, IrType::None)
                } else {
                    let (lhs, ty) = build_ir_from_expr(ib, &binary.left);
                    let rhs = build_ir_from_expr(ib, &binary.right).0;
                    (opcode, lhs, rhs, ty)
                }
            }

            Expr::Paren(paren) => binary_if_condition(ib, &paren.expr),
            _ => (IrOpcode::Nop, IrOperand::None, IrOperand::None, IrType::None),
        }
    }

    let (mut opcode, mut op1, mut op2, mut ty) = binary_if_condition(ib, cond);
    let op3 = IrOperand::Ident(false_target);

    if let IrOpcode::Nop = opcode {
        opcode = IrOpcode::IfEq;
        op1 = build_ir_from_expr(ib, cond).0;
        op2 = IrOperand::Value(IrValue::Bool(false));
        ty = IrType::I8;
    }

    ib.instructions.push(IrInstruction {
        opcode,
        op1,
        op2,
        op3,
        ty,
        span: span,
        ..Default::default()
    });
}

fn build_ir_if_expr<'a>(ib: &mut IrBuilder<'a>, if_expr: &ExprIf, assign_op: Option<IrOperand>) {
    let exit_label = create_ir_ident(ib.if_exit_symbol, ib.if_exit_index);
    ib.if_exit_index += 1;
    let else_label = create_ir_ident(ib.if_else_symbol, ib.if_else_index);
    ib.if_else_index += 1;

    let false_label = match if_expr.else_block {
        Some(_) => else_label,
        None    => exit_label,
    };

    build_ir_conditional_if(ib, &*if_expr.cond, if_expr.span, false_label);

    build_ir_from_block(ib, &if_expr.then_block, None, Some(false_label), assign_op);

    if false_label == else_label {
        ib.instructions.push(IrInstruction {
            opcode: IrOpcode::Jump,
            op1: IrOperand::Ident(exit_label),
            ..Default::default()
        });

        ib.instructions.push(IrInstruction {
            opcode: IrOpcode::Label,
            op1: IrOperand::Ident(false_label),
            ..Default::default()
        });
    }

    if let Some(block) = &if_expr.else_block {
        build_ir_from_block(ib, &block, Some(false_label), Some(exit_label), assign_op);
    }

    ib.instructions.push(IrInstruction {
        opcode: IrOpcode::Label,
        op1: IrOperand::Ident(exit_label),
        ..Default::default()
    });
}

pub fn build_ir_from_expr<'a>(ib: &mut IrBuilder<'a>, expr: &Expr) -> (IrOperand, IrType) {
    match expr {
        Expr::Assign(assign) => {
            let mut opcode = IrOpcode::Copy;

            let (op1, ty) = match &*assign.left {
                Expr::Ident(ident) => {
                    let ident = create_ir_ident(ident.sym, 0);
                    let ty = ib.scopes[0].locals.get(&ident).unwrap();
                    (IrOperand::Ident(ident), *ty)
                }

                Expr::Unary(unary) => {
                    if let UnOp::Deref = unary.op {
                        opcode = IrOpcode::CopyToDeref;
                    } else {
                        panic!("expected dereference");
                    }
                    let (op, ty) = build_ir_from_expr(ib, &unary.expr);
                    let ty = match ty {
                        IrType::PtrI8(1) => IrType::I8,
                        IrType::PtrI32(1) => IrType::I32,
                        IrType::PtrI8(x) => IrType::PtrI8(x - 1),
                        IrType::PtrI32(x) => IrType::PtrI32(x - 1),
                        _ => panic!("cannot dereference non-pointer type"),
                    };
                    (op, ty)
                }

                _ => panic!("expected identifier or dereference"),
            };

            if let Expr::If(if_expr) = &*assign.right {
                build_ir_if_expr(ib, if_expr, Some(op1));
            } else {
                let op2 = build_ir_from_expr(ib, &assign.right).0;
                update_ir_live_interval(ib, op1);
                update_ir_live_interval(ib, op2);
                ib.instructions.push(IrInstruction {
                    opcode,
                    op1,
                    op2,
                    ty,
                    span: assign.span,
                    ..Default::default()
                });
            };

            (op1, ty)
        }

        Expr::Binary(binary) => {
            let op1 = allocate_register(ib);
            let (op2, lhs_ty) = build_ir_from_expr(ib, &binary.left);
            let op3 = build_ir_from_expr(ib, &binary.right).0;
            let (opcode, ty) = match binary.op {
                BinOp::Add => (IrOpcode::Add, IrType::I32),
                BinOp::Sub => (IrOpcode::Sub, IrType::I32),
                BinOp::Mul => (IrOpcode::Mul, IrType::I32),
                BinOp::Div => (IrOpcode::Div, IrType::I32),
                BinOp::Pow => (IrOpcode::Pow, IrType::I32),
                BinOp::Mod => (IrOpcode::Mod, IrType::I32),
                BinOp::And => (IrOpcode::And, IrType::I8),
                BinOp::Or  => (IrOpcode::Or,  IrType::I8),
                BinOp::Eq  => (IrOpcode::Eq,  lhs_ty),
                BinOp::Ne  => (IrOpcode::Ne,  lhs_ty),
                BinOp::Lt  => (IrOpcode::Lt,  lhs_ty),
                BinOp::Le  => (IrOpcode::Le,  lhs_ty),
                BinOp::Gt  => (IrOpcode::Gt,  lhs_ty),
                BinOp::Ge  => (IrOpcode::Ge,  lhs_ty),
            };

            update_ir_live_interval(ib, op2);
            update_ir_live_interval(ib, op3);

            ib.instructions.push(IrInstruction {
                opcode,
                op1,
                op2,
                op3,
                ty,
                span: binary.span,
            });

            (op1, ty)
        }

        Expr::Block(block) => build_ir_from_block(ib, &block.block, None, None, None),

        Expr::Break(_) |
        Expr::Continue(_) => {
            for scope in ib.scopes.iter().rev() {
                if let Some(enter_label) = scope.enter_label {
                    if ib.while_enter_symbol == enter_label.symbol {
                        if let Expr::Continue(cont_expr) = expr {
                            ib.instructions.push(IrInstruction {
                                opcode: IrOpcode::Jump,
                                op1: IrOperand::Ident(enter_label),
                                span: cont_expr.span,
                                ..Default::default()
                            });
                        } else if let Expr::Break(break_expr) = expr {
                            ib.instructions.push(IrInstruction {
                                opcode: IrOpcode::Jump,
                                op1: IrOperand::Ident(scope.exit_label.unwrap()),
                                span: break_expr.span,
                                ..Default::default()
                            });
                        }
                        break;
                    }
                }
            }

            (IrOperand::None, IrType::None)
        }

        Expr::Call(call) => {
            // Setup parameters
            let mut param_size = 0;
            let call_insn_pos = ib.instructions.len() + call.args.len();
            for arg in &call.args {
                let (op1, ty) = build_ir_from_expr(ib, &arg);

                // NOTE(alexander): update lifetime to include the call instruction also
                if let IrOperand::Ident(ident) = op1 {
                    match ib.live_intervals.get_mut(&ident) {
                        Some(live_interval) => live_interval.end = call_insn_pos,
                        None => {},
                    }
                }
                
                ib.instructions.push(IrInstruction {
                    opcode: IrOpcode::Param,
                    op1,
                    ty,
                    span: arg.get_span(),
                    ..Default::default()
                });

                param_size += 1;
            }

            // Make the function call
            let function_label = create_ir_ident(call.ident.sym, 0);
            let op1 = allocate_register(ib);
            let (op2, return_type) = match ib.functions.get(&function_label) {
                Some(bb) => {
                    if let Some(func_address) = bb.func_address {
                        if ib.addr_size == 4 {
                            (IrOperand::Value(IrValue::U32(func_address as u32)), bb.return_type)
                        } else if ib.addr_size == 8 {
                            (IrOperand::Value(IrValue::U64(func_address as u64)), bb.return_type)
                        } else {
                            panic!("unsupported address size: `{}-bit`, expected 32- or 64-bit", ib.addr_size*8);
                        }
                    } else {
                        (IrOperand::Ident(function_label), bb.return_type)
                    }
                }

                None => (IrOperand::Ident(function_label), IrType::None),
            };

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Call,
                op1,
                op2,
                op3: IrOperand::Value(IrValue::I32(param_size)),
                ty: return_type,
                span: call.span,
                ..Default::default()
            });

            (op1, return_type)
        }

        Expr::Ident(ident) => {
            let ident = create_ir_ident(ident.sym, 0);
            let op = IrOperand::Ident(ident);
            update_ir_live_interval(ib, op);
            let ty = ib.scopes[0].locals.get(&ident).unwrap(); // TODO(alexander): should we not search all in scopes
            (op, *ty)
        }

        Expr::If(if_expr) => {
            build_ir_if_expr(ib, if_expr, None);
            (IrOperand::None, IrType::None)
        }

        Expr::Lit(literal) => match literal.lit {
            Lit::Int(val)  => (IrOperand::Value(IrValue::I32(val)), IrType::I32),
            Lit::Bool(val) => (IrOperand::Value(IrValue::Bool(val)), IrType::I8),
        }

        Expr::Paren(paren) => build_ir_from_expr(ib, &paren.expr),

        Expr::Reference(reference) => {
            let (op2, ty) = build_ir_from_expr(ib, &reference.expr);
            let op1 = allocate_register(ib);

            let ref_ty = match ty {
                IrType::I8        => IrType::PtrI8(1),
                IrType::I32       => IrType::PtrI32(1),
                IrType::PtrI8(i)  => IrType::PtrI8(i + 1),
                IrType::PtrI32(i) => IrType::PtrI32(i + 1),
                IrType::None      => panic!("missing type info"),
                _                 => panic!("unsupported type"),
            };

            update_ir_live_interval(ib, op2);

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::CopyFromRef,
                op1,
                op2,
                ty: ref_ty,
                span: reference.span,
                ..Default::default()
            });

            (op1, ref_ty)
        }

        Expr::Return(return_expr) => {
            let (op1, ty) = match &*return_expr.expr {
                Some(expr) => build_ir_from_expr(ib, expr),
                None => (IrOperand::None, IrType::None),
            };

            update_ir_live_interval(ib, op1);

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Return,
                op1,
                ty,
                span: return_expr.span,
                ..Default::default()
            });

            (IrOperand::None, IrType::None)
        }

        Expr::Unary(unary) => {
            match unary.op {
                UnOp::Neg => {
                    let (op2, ty) = build_ir_from_expr(ib, &unary.expr);
                    let op1 = allocate_register(ib);

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpcode::Clear,
                        op1,
                        ty,
                        ..Default::default()
                    });

                    update_ir_live_interval(ib, op1);
                    update_ir_live_interval(ib, op2);

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpcode::Sub,
                        op1,
                        op2,
                        ty,
                        span: unary.span,
                        ..Default::default()
                    });

                    (op1, ty)
                },

                UnOp::Not => {
                    let (op1, ty) = build_ir_from_expr(ib, &unary.expr);

                    update_ir_live_interval(ib, op1);
                    ib.instructions.push(IrInstruction {
                        opcode: IrOpcode::Xor,
                        op1,
                        op2: IrOperand::Value(IrValue::Bool(true)),
                        ty,
                        span: unary.span,
                        ..Default::default()
                    });

                    (op1, ty)
                },

                UnOp::Deref => {
                    let (op2, op2_ty) = build_ir_from_expr(ib, &unary.expr);
                    let op1 = allocate_register(ib);

                    let ty = match op2_ty {
                        IrType::I8  |
                        IrType::I32 |
                        IrType::I64 |
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

                    update_ir_live_interval(ib, op2);

                    ib.instructions.push(IrInstruction {
                        opcode: IrOpcode::CopyFromDeref,
                        op1,
                        op2,
                        ty,
                        span: unary.span,
                        ..Default::default()
                    });

                    (op1, ty)
                }
            }
        }

        Expr::While(while_expr) => {
            let enter_label = create_ir_ident(ib.while_enter_symbol, ib.while_enter_index);
            let exit_label = create_ir_ident(ib.while_exit_symbol, ib.while_exit_index);
            ib.while_enter_index += 1;
            ib.while_exit_index += 1;

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Label,
                op1: IrOperand::Ident(enter_label),
                ..Default::default()
            });

            build_ir_conditional_if(ib, &*while_expr.cond, while_expr.span, exit_label);

            build_ir_from_block(ib, &while_expr.block, Some(enter_label), Some(exit_label), None);

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Jump,
                op1: IrOperand::Ident(enter_label),
                ..Default::default()
            });

            ib.instructions.push(IrInstruction {
                opcode: IrOpcode::Label,
                op1: IrOperand::Ident(exit_label),
                ..Default::default()
            });

            (IrOperand::None, IrType::None)
        }
    }
}

impl Default for IrInstruction {
    fn default() -> Self {
        IrInstruction {
            opcode: IrOpcode::Nop,
            op1: IrOperand::None,
            op2: IrOperand::None,
            op3: IrOperand::None,
            ty: IrType::None,
            span: Span::new(),
        }
    }
}

impl fmt::Display for IrBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for insn in &self.instructions {
            match insn.opcode {
                IrOpcode::Label => if let IrOperand::Ident(ident) = insn.op1 {
                    write!(f, "{}:\n", ident)?;
                } else {
                    write!(f, "{}:\n", insn.op1)?;
                }

                _ => write!(f, "    {}\n", insn)?,
            }
        }
        Ok(())
    }
}

impl fmt::Display for IrInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.opcode {
            IrOpcode::Nop         |
            IrOpcode::AllocParams |
            IrOpcode::IfLt        |
            IrOpcode::IfGt        |
            IrOpcode::IfLe        |
            IrOpcode::IfGe        |
            IrOpcode::IfEq        |
            IrOpcode::IfNe        |
            IrOpcode::Param       |
            IrOpcode::Return      |
            IrOpcode::Label       |
            IrOpcode::Jump        |
            IrOpcode::Prologue    |
            IrOpcode::Epilogue    => {
                write!(f, "{}", self.opcode)?;
                if let IrType::None = self.ty {
                } else {
                    write!(f, " {}", self.ty)?;
                }
                write!(f, " {}", self.op1)?;
                if let IrOperand::None = self.op2 {
                } else {
                    write!(f, ", {}", self.op2)?;
                    if let IrOperand::None = self.op3 {
                    } else {
                        write!(f, ", {}", self.op3)?;
                    }
                }
            }

            _ => {
                write!(f, "{} = {}", self.op1, self.opcode)?;
                if let IrType::None = self.ty {
                } else {
                    write!(f, " {}", self.ty)?;
                }
                if let IrOperand::None = self.op2 {
                } else {
                    write!(f, " {}", self.op2)?;
                    if let IrOperand::None = self.op3 {
                    } else {
                        write!(f, ", {}", self.op3)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::I8        => write!(f, "i8"),
            IrType::I32       => write!(f, "i32"),
            IrType::I64       => write!(f, "i64"),
            IrType::U32       => write!(f, "u32"),
            IrType::U64       => write!(f, "u64"),
            IrType::PtrI8(i)  => write!(f, "i8{}", "*".repeat(*i as usize)),
            IrType::PtrI32(i) => write!(f, "i32{}", "*".repeat(*i as usize)),
            IrType::None      => write!(f, ""),
        }
    }
}

impl fmt::Display for IrIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = resolve_symbol(self.symbol);
        if self.index > 0 || s.len() == 0  {
            write!(f, "{}{}", s, self.index)
        } else {
            write!(f, "{}", s)
        }
    }
}

impl fmt::Display for IrOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            IrOperand::Ident(label) => write!(f, "%{}", label),
            IrOperand::Value(val) => match val {
                IrValue::I32(v) => write!(f, "{}", v),
                IrValue::U32(v) => write!(f, "{}", v),
                IrValue::U64(v) => write!(f, "{}", v),
                IrValue::Bool(v) => write!(f, "{}", v),
            }
            IrOperand::None => write!(f, ""),
        }
    }
}

impl fmt::Display for IrOpcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrOpcode::Nop           => write!(f, "nop"),
            IrOpcode::Alloca        => write!(f, "alloca"),
            IrOpcode::AllocParams   => write!(f, "alloc_params"),
            IrOpcode::Copy          => write!(f, "copy"),
            IrOpcode::CopyFromRef   => write!(f, "copy_from_ref"),
            IrOpcode::CopyFromDeref => write!(f, "copy_from_deref"),
            IrOpcode::CopyToDeref   => write!(f, "copy_to_deref"),
            IrOpcode::Clear         => write!(f, "clear"),
            IrOpcode::Add           => write!(f, "add"),
            IrOpcode::Sub           => write!(f, "sub"),
            IrOpcode::Mul           => write!(f, "mul"),
            IrOpcode::Div           => write!(f, "div"),
            IrOpcode::Pow           => write!(f, "pow"),
            IrOpcode::Mod           => write!(f, "mod"),
            IrOpcode::And           => write!(f, "and"),
            IrOpcode::Or            => write!(f, "or"),
            IrOpcode::Xor           => write!(f, "xor"),
            IrOpcode::Eq            => write!(f, "eq"),
            IrOpcode::Ne            => write!(f, "ne"),
            IrOpcode::Lt            => write!(f, "lt"),
            IrOpcode::Le            => write!(f, "le"),
            IrOpcode::Gt            => write!(f, "gt"),
            IrOpcode::Ge            => write!(f, "ge"),
            IrOpcode::IfLt          => write!(f, "iflt"),
            IrOpcode::IfGt          => write!(f, "ifgt"),
            IrOpcode::IfLe          => write!(f, "ifle"),
            IrOpcode::IfGe          => write!(f, "ifge"),
            IrOpcode::IfEq          => write!(f, "ifeq"),
            IrOpcode::IfNe          => write!(f, "ifne"),
            IrOpcode::Param         => write!(f, "param"),
            IrOpcode::Call          => write!(f, "call"),
            IrOpcode::Return        => write!(f, "return"),
            IrOpcode::Label         => write!(f, "label"),
            IrOpcode::Jump          => write!(f, "jump"),
            IrOpcode::Prologue      => write!(f, "prologue"),
            IrOpcode::Epilogue      => write!(f, "epilogue\n"),
        }
    }
}
