use std::cmp::min;
use std::collections::HashMap;
use crate::ast::*;
use crate::error::*;

pub struct TypeTable {
    types: HashMap<Symbol, Ty>,
    inside_loop: bool,
}

pub struct TypeContext<'a> {
    pub file: Option<&'a File>,
    pub locals: Vec<TypeTable>, // based on the call stack
    pub items: HashMap<Symbol, &'a Item>,
    pub current_item: Option<&'a Item>,
    pub error_count: u32,
}

pub fn create_type_context<'a>() -> TypeContext<'a> {
    TypeContext {
        file: None,
        locals: Vec::new(),
        items: HashMap::new(),
        current_item: None,
        error_count: 0,
    }
}

pub fn create_type_table(inside_loop: bool) -> TypeTable {
    TypeTable {
        types: HashMap::new(),
        inside_loop,
    }
}

fn is_inside_loop<'a>(tc: &mut TypeContext<'a>) -> bool {
    for table in &tc.locals {
        if table.inside_loop {
            return true;
        }
    }
    false
}

fn store_global_items<'a>(tc: &mut TypeContext<'a>, items: &'a Vec<Item>) {
    for item in items {
        let symbol = match item {
            Item::Fn(func) => func.ident.sym,
            Item::ForeignFn(func) => func.ident.sym,
            Item::ForeignMod(module) => {
                store_global_items(tc, &module.items);
                continue;
            }
        };

        tc.items.insert(symbol, &item);
    }
}

pub fn type_check_file<'a>(tc: &mut TypeContext<'a>, file: &'a File) {
    tc.file = Some(file);
    store_global_items(tc, &file.items);

    for item in &file.items {
        type_check_item(tc, item);
    }
}

pub fn type_check_item<'a>(tc: &mut TypeContext<'a>, item: &'a Item) {
    tc.current_item = Some(item);
    match item {
        Item::Fn(func) => { type_check_function(tc, func); },
        _ => { },
    };
}

pub fn type_check_function<'a>(tc: &mut TypeContext<'a>, func: &'a FnItem) -> Ty {
    let ret_ty = type_check_block(tc, &func.block, false);

    if func.decl.output != ret_ty {
        if ret_ty.is_none() {
            let mut msg = create_error_msg(
                tc,
                ErrorLevel::Error,
                func.decl.output.span,
                "mismatched types",
                &format!("expected {} found ()", func.decl.output));

            if ret_ty.span.is_empty()  {
                msg.next = Some(Box::new(create_error_msg(
                    tc,
                    ErrorLevel::Note,
                    func.ident.span,
                    "",
                    "implicitly returns `()` as its body has no tail or `return` expression")));

            } else {
                msg.next = Some(Box::new(create_error_msg(
                    tc,
                    ErrorLevel::Note,
                    ret_ty.span,
                    "",
                    "return type is not ()")));
            }

            print_error_msg(&msg);
            tc.error_count += 1;
        } else {
            // TODO(alexander): check empty return type
            mismatched_types_error(tc, ret_ty.span, &func.decl.output.kind, &ret_ty);
        }
    }

    Ty::default()
}

pub fn type_check_block<'a>(tc: &mut TypeContext<'a>, block: &'a Block, inside_loop: bool) -> Ty {
    tc.locals.push(create_type_table(inside_loop));

    let mut ret_ty = Ty::default();
    for i in 0..block.stmts.len() {
        ret_ty = type_check_stmt(tc, &block.stmts[i]);

        let is_return = match block.stmts[i] {
            Stmt::Semi(ref expr) => match expr {
                Expr::Return(_) => true,
                _ => false,
            },
            Stmt::Expr(_) => true,
            _ => false,
        };

        if !ret_ty.is_none() && !is_return {
            if i < block.stmts.len() - 1 {
                mismatched_types_error(tc, ret_ty.span, &TyKind::None, &ret_ty);
            }
        }
    }

    tc.locals.pop();
    ret_ty
}

pub fn type_check_stmt<'a>(tc: &mut TypeContext<'a>, stmt: &'a Stmt) -> Ty {
    match stmt {
        Stmt::Local(local) => {
            let (ty, first_assigned_span) = match &*local.init {
                Some(init) => {
                    let init_ty = type_check_expr(tc, init);
                    if init_ty != local.ty {
                        mismatched_types_error(tc, init_ty.span, &local.ty.kind, &init_ty);
                    }
                    (init_ty, local.span)
                },

                None => {
                    match local.ty.kind {
                        TyKind::None => {
                            type_error(
                                tc,
                                local.ty.span,
                                &format!("missing type annotation"),
                                &format!("give `{}` a type", resolve_symbol(local.ident.sym)));
                        }

                        _ => {},
                    };
                    (local.ty.clone(), Span::new())
                }
            };

            if tc.locals.len() > 0 {
                let len = tc.locals.len();
                let ty_info = Ty {
                    kind: ty.kind,
                    mutable: local.mutable,
                    assigned: (*local.init).is_some(),
                    span: local.span,
                    sym: Some(local.ident.sym),
                    first_declared_span: local.ident.span,
                    first_assigned_span,
                };
                tc.locals[len - 1].types.insert(local.ident.sym, ty_info);
            }

            Ty::default()
        }

        Stmt::Item(item) => {
            type_check_item(tc, &item);
            Ty::default()
        }

        Stmt::Semi(expr) => {
            let ret_ty = type_check_expr(tc, expr);
            if let Expr::Return(_) = expr {
                ret_ty
            } else {
                Ty::default()
            }
        },

        Stmt::Expr(expr) => type_check_expr(tc, expr),
    }
}

pub fn type_check_expr<'a>(tc: &mut TypeContext<'a>, expr: &'a Expr) -> Ty {
    match expr {
        Expr::Assign    (e) => type_check_assign_expr(tc, e),
        Expr::Binary    (e) => type_check_binary_expr(tc, e),
        Expr::Block     (e) => type_check_block(tc, &e.block, false),
        Expr::Call      (e) => type_check_call_expr(tc, e),
        Expr::Ident     (e) => type_check_ident_expr(tc, e),
        Expr::If        (e) => type_check_if_expr(tc, e),
        Expr::Lit       (e) => type_check_literal_expr(e),
        Expr::Paren     (e) => type_check_expr(tc, &e.expr),
        Expr::Reference (e) => type_check_reference_expr(tc, e),
        Expr::Return    (e) => type_check_return_expr(tc, e),
        Expr::Unary     (e) => type_check_unary_expr(tc, e),
        Expr::While     (e) => type_check_while_expr(tc, e),

        Expr::Break(e) => {
            if !is_inside_loop(tc) {
                type_error(tc, e.span, "cannot break outside loop", "help: remove this or move it inside a loop");
            }
            Ty::default()
        }
        
        Expr::Continue(e) => {
            if !is_inside_loop(tc) {
                type_error(tc, e.span, "cannot continue outside loop", "help: remove this or move it inside a loop");
            }
            Ty::default()
        }
    }
}

pub fn type_check_assign_expr<'a>(tc: &mut TypeContext<'a>, assign_expr: &'a ExprAssign) -> Ty {

    fn type_check_locator_expr<'a>(tc: &mut TypeContext<'a>, expr: &'a Expr, span: Span) -> Ty {
        match expr {
            Expr::Ident(ident) => {
                for table in tc.locals.iter().rev() {
                    if let Some(ty) = table.types.get(&ident.sym) {
                        return ty.clone();
                    }
                }
            }

            Expr::Unary(unary_expr) => {
                if let UnOp::Deref = unary_expr.op {
                    let ty = type_check_locator_expr(tc, &unary_expr.expr, span);
                    if let TyKind::Ref(ty_ref) = ty.kind {
                        if !ty_ref.mutable {
                            type_error(tc,
                                       span,
                                       "cannot assign through an `&` immutable reference",
                                       "help: change to `&mut` mutable reference")
                        }
                        return *ty_ref.elem;
                    } else {
                        type_error(tc,
                                   unary_expr.span,
                                   &format!("expected `reference` found `{}`", &ty),
                                   "help: remove the dereference `*`");
                    }
                }
            }

            _ => {
                type_error(tc,
                           span,
                           "invalid left-hand side of assignment",
                           "cannot assign to this expression");
            }
        };

        Ty::default()
    }

    let lhs_ty = type_check_locator_expr(tc, &assign_expr.left, assign_expr.span);
    let rhs_ty = type_check_expr(tc, &assign_expr.right);

    let inside_loop = is_inside_loop(tc);
    if (lhs_ty.assigned || inside_loop) && !lhs_ty.mutable {
        let var_str = resolve_symbol(lhs_ty.sym.unwrap());
        let mut msg = create_error_msg(
            tc,
            ErrorLevel::Error,
            assign_expr.span,
            &format!("cannot assign twice to immutable variable `{}`", var_str),
            "cannot assign twice to immutable variable",
        );

        if !lhs_ty.first_declared_span.is_empty() {
            let mut msg_decl = create_error_msg(
                tc,
                ErrorLevel::Note,
                lhs_ty.first_declared_span,
                "",
                &format!("help: make variable mutable `mut {}`", var_str),
            );

            let is_assigned_elsewhere = lhs_ty.first_assigned_span.is_inside(lhs_ty.first_declared_span);
            if !lhs_ty.first_assigned_span.is_empty() && is_assigned_elsewhere {
                let msg_assigned = create_error_msg(
                    tc,
                    ErrorLevel::Note,
                    lhs_ty.first_assigned_span,
                    "",
                    "variable is first assigned here",
                );

                msg_decl.next = Some(Box::new(msg_assigned));
            }
            
            msg.next = Some(Box::new(msg_decl));            
        }

        print_error_msg(&msg);
        tc.error_count += 1;
    }

    if !lhs_ty.assigned {
        if let Some(sym) = lhs_ty.sym {
            for table in tc.locals.iter_mut().rev() {
                if let Some(ty) = table.types.get_mut(&sym) {
                    ty.assigned = true;
                    ty.first_assigned_span = assign_expr.span;
                }
            }
        }
    }

    if lhs_ty != rhs_ty {
        mismatched_types_error(tc, assign_expr.span, &lhs_ty.kind, &rhs_ty);
    }

    Ty::default()
}

pub fn type_check_binary_expr<'a>(tc: &mut TypeContext<'a>, binary_expr: &'a ExprBinary) -> Ty {
    let lhs_ty = type_check_expr(tc, &binary_expr.left);
    let rhs_ty = type_check_expr(tc, &binary_expr.right);
    let ok = match binary_expr.op {
        BinOp::Add => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Sub => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Mul => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Div => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Pow => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Mod => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::And => lhs_ty.is_bool() && rhs_ty.is_bool(),
        BinOp::Or  => lhs_ty.is_bool() && rhs_ty.is_bool(),
        BinOp::Eq  => lhs_ty == rhs_ty,
        BinOp::Ne  => lhs_ty == rhs_ty,
        BinOp::Lt  => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Le  => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Gt  => lhs_ty.is_int() && rhs_ty.is_int(),
        BinOp::Ge  => lhs_ty.is_int() && rhs_ty.is_int(),
    };

    if !ok {
        type_error(
            tc,
            Span::combine(lhs_ty.span, rhs_ty.span),
            &format!("cannot {} `{}` to `{}`", binary_expr.op, lhs_ty, rhs_ty),
            &format!("no implementation for `{} {} {}`", lhs_ty, binary_expr.op.token(), rhs_ty));
    }

    // infer the type of resulting value
    match binary_expr.op {
        BinOp::Eq |
        BinOp::Ne |
        BinOp::Lt |
        BinOp::Le |
        BinOp::Gt |
        BinOp::Ge => Ty::new(TyKind::Bool, Span::combine(lhs_ty.span, rhs_ty.span)),

        _ => Ty::new(lhs_ty.kind.clone(), Span::combine(lhs_ty.span, rhs_ty.span)),
    }
}

pub fn type_check_call_expr<'a>(tc: &mut TypeContext<'a>, call: &'a ExprCall) -> Ty {
    let fn_decl = match tc.items.get(&call.ident.sym) {
        Some(item) => match item {
            Item::Fn(func) => &func.decl,
            Item::ForeignFn(func) => &func.decl,
            _ => panic!("compiler bug"),
        }

        None => {
            type_error(
                tc,
                call.ident.span,
                &format!("cannot find function `{}` in this scope", resolve_symbol(call.ident.sym)),
                "not found in this scope");
            return Ty::default();
        }
    };

    if call.args.len() != fn_decl.inputs.len() {
        type_error(
            tc,
            call.ident.span,
            &format!("function takes in {} arguments but {} arguments were supplied",
                    fn_decl.inputs.len(), call.args.len()),
            &format!("expected {} argument", fn_decl.inputs.len()));
    }

    // type check each input argument
    let mut arg_types = Vec::new();
    for i in 0..min(call.args.len(), fn_decl.inputs.len()) {
        let ty = type_check_expr(tc, &call.args[i]);
        arg_types.push(ty);
    }

    for i in 0..min(arg_types.len(), fn_decl.inputs.len()) {
        if fn_decl.inputs[i].ty != arg_types[i] {
            mismatched_types_error(tc, arg_types[i].span, &fn_decl.inputs[i].ty.kind, &arg_types[i]);
        }
    }

    let mut out_ty = fn_decl.output.clone();
    out_ty.span = call.span;
    out_ty
}

pub fn type_check_ident_expr<'a>(tc: &mut TypeContext<'a>, ident: &'a ExprIdent) -> Ty {
    for table in tc.locals.iter_mut().rev() {
        if let Some(ty) = table.types.get_mut(&ident.sym) {
            let mut ty_ident = ty.clone();
            if !ty.assigned {
                ty.assigned = true; // NOTE(alexander): ignore any future errors of this kind.
                type_error(tc,
                           ident.span,
                           &format!("use of possibly-uninitialized variable `{}`", resolve_symbol(ident.sym)),
                           "");
            }
            ty_ident.span = ident.span;
            return ty_ident;
        }
    }

    type_error(tc,
               ident.span,
               &format!("cannot find value `{}` in this scope", resolve_symbol(ident.sym)),
               "not found in this scope");
    let mut ty = Ty::default();
    ty.kind = TyKind::Error;
    ty
}

pub fn type_check_if_expr<'a>(tc: &mut TypeContext<'a>, if_expr: &'a ExprIf) -> Ty {
    let cond_type = type_check_expr(tc, &if_expr.cond);
    if cond_type.kind != TyKind::Bool {
        mismatched_types_error(tc, cond_type.span, &TyKind::Bool, &cond_type);
    }
    let then_ty = type_check_block(tc, &if_expr.then_block, false);
    match &if_expr.else_block {
        Some(block) => {
            let else_ty = type_check_block(tc, block, false);
            if then_ty != else_ty {
                mismatched_types_error(tc, else_ty.span, &then_ty.kind, &else_ty);
            }
        },
        None => { },
    };
    then_ty
}

pub fn type_check_literal_expr<'a>(literal: &'a ExprLit) -> Ty {
    match literal.lit {
        Lit::Int(_) => Ty::new(TyKind::Int, literal.span),
        Lit::Bool(_) => Ty::new(TyKind::Bool, literal.span),
    }
}

pub fn type_check_reference_expr<'a>(tc: &mut TypeContext<'a>, reference_expr: &'a ExprReference) -> Ty {
    let ty = type_check_expr(tc, &reference_expr.expr);
    if ty.is_none() {
        type_error(
            tc,
            reference_expr.span,
            "cannot reference none type",
            "expected a typed value here, got nothing");
        Ty::default()
    } else {
        Ty {
            span: reference_expr.span,
            kind: TyKind::Ref(TypeRef {
                mutable: reference_expr.mutable,
                elem: Box::new(ty),
            }),
            ..Default::default()
        }
    }
}

pub fn type_check_return_expr<'a>(tc: &mut TypeContext<'a>, return_expr: &'a ExprReturn) -> Ty {
    let ret_ty = match &*return_expr.expr {
        Some(expr) => type_check_expr(tc, &expr),
        None => {
            let mut ret_ty = Ty::default();
            ret_ty.span = return_expr.span;
            ret_ty
        }
    };
    let actual_ret_ty = match tc.current_item {
        Some(item) => match item {
            Item::Fn(func) => &func.decl.output,
            Item::ForeignFn(func) => &func.decl.output,
            _ => panic!("compiler bug"),
        }
        None => panic!("compiler bug: not analysing any function"),
    };

    if *actual_ret_ty != ret_ty {
        if let TyKind::Error = ret_ty.kind {
        } else {
            mismatched_types_error(tc, return_expr.span, &actual_ret_ty.kind, &ret_ty);
        }

        // TODO(alexander): check empty return type
        actual_ret_ty.clone()
    } else {
        ret_ty
    }
}

pub fn type_check_unary_expr<'a>(tc: &mut TypeContext<'a>, unary_expr: &'a ExprUnary) -> Ty {
    let mut ty = type_check_expr(tc, &unary_expr.expr);
    let ok = match unary_expr.op {
        UnOp::Neg   => ty.kind == TyKind::Int,
        UnOp::Not   => ty.kind == TyKind::Bool,
        UnOp::Deref => match ty.get_ref() {
            Some(r) => {
                ty = (*r.elem).clone();
                true
            },
            
            None => false,
        }
    };

    if !ok {
        type_error(
            tc,
            unary_expr.span,
            &format!("type `{}` cannot be {}", ty, unary_expr.op),
            &format!("no implementation for `{}{}`", unary_expr.op.token(), ty));
    }

    ty.span = unary_expr.span;
    ty
}

fn type_check_while_expr<'a>(tc: &mut TypeContext<'a>, while_expr: &'a ExprWhile) -> Ty {
    let cond_type = type_check_expr(tc, &while_expr.cond);
    if cond_type.kind != TyKind::Bool {
        mismatched_types_error(tc, cond_type.span, &TyKind::Bool, &cond_type);
    }
    type_check_block(tc, &while_expr.block, true);
    Ty::default()
}

fn create_error_msg<'a>(
    tc: &TypeContext<'a>,
    level: ErrorLevel,
    span: Span,
    message: &str,
    label: &str
) -> ErrorMsg {
    match tc.file {
        Some(file) => create_error_msg_from_span(level,
                                                 &file.lines,
                                                 span,
                                                 &file.filename,
                                                 &file.source,
                                                 message,
                                                 label),

        None => ErrorMsg {
            level: level,
            line_number: 0,
            column_number: 0,
            annotation_length: 0,
            path: "".to_string(),
            msg: message.to_string(),
            source: "".to_string(),
            label: label.to_string(),
            next: None,
        },
    }
}

fn type_error<'a>(tc: &mut TypeContext<'a>, span: Span, message: &str, label: &str) {
    let msg = create_error_msg(
        tc,
        ErrorLevel::Error,
        span,
        message,
        label
    );
    print_error_msg(&msg);
    tc.error_count += 1;
}

fn mismatched_types_error<'a>(tc: &mut TypeContext<'a>, span: Span, expected: &TyKind, found: &Ty) {
    let msg = create_error_msg(
        tc,
        ErrorLevel::Error,
        span,
        &format!("expected `{}`, found `{}`", expected, found),
        ""
    );
    print_error_msg(&msg);
    tc.error_count += 1;
}
