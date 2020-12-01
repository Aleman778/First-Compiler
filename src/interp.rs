#![allow(dead_code)]

use std::fmt;
use std::collections::HashMap;
use crate::ast::*;
use crate::intrinsics::*;
use crate::error::*;

pub type IResult<T> = Result<T, ErrorMsg>;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Ref(Reference),
    Void, // signal that there is no value
    None, // used if value is not applicable
}

#[derive(Debug, Clone)]
pub struct Reference {
    pub addr: usize,
    pub ref_ty: Ty,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct InterpValue {
    pub data: Value,
    pub span: Span,
    pub mutable: bool,
    pub from_return: bool,
    pub should_continue: bool,
    pub should_break: bool,
}

pub fn create_interp_value(data: Value, span: Span, mutable: bool) -> InterpValue {
    InterpValue {
        data,
        span,
        mutable,
        from_return: false,
        should_continue: false,
        should_break: false,
    }
}

pub fn empty_interp_value() -> InterpValue {
    create_interp_value(Value::None, Span::new(), false)
} 

pub fn is_value_empty(value: &Value) -> bool {
    match value {
        Value::Void |
        Value::None => true,
        _ => false,
    }
}

pub fn to_type_kind(value: &Value) -> TyKind {
    match value {
        Value::Int(_) => TyKind::Int,
        Value::Bool(_) => TyKind::Bool,
        Value::Ref(r) => TyKind::Ref(
            TypeRef {
                mutable: r.mutable,
                elem: Box::new(r.ref_ty.clone()),
            }
        ),
        _ => TyKind::None,
    }
}

pub fn to_type(value: &InterpValue) -> Ty {
    let ty_kind = to_type_kind(&value.data);
    Ty { kind: ty_kind, span: value.span }
}

#[derive(Clone)]
pub struct InterpScope {
    pub entities: HashMap<String, usize>,
    pub span: Span,
    pub is_block_scope: bool,
}

pub fn create_interp_scope(span: Span, is_block_scope: bool) -> InterpScope {
    InterpScope {
        entities: HashMap::new(),
        span,
        is_block_scope,
    }
}

pub struct InterpContext<'a> {
    pub file:          Option<&'a File>,
    pub signatures:    HashMap<String, &'a Item>,
    pub call_stack:    Vec<InterpScope>,
    pub stack:         Vec<InterpValue>,
    pub stack_pointer: usize,
    pub base_pointer:  usize,
}

pub fn create_interp_context<'a>() -> InterpContext<'a> {
    InterpContext {
        file: None,
        signatures: HashMap::new(),
        call_stack: Vec::new(),
        stack: Vec::with_capacity(1000),
        stack_pointer: 0,
        base_pointer: 0,
    }
}

fn store_local_variable<'a>(ic: &mut InterpContext<'a>, value: InterpValue, symbol: Option<String>) -> usize {
    if let Some(id) = symbol {
        if ic.call_stack.len() > 0 {
            let len = ic.call_stack.len();
            ic.call_stack[len - 1].entities.insert(id, ic.stack_pointer);
        }
    }
    if ic.stack_pointer >= ic.stack.len() {
        ic.stack.push(value);
    } else {
        ic.stack[ic.stack_pointer] = value;
    }
    ic.stack_pointer += 1;
    return ic.stack_pointer - 1;
}

fn find_local_variable<'a>(ic: &mut InterpContext<'a>, span: Span, symbol: String) -> IResult<(InterpValue, usize)> {
    for scope in &ic.call_stack {
        if let Some(&addr) = scope.entities.get(&symbol) {
            if addr >= ic.base_pointer && addr < ic.stack_pointer {
                return Ok((ic.stack[addr].clone(), addr));
            }
            return Err(fatal_error(ic, span, &format!("value `{}` is outside stack frame", symbol), ""));
        }

        if !scope.is_block_scope {
            break;
        }
    }
    Err(fatal_error(ic, span, &format!("cannot find value `{}` in this scope", symbol), ""))
}

fn create_error_msg<'a>(
    ic: &InterpContext<'a>,
    level: ErrorLevel,
    span: Span,
    message: &str,
    label: &str
) -> ErrorMsg {
    match ic.file {
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
            path: "".to_string(),
            msg: message.to_string(),
            source: "".to_string(),
            label: label.to_string(),
            next: None,
        },
    }
}

fn fatal_error<'a>(ic: &InterpContext<'a>, span: Span, message: &str, label: &str) -> ErrorMsg {
    create_error_msg(ic, ErrorLevel::Fatal, span, message, label)
}

fn mismatched_types_fatal_error<'a>(ic: &InterpContext<'a>, span: Span, expected: &TyKind, found: &Ty) -> ErrorMsg {
    create_error_msg(
        ic,
        ErrorLevel::Fatal,
        span,
        &format!("expected `{}`, found `{}`", expected, found),
        ""
    )
}

fn write_str(buf: &mut fmt::Formatter<'_>, mut s: &str, indent: usize) -> fmt::Result {
    while !s.is_empty() {
        let newline;
        let split = match s.find('\n') {
            Some(pos) => {
                newline = true;
                pos + 1
            },
            None => {
                newline = false;
                s.len()
            },
        };
        buf.write_str(&s[..split])?;
        s = &s[split..];

        if newline {
            buf.write_str(&" ".repeat(indent))?;
        }
    }

    Ok(())
}

impl<'a> fmt::Debug for InterpContext<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        write_str(f, "Trace:", 0)?;

        indent += 4;
        write_str(f, "\nSignatures: [", indent)?;
        if self.signatures.len() > 8 {
            write_str(f, "\n", indent)?;
        }
        let mut result = String::new();
        for (sig, _) in &self.signatures {
            result.push_str(&format!("\"{}\", ", sig));
        }
        result.pop();
        result.pop();
        write_str(f, &result, indent)?;
        if self.signatures.len() > 8 {
            write_str(f, "\n", indent)?;
        }
        indent -= 4;
        write_str(f, "]", indent)?;
        indent += 4;

        write_str(f, "\nCall Stack: {", indent)?;
        for (index, scope) in self.call_stack.iter().rev().enumerate() {
            fmt_interp_scope(scope, f, self.file.unwrap(), indent + 8, index)?;
        }
        write_str(f, "\n}", indent)?;
        write_str(f, &format!("\nStack: {:#?}", self.stack), 4)
    }
}

pub fn fmt_interp_scope(
    scope: &InterpScope,
    f: &mut fmt::Formatter<'_>,
    file: &File,
    indent: usize,
    index: usize
) -> fmt::Result {
    write_str(f, &format!("\n{}: ", index), indent - 4)?;
    let (line, _, line_beg, line_end) = get_span_location_in_file(&file.lines, scope.span);
    let source_line = &file.source[line_beg..line_end];
    if line > 0 {
        write_str(f, source_line.trim(), indent)?;
        write_str(f, &format!("\n at {}:{}", file.filename, line), indent)?;
    } else {
        write_str(f, &format!("{}", file.filename), indent)?;
    }
    if scope.entities.len() > 0 {
        write_str(f, &format!("\nSymbols Table: {:#?},", &scope.entities), indent)?;
    }
    write_str(f, "\n},", indent - 4)
}

pub fn interp_file<'a>(ic: &mut InterpContext<'a>, file: &'a File) {
    ic.file = Some(file);
    for item in &file.items {
        interp_item(ic, &item);
    }
}

pub fn interp_item<'a>(ic: &mut InterpContext<'a>, item: &'a Item) {
    match item { // FIXME(alexander): cloning string should not be necessary, maybe use string interner
        Item::Fn(func) => {
            ic.signatures.insert(func.ident.to_string.clone(), item);
        }
        Item::ForeignFn(func) => {
            ic.signatures.insert(func.ident.to_string.clone(), item);
        }
        Item::ForeignMod(module) => {
            for foreign_item in &module.items {
                interp_item(ic, foreign_item);
            }
        }
    };
}

pub fn interp_entry_point<'a>(ic: &mut InterpContext<'a>) -> i32 {
    let main_function = match ic.signatures.get("main") {
        Some(item) => {
            match item {
                Item::Fn(func) => func,
                _ => {
                    print_error_msg(&fatal_error(ic, Span::new(), "main exists but is not a valid function", ""));
                    return 1;
                }
            }
        }
        None => {
            print_error_msg(&fatal_error(ic, Span::new(), "there is no main function", ""));
            return 1;
        }
    };
    match interp_block(ic, &main_function.block, true) {
        Ok(val) => match val.data {
            Value::Int(out) => out,
            _ => 0, // TODO(alexander): should this be a type error maybe?
        }
        Err(err) => {
            print_error_msg(&err);
            1
        }
    }
}

fn interp_intrinsics<'a>(
    ic: &mut InterpContext<'a>,
    item: &ForeignFnItem,
    values: Vec<InterpValue>
) -> IResult<InterpValue> {
    match item.ident.to_string.as_str() {
        "trace" => {
            trace(ic);
        },

        "print_int" => {
            match values[0].data {
                Value::Int(arg) => print_int(arg),
                _ => return Err(mismatched_types_fatal_error(
                    ic, values[0].span, &TyKind::Int, &to_type(&values[0])))
            };
        },

        "print_bool" => {
            match values[0].data {
                Value::Bool(arg) => print_bool(arg),
                _ => return Err(mismatched_types_fatal_error(
                    ic, values[0].span, &TyKind::Bool, &to_type(&values[0]))),
            };
        },

        "assert" => {
            match values[0].data {
                Value::Bool(arg) => assert(arg),
                _ => return Err(mismatched_types_fatal_error(
                    ic, values[0].span, &TyKind::Bool, &to_type(&values[0]))),
            };
        },

        "assert_eq_int" => {
            match values[0].data {
                Value::Int(arg0) => match values[1].data {
                    Value::Int(arg1) => assert_eq_int(arg0, arg1),
                    _ => return Err(mismatched_types_fatal_error(
                        ic, values[1].span, &TyKind::Int, &to_type(&values[1])))
                }
                _ => return Err(mismatched_types_fatal_error(ic,
                    values[0].span, &TyKind::Int, &to_type(&values[0])))
            };
        },

        "assert_eq_bool" => {
            match values[0].data {
                Value::Bool(arg0) => match values[1].data {
                    Value::Bool(arg1) => assert_eq_bool(arg0, arg1),
                    _ => return Err(mismatched_types_fatal_error(
                        ic, values[1].span, &TyKind::Bool, &to_type(&values[1])))
                }
                _ => return Err(mismatched_types_fatal_error(
                    ic, values[0].span, &TyKind::Bool, &to_type(&values[0]))),
            };
        },

        _ => return Err(fatal_error(
            ic,
            Span::new(),
            &format!("`{}` is not a valid intrinsic function", item.ident.to_string),
            "")),
    };
    Ok(create_interp_value(Value::None, Span::new(), false))
}

pub fn interp_block<'a>(ic: &mut InterpContext<'a>, block: &Block, is_block_scope: bool) -> IResult<InterpValue> {
    let base_pointer = ic.stack_pointer;
    ic.call_stack.push(create_interp_scope(block.span, is_block_scope));

    let mut ret_val = create_interp_value(Value::None, Span::new(), false);
    for stmt in &block.stmts {
        let val = interp_stmt(ic, stmt)?;
        match val.data {
            Value::None => continue,
            _ => ret_val = val,
        };
        break;
    }

    ic.call_stack.pop();
    ic.stack_pointer = base_pointer;
    Ok(ret_val)
}

pub fn interp_stmt<'a>(ic: &mut InterpContext<'a>, stmt: &Stmt) -> IResult<InterpValue> {
    match stmt {
        Stmt::Local(local) => {
            let val = match &*local.init {
                Some(init) => {
                    let mut val = interp_expr(ic, init)?;
                    val.mutable = local.mutable;
                    let val_ty = to_type(&val);
                    if val_ty != local.ty {
                        return Err(mismatched_types_fatal_error(ic, val.span, &local.ty.kind, &val_ty));
                    }
                    val
                }
                None => empty_interp_value()
            };
            store_local_variable(ic, val, Some(local.ident.to_string.clone()));
            Ok(empty_interp_value())
        }

        Stmt::Semi(expr) => {
            let ret_val = interp_expr(ic, expr)?;
            // Only perform explicit returns e.g. return expression.
            Ok(match expr {
                Expr::Return(_) => ret_val,
                Expr::Continue(_) => ret_val,
                Expr::Break(_) => ret_val,
                _ => empty_interp_value(),
            })
        },

        Stmt::Expr(expr) => interp_expr(ic, expr),

        Stmt::Item(item) => {
            let span = match item {
                Item::Fn(x) => x.span,
                Item::ForeignFn(x) => x.span,
                Item::ForeignMod(x) => x.span,
            };
            Err(fatal_error(ic, span, "items in functions are not supported by the interpreter", ""))
        }
    }
}

pub fn interp_expr(ic: &mut InterpContext, expr: &Expr) -> IResult<InterpValue> {
    match expr {
        Expr::Assign    (e) => interp_assign_expr(ic, e),
        Expr::Binary    (e) => interp_binary_expr(ic, e),
        Expr::Block     (e) => interp_block_expr(ic, e),
        Expr::Call      (e) => interp_call_expr(ic, e),
        Expr::Ident     (e) => find_local_variable(ic, e.span, e.to_string.clone()).map(|(v, _)| v),
        Expr::If        (e) => interp_if_expr(ic, e),
        Expr::Lit       (e) => interp_lit_expr(e),
        Expr::Paren     (e) => interp_expr(ic, &e.expr),
        Expr::Reference (e) => interp_reference_expr(ic, e),
        Expr::Return    (e) => interp_return_expr(ic, e),
        Expr::Unary     (e) => interp_unary_expr(ic, e),
        Expr::While     (e) => interp_while_expr(ic, e),
        Expr::Break     (e) => {
            let mut val = empty_interp_value();
            val.span = e.span;
            val.should_break = true;
            Ok(val)
        }
        Expr::Continue  (e) => {
            let mut val = empty_interp_value();
            val.span = e.span;
            val.should_continue = true;
            Ok(val)
        }
    }
}

/**
 * Interprets the memory address of a given expression.
 */
pub fn interp_addr_of_expr(ic: &mut InterpContext, expr: &Expr) -> IResult<(InterpValue, usize)> {
    match expr {
        Expr::Ident(ident) => find_local_variable(ic, ident.span, ident.to_string.clone()),
        Expr::Unary(unary) => {
            match unary.op {
                UnOp::Deref => {
                    let (value, _) = interp_addr_of_expr(ic, &unary.expr)?;
                    match value.data {
                        Value::Ref(r) => {
                            if r.mutable {
                                Ok((ic.stack[r.addr].clone(), r.addr))
                            } else {
                                
                                let mut err = fatal_error(
                                    ic,
                                    expr.get_span(),
                                    "cannot assign to variable using immutable reference",
                                    "variable is a reference, so data it refers to cannot be written");
                                err.next = Some(Box::new(create_error_msg(
                                    ic,
                                    ErrorLevel::Help,
                                    r.ref_ty.span,
                                    "",
                                    "help: consider changing this to be a mutable reference"
                                )));
                                
                                Err(err)
                            }
                        },
                        
                        _ => Err(fatal_error(ic, Span::new(), "invalid expression", "")),
                    }
                },
                
                _ => Err(fatal_error(ic, Span::new(), "invalid expression", "")),
            }
        },
        
        _ => Err(fatal_error(ic, Span::new(), "invalid expression", "")),
    }
}

fn interp_assign_expr(ic: &mut InterpContext, expr: &ExprAssign) -> IResult<InterpValue> {
    let addr = match interp_addr_of_expr(ic, &expr.left) {
        Ok(addr) => addr.1,
        Err(mut err) => {
            if err.msg == "invalid expression" {
                err = fatal_error(
                    ic,
                    expr.span,
                    "invalid left-hand side expression",
                    "left-hand of expression not valid");
            }
            return Err(err);
        }
    };
    let val = interp_expr(ic, &expr.right)?;
    ic.stack[addr] = val;
    Ok(empty_interp_value())
}

pub fn interp_binary_expr<'a>(ic: &mut InterpContext<'a>, expr: &ExprBinary) -> IResult<InterpValue> {
    let left_val = interp_expr(ic, &expr.left)?;
    let left_type = to_type(&left_val);
    let right_val = interp_expr(ic, &expr.right)?;
    let right_type = to_type(&right_val);
    let combined_span = Span::combine(left_val.span, right_val.span);

    let result = match left_val.data {
        Value::Int(lhs) => {
            match right_val.data {
                Value::Int(rhs) => match expr.op {
                    BinOp::Add => Value::Int(lhs + rhs),
                    BinOp::Sub => Value::Int(lhs - rhs),
                    BinOp::Div => Value::Int(lhs / rhs),
                    BinOp::Mul => Value::Int(lhs * rhs),
                    BinOp::Pow => Value::Int(lhs.pow(rhs as u32)),
                    BinOp::Mod => Value::Int(lhs % rhs),
                    BinOp::Eq  => Value::Bool(lhs == rhs),
                    BinOp::Ne  => Value::Bool(lhs != rhs),
                    BinOp::Lt  => Value::Bool(lhs <  rhs),
                    BinOp::Le  => Value::Bool(lhs <= rhs),
                    BinOp::Gt  => Value::Bool(lhs >  rhs),
                    BinOp::Ge  => Value::Bool(lhs >= rhs),
                    _ => Value::None,
                },
                _ => Value::None,
            }
        },

        Value::Bool(lhs) => {
            match right_val.data {
                Value::Bool(rhs) => match expr.op {
                    BinOp::And => Value::Bool(lhs && rhs),
                    BinOp::Or  => Value::Bool(lhs || rhs),
                    _ => Value::None,
                },
                _ => Value::None,
            }
        },
        
        _ => Value::None,
    };

    match result {
        Value::None => {
            Err(fatal_error(
                ic,
                combined_span,
                &format!("cannot {} `{}` to `{}`", expr.op, left_type, right_type),
                &format!("no implementation for `{} {} {}`", left_type, expr.op.token(), right_type)))
        },
        _ => Ok(create_interp_value(result, combined_span, false)),
    }
}

/**
 * Interprets a block expression.
 */
pub fn interp_block_expr(ic: &mut InterpContext, block: &ExprBlock) -> IResult<InterpValue> {
    interp_block(ic, &block.block, true)
}

/**
 * Interprets a function call.
 */
pub fn interp_call_expr(ic: &mut InterpContext, call: &ExprCall) -> IResult<InterpValue> {
    let base_pointer = ic.base_pointer;
    let new_scope = create_interp_scope(call.span, false);
    ic.call_stack.push(new_scope);

    let mut values = Vec::new();
    for arg in &call.args {
        let val = interp_expr(ic, arg)?;
        values.push(val);
    }

    let item = match ic.signatures.get(&call.ident.to_string) {
        Some(item) => item,
        None => return Err(fatal_error(
            ic,
            call.ident.span,
            &format!("cannot find function `{}` in this scope", call.ident.to_string),
            ""))
    };
    
    let result = match item {
        Item::Fn(func) => {

            let inputs = &func.decl.inputs;
            if inputs.len() == values.len() {
                for i in 0..inputs.len() {
                    let arg_ty = &inputs[i].ty;
                    let val_ty = &to_type(&values[i]);
                    if arg_ty != val_ty {
                        let span = values[i].span;
                        return Err(mismatched_types_fatal_error(ic, span, &arg_ty.kind, val_ty));
                    }
                    let id = &inputs[i].ident;
                    let addr = store_local_variable(ic, values[i].clone(), Some(id.to_string.clone()));
                    let len = ic.call_stack.len();
                    ic.call_stack[len - 1].entities.insert(id.to_string.clone(), addr);
                }
            } else {
                let err = fatal_error(
                    ic,
                    func.span,
                    &format!("this function takes {} parameters but {} parameters were supplied",
                             inputs.len(),
                             values.len()),
                    ""
                );
                return Err(err);
            }

            interp_block(ic, &func.block, false)
        }

        Item::ForeignFn(func) => {
            if values.len() != func.decl.inputs.len() {
                return Err(fatal_error(
                    ic,
                    func.span,
                    &format!("this function takes {} parameters but {} parameters were supplied",
                             func.decl.inputs.len(),
                             values.len()),
                    ""))
            }
            interp_intrinsics(ic, &func, values)
        }

        _ => {
            let ident = &call.ident;
            Err(fatal_error(
                ic,
                ident.span,
                &format!("cannot find function `{}` in this scope", ident.to_string),
                "not found in this scope"))
        }
    };

    ic.stack_pointer = ic.base_pointer;
    ic.base_pointer = base_pointer;
    ic.call_stack.pop();

    return result;
}

/**
 * Interprets an if statement.
 */
pub fn interp_if_expr(ic: &mut InterpContext, if_expr: &ExprIf) -> IResult<InterpValue> {
    let value = interp_expr(ic, &if_expr.cond)?;
    match value.data {
        Value::Bool(cond) => {
            if cond {
                interp_block(ic, &if_expr.then_block, true)
            } else {
                match if_expr.else_block.clone() {
                    Some(block) => {
                        interp_block(ic, &block, true)
                    },
                    None => Ok(empty_interp_value()),
                }
            }
        },
        _ => Err(mismatched_types_fatal_error(ic, value.span, &TyKind::Bool, &to_type(&value))),
    }
}

/**
 * Interprets a literal.
 */
pub fn interp_lit_expr(literal: &ExprLit) -> IResult<InterpValue> {
    match literal.lit {
        Lit::Int(val)  => Ok(create_interp_value(Value::Int(val), literal.span, false)),
        Lit::Bool(val) => Ok(create_interp_value(Value::Bool(val), literal.span, false)),
    }
}

/**
 * Interprets a reference expression.
 */
pub fn interp_reference_expr(ic: &mut InterpContext, ref_expr: &ExprReference) -> IResult<InterpValue> {
    match interp_addr_of_expr(ic, &ref_expr.expr) { // TODO(alexander): this does not work for referencing constants!
        Ok((value, addr)) => {
            let reference = Reference {
                addr,
                ref_ty: to_type(&value),
                mutable: ref_expr.mutable,
            };
            Ok(create_interp_value(Value::Ref(reference), ref_expr.span, ref_expr.mutable))
        }
        Err(mut err) => {
            if err.msg == "invalid expression" {
                err = fatal_error(
                    ic,
                    ref_expr.span,
                    "cannot reference of ",
                    "left-hand of expression not valid");
            }
            Err(err)
        }
    }
}
        
/**
 * Interprets a return statement.
 */
pub fn interp_return_expr(ic: &mut InterpContext, return_expr: &ExprReturn) -> IResult<InterpValue> {
    let ret = match &*return_expr.expr {
        Some(expr) => interp_expr(ic, expr),
        None => Ok(create_interp_value(Value::Void, return_expr.span, false)),
    };
    ret.map(|mut val| {
        val.from_return = true;
        val
    })
}

/**
 * Interpret unary expression.
 */
pub fn interp_unary_expr<'a>(ic: &mut InterpContext, unary: &ExprUnary) -> IResult<InterpValue> {
    let value = interp_expr(ic, &unary.expr)?;
    let value_type = to_type(&value);

    let result = match unary.op {
        UnOp::Neg => match value.data {
            Value::Int(val) => Value::Int(-val),
            _ => Value::None,
        },

        UnOp::Not => match value.data {
            Value::Bool(val) => Value::Bool(!val),
            _ => Value::None,
        },

        UnOp::Deref => match value.data {
            Value::Ref(r) => ic.stack[r.addr].data.clone(),
            _ => Value::None,
        }
    };

    match result {
        Value::None => {
            let err = fatal_error(
                ic,
                unary.span,
                &format!("type `{}` cannot be {}", value_type, unary.op),
                &format!("no implementation for `{}{}`", unary.op.token(), value_type));
            Err(err)
        }
        _ => Ok(create_interp_value(result, unary.span, false)),
    }
}

/**
 * Interprets a while loop.
 */
pub fn interp_while_expr(ic: &mut InterpContext, while_expr: &ExprWhile) -> IResult<InterpValue> {
    loop {
        let value = interp_expr(ic, &while_expr.cond)?;
        match value.data {
            Value::Bool(cond) => {
                if cond {
                    let val = interp_block(ic, &while_expr.block, true)?;
                    if let Value::None = val.data {
                        continue;
                    }

                    if val.should_continue {
                        continue;
                    }

                    if val.should_break {
                        break;
                    }

                    return Ok(val);
                } else {
                    break;
                }
            },
            _ => return Err(mismatched_types_fatal_error(ic, value.span, &TyKind::Bool, &to_type(&value)))
        };
    }
    Ok(empty_interp_value())
}
