#![allow(dead_code)]

/**
 * Require the parser from assignment 2.
 */
use a02_parser::{
    Op,
    Span,
    SpanOp,
    SpanType,
    SpanExpr,
    Expr,
    AST,
};


/**
 * Require the use of env module.
 */
use crate::env::Env;


/**
 * Used for custom erros and formatting them.
 */
use std::{fmt, error};


/**
 * Runtime error is occured when the given program
 * was not able to be interpreted i.e. a faulty program was detected.
 */
#[derive(Debug, Clone)]
pub enum RuntimeError<'a> {
    TypeError(&'a str, Span<'a>),
    InvalidExpression(&'a str, Span<'a>),
    MemoryError(&'a str, Span<'a>),
}

/**
 * Type alias of result to always include runtime erros.
 */
pub type Result<'a, O, E = RuntimeError<'a>> = std::result::Result<O, E>;


/**
 * Value enum is used to return information during evaluation.
 */
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Val {
    // Number literal
    Num(i32),

    // Boolean literal
    Bool(bool),

    // No value
    None,
}


/**
 * Type alias of identifier strings to include span information.
 */
pub type SpanIdent<'a> = (Span<'a>, &'a str);


/**
 * Formatting runtime errors.
 */
impl<'a> fmt::Display for RuntimeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::TypeError(reason, span) => write!(f, "{} {:?}", reason, span),
            RuntimeError::InvalidExpression(reason, span) => write!(f, "{} {:?}", reason, span),
            RuntimeError::MemoryError(reason, span) => write!(f, "{} {:?}", reason, span),
        }
    }
}


/**
 * Implementing runtime errors as an error.
 */
impl<'a> error::Error for RuntimeError<'a> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            RuntimeError::TypeError(_, _) => None,
            RuntimeError::InvalidExpression(_, _) => None,
            RuntimeError::MemoryError(_, _) => None,
        }
    }
}


/**
 * Runs the main method in the Abstract Syntax Tree.
 */
pub fn eval<'a>(ast: AST<'a>) {
    let mut env = Env::new();
    let val = eval_ast(ast, &mut env);
    println!("env:\n{:#?}\n\nval:\n{:#?}", env, val);
}


/**
 * Evaluates the Abstract Syntax Tree, iterates over all the functions
 * and stores them inside the environment also the main function is evaluated.
 */
pub fn eval_ast<'a>(ast: AST<'a>, env: &mut Env<'a>) -> Result<'a, Val> {
    for func in ast.0 {
        env.store_func(func.1);
    }
    let expr = env.push_main().unwrap();
    eval_expr(*expr, env)
}


/**
 * Evaluates an expression inside a given environment and returns a value,
 * note that an expression may update the environment, e.g. let statements.
 * e.g. ast for 5 + 2 gives the result Num(7).
 */
pub fn eval_expr<'a>(expr: SpanExpr<'a>, env: &mut Env<'a>) -> Result<'a, Val> {
    match expr.1 {
        Expr::BinOp(left, op, right) => eval_binop(*left, op, *right, env),
        Expr::UnOp(op, right) => eval_unop(op, *right, env),
        Expr::Block(exprs) => eval_block(exprs, env),
        Expr::Local(mutable, ident, ty, init) => eval_local(mutable, *ident, ty, *init, env),
        _ => eval_atom(expr, env),
    }
}


/**
 * Computes the value of a binary operation.
 */
fn eval_binop<'a>(left: SpanExpr<'a>, op: SpanOp<'a>, right: SpanExpr<'a>, env: &mut Env<'a>) -> Result<'a, Val> {
    let lval = eval_atom(left.clone(), env).unwrap();
    let rval = eval_expr(right.clone(), env).unwrap();
    let bl = get_bool(lval, left.0);
    let br = get_bool(rval, right.0);
    let il = get_int(lval, left.0);
    let ir = get_int(rval, right.0);
    if bl.is_ok() && br.is_ok() {
        // Boolean binary operations
        let bl: bool = bl.unwrap();
        let br: bool = br.unwrap();
        match op.1 {
            Op::Equal => Ok(Val::Bool(bl == br)),
            Op::NotEq => Ok(Val::Bool(bl != br)),
            Op::And   => Ok(Val::Bool(bl && br)),
            Op::Or    => Ok(Val::Bool(bl || br)),
            _ => Err(RuntimeError::InvalidExpression("not a valid binary operator for boolean values", op.0)),
        }
    } else if il.is_ok() && ir.is_ok() {
        // Integer binary opeartions
        let il: i32 = il.unwrap();
        let ir: i32 = ir.unwrap();
        match op.1 {
            Op::Equal      => Ok(Val::Bool(il == ir)),
            Op::NotEq      => Ok(Val::Bool(il != ir)),
            Op::LessThan   => Ok(Val::Bool(il <  ir)),
            Op::LessEq     => Ok(Val::Bool(il <= ir)),
            Op::LargerThan => Ok(Val::Bool(il >  ir)),
            Op::LargerEq   => Ok(Val::Bool(il >= ir)),
            Op::Add        => Ok(Val::Num(il + ir)),
            Op::Sub        => Ok(Val::Num(il - ir)),
            Op::Mul        => Ok(Val::Num(il * ir)),
            Op::Div        => Ok(Val::Num(il / ir)),
            Op::Mod        => Ok(Val::Num(il % ir)),
            _ => Err(RuntimeError::InvalidExpression("not a valid binary operator for integer values", op.0)),
        }
    } else {
        if bl.is_ok() {
            return Err(RuntimeError::TypeError("expected type bool got i32", right.0));
        } else if il.is_ok() {
            return Err(RuntimeError::TypeError("expected type i32 got bool", right.0));
        } else {
            return Err(RuntimeError::TypeError("incompatible type", right.0));
        }
    }
}


/**
 * Computes the value of an unary operation.
 */
fn eval_unop<'a>(op: SpanOp<'a>, right: SpanExpr<'a>, env: &mut Env<'a>) -> Result<'a, Val> {
    let val = eval_expr(right.clone(), env).unwrap();
    match op.1 {
        Op::Sub => Ok(Val::Num(-get_int(val, right.0).unwrap())),
        Op::Not => Ok(Val::Bool(!get_bool(val, right.0).unwrap())),
        _ => Err(RuntimeError::InvalidExpression("not a valid unary operator", op.0)),
    }
}


/**
 * Evaluates a block of expressions. If this is a function then ret param
 * should be true to indicate if the function returns something.
 */
pub fn eval_block<'a>(exprs: Vec<SpanExpr<'a>>, env: &mut Env<'a>) -> Result<'a, Val> {
    for i in 0..exprs.len() {
        match exprs[i].1.clone() {
            Expr::Return(val) => return eval_return(*val, env),
            _ => {
                let ret = eval_expr(exprs[i].clone(), env).unwrap();
                if i == (exprs.len() - 1) {
                    return Ok(ret);
                }
            }
        }
    }
    Ok(Val::None)
}


/**
 * Evaluates assignment of a local variable.
 * This only updates the given environment and returns Val::None.
 */
pub fn eval_local<'a>(_mutable: bool, ident: SpanExpr<'a>, _ty: SpanType<'a>, init: SpanExpr<'a>, env: &mut Env<'a>) -> Result<'a, Val> {
    let val = eval_expr(init, env).unwrap();
    let res = env.store_var(get_ident(&ident).unwrap(), val);
    match res {
        Ok(_) => Ok(Val::None),
        Err(_) => Err(RuntimeError::MemoryError("failed to store variable", ident.0)),
    }
}


/**
 * Evaluates a return statement with optional return value.
 * If no return value is provided Val::None is returned instead.
 */
pub fn eval_return<'a>(val: Option<SpanExpr<'a>>, env: &mut Env<'a>) -> Result<'a, Val> {
    match val {
        Some(expr) => Ok(eval_expr(expr, env).unwrap()),
        None => Ok(Val::None),
    }
}


/**
 * Evaluates an atom i.e. either a parenthesized expression, literal, function call or identifier.
 */
pub fn eval_atom<'a>(expr: SpanExpr<'a>, env: &mut Env<'a>) -> Result<'a, Val> {
    match expr.1 {
        Expr::Paren(inl_expr) => eval_expr(*inl_expr, env),
        Expr::Call(ident, args) => eval_func_call(*ident, args, env),
        Expr::Ident(ident) => Ok(env.load_var((Span::new(""), ident)).unwrap()),
        Expr::Num(int) => Ok(Val::Num(int)),
        Expr::Bool(en) => Ok(Val::Bool(en)),
        _ => Err(RuntimeError::InvalidExpression("invalid expression", expr.0)),
    }
}

/**
 * Evaluates an function call, creates a new environment and runs the function.
 * This function return is forwarded from whatver the invoked function returns.
 */
pub fn eval_func_call<'a>(ident: SpanExpr<'a>, args: Vec<SpanExpr<'a>>, env: &mut Env<'a>) -> Result<'a, Val> {
    let mut values = Vec::new();
    for arg in args {
        values.push(eval_expr(arg, env).unwrap());
    }
    let id = get_ident(&ident).unwrap();
    let expr = env.push_func(id, values).unwrap();
    let ret = match (*expr).1 {
        Expr::Block(expr) => eval_block(expr, env),
        _ => Err(RuntimeError::InvalidExpression("should be a block", (*expr).0)),
    };
    env.pop_func();
    ret
}


/**
 * Get the identifier from an expression.
 */
pub fn get_ident<'a>(expr: &SpanExpr<'a>) -> Result<'a, SpanIdent<'a>> {
    match expr.1 {
        Expr::Ident(id) => Ok((expr.0, id)),
        _ => Err(RuntimeError::InvalidExpression("not a valid identifier", expr.0)),
    }
}


/**
 * Get the integer value of an expression.
 * Returns a type error if expression is not an i32 number.
 */
pub fn get_int<'a>(value: Val, span: Span<'a>) -> Result<i32> {
    match value {
        Val::Num(val) => Ok(val),
        _ => Err(RuntimeError::TypeError("expected type i32 got bool", span)),
    }
}


/**
 * Get the boolean value of an expression.
 * Returns type error if the expression is not a boolean.
 */
pub fn get_bool<'a>(value: Val, span: Span<'a>) -> Result<bool> {
    match value {
        Val::Bool(b) => Ok(b),
        _ => Err(RuntimeError::TypeError("expected type bool got i32", span)),
    }
}
