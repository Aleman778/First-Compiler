
/***************************************************************************
 * Helper functions for testing mathematical expressions.
 ***************************************************************************/


use compiler::ast::{
    op::*,
    lit::*,
    expr::*,
};
use compiler::parser::ParseSpan;


/**
 * Value enum can either be i32 or bool.
 */
pub enum Val {
    Num(i32),
    Bool(bool),
    Error,
}


/**
 * Parses and evaluates the input expression.
 */
pub fn eval_math(input: &str) -> Val {
    let (_, expr) = Expr::parse_math(ParseSpan::new(input)).unwrap();
    eval_expr(expr)
}


/**
 * Evaluates an expression.
 */
fn eval_expr(expr: Expr) -> Val {
    match expr {
        Expr::Binary(binary) => eval_binary(binary),
        Expr::Unary(unary) => eval_unary(unary),
        Expr::Lit(literal) => eval_literal(literal),
        Expr::Paren(paren) => eval_paren(paren),
        _ => Val::Error,
    }
}


/**
 * Evaluates a binary expression.
 */
fn eval_binary(binary: ExprBinary) -> Val {
    let left = eval_expr(*binary.left);
    let right = eval_expr(*binary.right);
    let bl = get_bool(&left);
    let br = get_bool(&right);
    let il = get_int(&left);
    let ir = get_int(&right);
    if bl.is_ok() && br.is_ok() {
        // Boolean binary operations
        let bl: bool = bl.unwrap();
        let br: bool = br.unwrap();
        match binary.op {
            BinOp::Eq{span: _}  => Val::Bool(bl == br),
            BinOp::Ne{span: _}  => Val::Bool(bl != br),
            BinOp::And{span: _} => Val::Bool(bl && br),
            BinOp::Or{span: _}  => Val::Bool(bl || br),
            _ => Val::Error,
        }
    } else if il.is_ok() && ir.is_ok() {
        // Integer binary opeartions
        let il: i32 = il.unwrap();
        let ir: i32 = ir.unwrap();
        match binary.op {
            BinOp::Eq{span: _}  => Val::Bool(il == ir),
            BinOp::Ne{span: _}  => Val::Bool(il != ir),
            BinOp::Lt{span: _}  => Val::Bool(il <  ir),
            BinOp::Le{span: _}  => Val::Bool(il <= ir),
            BinOp::Gt{span: _}  => Val::Bool(il >  ir),
            BinOp::Ge{span: _}  => Val::Bool(il >= ir),
            BinOp::Add{span: _} => Val::Num(il + ir),
            BinOp::Sub{span: _} => Val::Num(il - ir),
            BinOp::Mul{span: _} => Val::Num(il * ir),
            BinOp::Div{span: _} => Val::Num(il / ir),
            BinOp::Mod{span: _} => Val::Num(il % ir),
            _ => Val::Error
        }
    } else {
        Val::Error
    }
}


/**
 * Evaluates a unary expression.
 */
fn eval_unary(unary: ExprUnary) -> Val {
    let val = eval_expr(*unary.right);
    match unary.op {
        UnOp::Neg{span: _} => Val::Num(-get_int(&val).unwrap()),
        UnOp::Not{span: _} => Val::Bool(!get_bool(&val).unwrap()),
        _ => Val::Error
    }
}


/**
 * Evaluates a literal expression.
 */
fn eval_literal(literal: ExprLit) -> Val {
    match literal.lit {
        Lit::Int(lit) => Val::Num(lit.value),
        Lit::Bool(lit) => Val::Bool(lit.value),
    }
}

    

/**
 * Evaluates a parenthesized expression.
 */
fn eval_paren(paren: ExprParen) -> Val {
    eval_expr(*paren.expr)
}


/**
 * Get the integer value of an expression.
 * Returns a type error if expression is not an i32 number.
 */
fn get_int<'a>(value: &'a Val) -> Result<i32, &'static str> {
    match value {
        Val::Num(val) => Ok(*val),
        _ => Err("type error"),
    }
}


/**
 * Get the boolean value of an expression.
 * Returns type error if the expression is not a boolean.
 */
fn get_bool<'a>(value: &'a Val) -> Result<bool, &'static str> {
    match value {
        Val::Bool(b) => Ok(*b),
        _ => Err("type error"),
    }
}
