
/***************************************************************************
 * Helper functions for testing mathematical expressions.
 ***************************************************************************/


use crate::compiler::{
    ast::op::*,
    ast::lit::*,
    ast::expr::*,
    parser::Parser,
};


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
    let expr = Expr::parse_math(input);
    eval_expr(expr);
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
    }
}


/**
 * Evaluates a binary expression.
 */
fn eval_binary(binary: ExprBinary) -> Val {
    let left = eval_expr(binary.left);
    let right = eval_expr(binary.right);
    let bl = get_bool(left);
    let br = get_bool(right);
    let il = get_int(left);
    let ir = get_int(right);
    if bl.is_ok() && br.is_ok() {
        // Boolean binary operations
        let bl: bool = bl.unwrap();
        let br: bool = br.unwrap();
        match binary.op {
            BinOp::Eq{span: _}  => Ok(Val::Bool(bl == br)),
            BinOp::Ne{span: _}  => Ok(Val::Bool(bl != br)),
            BinOp::And{span: _} => Ok(Val::Bool(bl && br)),
            BinOp::Or{span: _}  => Ok(Val::Bool(bl || br)),
            _ => Val::Error
        }
    } else if il.is_ok() && ir.is_ok() {
        // Integer binary opeartions
        let il: i32 = il.unwrap();
        let ir: i32 = ir.unwrap();
        match binary.op {
            BinOp::Eq{span: _}  => Ok(Val::Bool(il == ir)),
            BinOp::Ne{span: _}  => Ok(Val::Bool(il != ir)),
            BinOp::Lt{span: _}  => Ok(Val::Bool(il <  ir)),
            BinOp::Le{span: _}  => Ok(Val::Bool(il <= ir)),
            BinOp::Lt{span: _}  => Ok(Val::Bool(il >  ir)),
            BinOp::Le{span: _}  => Ok(Val::Bool(il >= ir)),
            BinOp::Add{span: _} => Ok(Val::Num(il + ir)),
            BinOp::Sub{span: _} => Ok(Val::Num(il - ir)),
            BinOp::Mul{span: _} => Ok(Val::Num(il * ir)),
            BinOp::Div{span: _} => Ok(Val::Num(il / ir)),
            BinOp::Mod{span: _} => Ok(Val::Num(il % ir)),
            _ => Val::Error
        }
    }
    
    Val::Error
}


/**
 * Evaluates a unary expression.
 */
fn eval_unary(unary: ExprUnary) -> Val {
    let val = eval_expr(unary.right, env)?;
    match op.1 {
        UnOp::Sub => Val::Num(-get_int(val).unwrap()),
        UnOp::Not => Val::Bool(!get_bool(val).unwrap()),
        _ => Val::Error
    }
}


/**
 * Evaluates a literal expression.
 */
fn eval_literal(literal: ExprLit) -> Val {
    match literal.lit {
        Lit::Int(lit) => Val(lit.value),
        Lit::Bool(lit) => Val(lit.value),
    }
}

    

/**
 * Evaluates a parenthesized expression.
 */
fn eval_paren(paren: ExprParen) -> Val {
    eval_expr(paren.expr)
}


/**
 * Get the integer value of an expression.
 * Returns a type error if expression is not an i32 number.
 */
fn get_int<'a>(value: SpanVal<'a>) -> Result<i32, &'static str> {
    match value.1 {
        Val::Num(val) => Ok(val),
        _ => Err("type error"),
    }
}


/**
 * Get the boolean value of an expression.
 * Returns type error if the expression is not a boolean.
 */
fn get_bool<'a>(value: SpanVal<'a>) -> Result<bool, &'static str> {
    match value.1 {
        Val::Bool(b) => Ok(b),
        _ => Err("type error"),
    }
}
