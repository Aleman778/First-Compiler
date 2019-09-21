/**
 * Require the parser from assignment 2.
 */
use a02_parser::{
    Op,
    Span,
    SpanOp,
    SpanVal,
    SpanExpr,
    Expr,
    Val,
};


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
}


/**
 * Formatting runtime errors.
 */
impl<'a> fmt::Display for RuntimeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::TypeError(reason, span) => write!(f, "{} {:?}", reason, span),
            RuntimeError::InvalidExpression(reason, span) => write!(f, "{} {:?}", reason, span),
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
        }
    }
}


pub fn eval_commands() {
    
}


/**
 * Interpret an expression returns a simplified expression,
 * e.g. ast for 5 + 2 gives the result Num(7).
 */
pub fn eval_expr<'a>(expr: SpanExpr<'a>) -> Result<SpanVal<'a>, RuntimeError<'a>> {
    match expr.1 {
        Expr::BinOp(left, op, right) => map_res(expr.0, compute_binop(eval_expr(*left).unwrap(), op,
                                                                      eval_expr(*right).unwrap())),
        Expr::UnOp(op, right) => map_res(expr.0, compute_unop(op, eval_expr(*right).unwrap())),
        Expr::Paren(inl_expr) => eval_expr(*inl_expr),
        Expr::Val(val) => Ok((expr.0, val)),
        _ => Err(RuntimeError::InvalidExpression("invalid expression", expr.0)),
    }
}


/**
 * Convinence function for mapping the expr result onto the span expr result.
 */
pub fn map_res<'a>(span: Span<'a>, res: Result<Val, RuntimeError<'a>>) -> Result<SpanVal<'a>, RuntimeError<'a>>{
    match res {
        Ok(expr) => Ok((span, expr)),
        Err(err) => Err(err),
    }
}


/**
 * Get the integer value of an expression.
 * Returns a type error if expression is not an i32 number.
 */
pub fn get_int<'a>(value: &SpanVal<'a>) -> Result<i32, RuntimeError<'a>> {
    match value.1 {
        Val::Num(val) => Ok(val),
        _ => Err(RuntimeError::TypeError("expected type i32 got bool", value.0)),
    }
}


/**
 * Get the boolean value of an expression.
 * Returns type error if the expression is not a boolean.
 */
pub fn get_bool<'a>(value: &SpanVal<'a>) -> Result<bool, RuntimeError<'a>> {
    match value.1 {
        Val::Bool(b) => Ok(b),
        _ => Err(RuntimeError::TypeError("expected type bool got i32", value.0)),
    }
}


/**
 * Computes the value of a binary operation.
 */
fn compute_binop<'a>(left: SpanVal<'a>, op: SpanOp<'a>, right: SpanVal<'a>) -> Result<Val, RuntimeError<'a>> {
    let bl = get_bool(&left);
    let br = get_bool(&right);
    let il = get_int(&left);
    let ir = get_int(&right);
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
fn compute_unop<'a>(op: SpanOp<'a>, right: SpanVal<'a>) -> Result<Val, RuntimeError<'a>> {
    match op.1 {
        Op::Sub => Ok(Val::Num(-get_int(&right).unwrap())),
        Op::Not => Ok(Val::Bool(!get_bool(&right).unwrap())),
        _ => Err(RuntimeError::InvalidExpression("not a valid unary operator", op.0)),
    }
}
