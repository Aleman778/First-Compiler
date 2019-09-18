/**
 * Require the parser from assignment 2.
 */
use a02_parser::{
    Op,
    Span,
    SpanOp,
    SpanExpr,
    SpanFn,
    SpanArg,
    Expr,
    Function,
    Argument
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
    InvalidExpression(&'a str),
}


/**
 * Formatting runtime errors.
 */
impl<'a> fmt::Display for RuntimeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::TypeError(reason, span) => write!(f, "{} {:?}", reason, span),
            RuntimeError::InvalidExpression(reason) => write!(f, "{}", reason),
        }
    }
}


/**
 * Implementing runtime errors as an error.
 */
impl<'a> error::Error for RuntimeError<'a> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            RuntimeError::TypeError(reason, span) => None,
            RuntimeError::InvalidExpression(reason) => None,
        }
    }
}


/**
 * Interpret an expression returns a simplified expression,
 * e.g. 5 + 2 gives the reuslt 7.
 */
pub fn interpret_expr<'a>(expr: SpanExpr<'a>) -> Result<SpanExpr<'a>, RuntimeError<'a>> {
    match expr.1 {
        Expr::BinOp(left, op, right) => Ok((expr.0, compute(interpret_expr(*left).unwrap(), op,
                                                            interpret_expr(*right).unwrap()).unwrap())),
        Expr::Paren(inl_expr) => Ok(*inl_expr),
        _ => Ok(expr),
    }
}


/**
 * Get the integer value of an expression.
 * Returns a type error if expression is not an i32 number.
 */
pub fn get_int<'a>(expr: SpanExpr<'a>) -> Result<i32, RuntimeError<'a>> {
    match expr.1 {
        Expr::Num(val) => Ok(val),
        _ => Err(RuntimeError::TypeError("expected type i32", expr.0)),
    }
}


/**
 * Get the boolean value of an expression.
 * Returns type error if the expression is not a boolean.
 */
pub fn get_bool<'a>(expr: SpanExpr<'a>) -> Result<bool, RuntimeError<'a>> {
    match expr.1 {
        Expr::Bool(b) => Ok(b),
        _ => Err(RuntimeError::TypeError("expected type bool", expr.0)),
    }
}


/**
 * Computes the value of a binary operation.
 */
fn compute<'a>(left: SpanExpr<'a>, op: SpanOp<'a>, right: SpanExpr<'a>) -> Result<Expr<'a>, RuntimeError<'a>> {
    match op.1 {
        // Boolean operators
        Op::Equal      => Ok(Expr::Bool(get_bool(left).unwrap() == get_bool(right).unwrap())),
        Op::NotEq      => Ok(Expr::Bool(get_bool(left).unwrap() != get_bool(right).unwrap())),
        Op::LessThan   => Ok(Expr::Bool(get_bool(left).unwrap() <  get_bool(right).unwrap())),
        Op::LessEq     => Ok(Expr::Bool(get_bool(left).unwrap() <= get_bool(right).unwrap())),
        Op::LargerThan => Ok(Expr::Bool(get_bool(left).unwrap() >  get_bool(right).unwrap())),
        Op::LargerEq   => Ok(Expr::Bool(get_bool(left).unwrap() >= get_bool(right).unwrap())),
        Op::And        => Ok(Expr::Bool(get_bool(left).unwrap() && get_bool(right).unwrap())),
        Op::Or         => Ok(Expr::Bool(get_bool(left).unwrap() || get_bool(right).unwrap())),
        
        // Numerical operators
        Op::Add => Ok(Expr::Num(get_int(left).unwrap() + get_int(right).unwrap())),
        Op::Sub => Ok(Expr::Num(get_int(left).unwrap() - get_int(right).unwrap())),
        Op::Mul => Ok(Expr::Num(get_int(left).unwrap() * get_int(right).unwrap())),
        Op::Div => Ok(Expr::Num(get_int(left).unwrap() / get_int(right).unwrap())),
        Op::Mod => Ok(Expr::Num(get_int(left).unwrap() % get_int(right).unwrap())),
        _ => return Err(RuntimeError::InvalidExpression("not a valid binary operator")),
    }
}

