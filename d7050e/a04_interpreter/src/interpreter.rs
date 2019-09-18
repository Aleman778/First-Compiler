use a02_parser::{Op, Expr, Function, Argument};


type Result<T> = Result<I, RuntimeError>;

#[derive(Debug, Clone)]
enum RuntimeError<'a> {
    TypeError(&'a str, Span<'a>),
}


/**
 * Formatting runtime errors.
 */
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::TypeError => write!(f, "type error"),
        }
    }
}


/**
 * Implementing runtime errors as an error.
 */
impl error::Error for RuntimeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            SyntaxError::TypeError => None,
        }
    }
}


pub fn interpret_expr(expr: SpanExpr) -> Result<SpanExpr> {
    match expr {
        BinOp(left, op, right) => calculate(interpret_expr(left.1) op, interpret_expr(right.1)),
        _ => expr,
    }
}


pub fn get_integer(expr: SpanExpr) => Result<i32> {
    match (expr) {
        Num(val) => val,
        _ => Err(RuntimeError::TypeError("expected type: i32", )),
    }
}



fn compute(left: SpanExpr, op: Op, right: SpanExpr) -> Result<SpanExpr> {
    match op {
        Add => Expr::Num();
    }
}
