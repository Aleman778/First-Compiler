
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::ast::{
    expr::*,
};
use crate::interp::{
    value::Val,
    IResult,
    Eval,
};


/**
 * Evaluate an expression.
 */
impl Eval for Expr {
    fn eval(&self) -> IResult<Val> {
        match self {
            Expr::Binary(binary) => binary.eval(),
            Expr::Lit(literal) => literal.eval(),
            _ => Ok(Val::None),
        }
    }
}


/**
 * Evalues a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self) -> IResult<Val> {
        let left = self.left.eval()?;
        let right = self.right.eval()?;
        self.op.eval(left, right, self.span.clone())
    }
}


/**
 * Evaluates a literal expression.
 */
impl Eval for ExprLit {
    fn eval(&self) -> IResult<Val> {
        self.lit.eval()
    }
}
