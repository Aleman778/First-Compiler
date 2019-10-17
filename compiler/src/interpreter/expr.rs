
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::ast::expr::*;
use crate::interpreter::{
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
        }
    }
}


/**
 * Evaluates a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self) -> IResult<Val> {
        self.op.eval(self.left.eval()?, self.right.eval()?, self.span);
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
