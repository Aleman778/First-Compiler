
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
            Expr::Lit(literal) => literal.eval(),
            _ => Ok(Val::None),
        }
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
