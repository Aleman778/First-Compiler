
/***************************************************************************
 * Literal interpreter implementation converts literals into
 * values that can be used the interpreter.
 ***************************************************************************/


use crate::ast::lit::*;
use crate::interp::{
    value::Val,
    IResult,
    Eval,
};


/**
 * Evaluates a literal.
 */
impl Eval for Lit {
    fn eval(&self) -> IResult<Val> {
        match self {
            Lit::Int(literal) => literal.eval(),
            Lit::Bool(literal) => literal.eval(),
        }
    }
}


/**
 * Evaluates an int literal.
 */
impl Eval for LitInt {
    fn eval(&self) -> IResult<Val> {
        Ok(Val::from_i32(self.value, self.span))
    }
}


/**
 * Evaluates a boolean literal.
 */
impl Eval for LitBool {
    fn eval(&self) -> IResult<Val> {
        Ok(Val::from_bool(self.value, self.span))
    }
}
