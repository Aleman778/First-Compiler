
/***************************************************************************
 * Literal interpreter implementation converts literals into
 * values that can be used the interpreter.
 ***************************************************************************/


use crate::ast::lit::*;
use crate::interp::{
    value::Val,
    env::Env,
    IResult,
    Eval,
};


/**
 * Evaluates a literal.
 */
impl Eval for Lit {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        match self {
            Lit::Int(literal) => literal.eval(env),
            Lit::Bool(literal) => literal.eval(env),
        }
    }
}


/**
 * Evaluates an int literal.
 */
impl Eval for LitInt {
    fn eval(&self, _env: &mut Env) -> IResult<Val> {
        Ok(Val::from_i32(self.value, self.span.clone()))
    }
}


/**
 * Evaluates a boolean literal.
 */
impl Eval for LitBool {
    fn eval(&self, _env: &mut Env) -> IResult<Val> {
        Ok(Val::from_bool(self.value, self.span.clone()))
    }
}
