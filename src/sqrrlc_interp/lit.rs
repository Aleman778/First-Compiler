
/***************************************************************************
 * Literal interpreter implementation converts literals into
 * values that can be used the interpreter.
 ***************************************************************************/


use crate::sqrrlc_ast::lit::*;
use crate::sqrrlc_interp::{
    value::Val,
    env::RuntimeEnv,
    IResult,
    Eval,
};


/**
 * Evaluates a literal.
 */
impl Eval for Lit {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Lit::Int(literal) => literal.eval(env),
            Lit::Bool(literal) => literal.eval(env),
            Lit::Str(literal)  => Err(struct_span_fatal!(env.sess,
                                                         literal.span,
                                                         "unsupported literal type by the interpreter")),
        }
    }
}


/**
 * Evaluates an int literal.
 */
impl Eval for LitInt {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_i32(self.value, self.span))
    }
}


/**
 * Evaluates a boolean literal.
 */
impl Eval for LitBool {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_bool(self.value, self.span))
    }
}
