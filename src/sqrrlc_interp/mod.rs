
/***************************************************************************
 * Interpreter module is used to evaluate an Abstract Syntax Tree.
 ***************************************************************************/


use crate::sqrrlc_interp::{
    error::RuntimeError,
    env::RuntimeEnv,
    value::Val,
};


/**
 * Results used for the interpreter
 */
pub type IResult<T, E = RuntimeError> = Result<T, E>;


/**
 * Eval trait is used to define the evaluation
 * function for each AST node.
 */
pub trait Eval {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val>;
}


pub mod error;
pub mod primitive;
pub mod value;
pub mod env;
pub mod scope;
pub mod memory;
pub mod base;
pub mod expr;
pub mod lit;
pub mod op;
pub mod debug;
