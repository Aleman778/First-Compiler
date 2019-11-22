
/***************************************************************************
 * Type checker is a sub module of the semantic analyser and is used
 * check the program for type related errors.
 ***************************************************************************/


use crate::sqrrlc_ast::ty::Ty;
use crate::sqrrlc_typeck::{
    error::TypeError,
    env::TypeEnv,
};


/**
 * Type alias of result to use type errors as default.
 */
pub type IResult<T, E = TypeError> = Result<T, E>;


/**
 * The type checker trait should be implemented for every AST node
 * that handles types and can generate a type error.
 */
pub trait TypeChecker {
    fn check_type(&self, env: &mut TypeEnv) -> Ty;
}



pub mod env;
pub mod error;
pub mod base;
pub mod stmt;
pub mod expr;
pub mod lit;
pub mod op;
