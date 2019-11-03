
/***************************************************************************
 * Type checker is a sub module of the semantic analyser and is used
 * check the program for type related errors.
 ***************************************************************************/


use crate::ast::ty::Type;
use crate::type_checker::{
    env::TypeEnv,
};


/**
 * Type checker trait should be implemented for 
 * each AST node that evaluates to a type or handles types.
 */
pub trait TypeChecker {
    fn check_type(&self, env: &mut TypeEnv) -> Type;
}


pub mod env;
pub mod error;
pub mod expr;
pub mod lit;
pub mod op;
pub mod symbol_table;
