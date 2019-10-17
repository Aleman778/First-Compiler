
/***************************************************************************
 * Interpreter module is used to evaluate an Abstract Syntax Tree.
 ***************************************************************************/


use crate::ast::span::Span;
use crate::interpreter::{
    error::RuntimeError,
    value::Val,
};


/**
 * Results used for the interpreter
 */
pub type IResult<'a, T, E = RuntimeError<'a>> = Result<T, E>;


/**
 * Eval trait is used to define the evaluation
 * function for each AST node.
 */
trait Eval {
    fn eval(&self) -> IResult<Val>;
}


pub mod error;
pub mod value;
pub mod env;
pub mod scope;
pub mod memory;
pub mod expr;
