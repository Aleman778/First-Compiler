#![allow(dead_code)]

/***************************************************************************
 * The error submodule of the interpreter defines the types of errors
 * that can occur during program evaluation.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    op::*,
};
use crate::interp::value::Val;


/**
 * Runtime error stores information about an error
 * that can occur during evaluation of an AST node.
 */
#[derive(Debug)]
pub struct RuntimeError {
    span: Span,
    kind: ErrorKind,
}


/**
 * Error kind enum defines different types of runtime errors.
 */
#[derive(Debug)]
pub enum ErrorKind {
    ExprBinary(BinOp, Val, Val),
    ItemNotFound(&'static str),
    ValueNotFound(&'static str),
    Context(&'static str),
}
