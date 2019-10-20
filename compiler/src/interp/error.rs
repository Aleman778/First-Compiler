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
 * Implementation of runtime errors provides
 * factory methods for creating error messages.
 */
impl RuntimeError {
    /**
     * Runtime error with a given context message 
     * explaining what caused the error to occur.
     */
    pub fn context(span: Span, msg: &'static str) -> Self {
        RuntimeError {
            span: span,
            kind: ErrorKind::Context(msg),
        }
    }

    
    /**
     * Runtime error for describing that a particular
     * item identifier of specific type is not found in this scope.
     */
    pub fn item_not_found(span: Span, item: &'static str, item_types: &[&'static str]) -> Self {
        RuntimeError {
            span: span,
            kind: ErrorKind::ItemNotFound(item, item_types),
        }
    }
}


/**
 * Error kind enum defines different types of runtime errors.
 */
#[derive(Debug)]
pub enum ErrorKind {
    ExprBinary(BinOp, Val, Val),
    ItemNotFound(&'static str, &'static [&'static str]),
    Context(&'static str),
}


/**
 * Implementation of ErrorKind.
 */
impl ErrorKind {
    /**
     * Converts ErrorKind into a text description.
     */
    pub fn description(&self) -> &str {
        match self {
            ErrorKind::ExprBinary(op, left, right)
                => format!("cannot {:?} `{:?}` to `{:?}`", op, left, right),
            ErrorKind::ItemNotFound(id, items)
                => format!("cannot find {:?} `{}` in this scope", items, id),
            ErrorKind::Context(ctx) => format!("{}", ctx),
        }
    }
}
