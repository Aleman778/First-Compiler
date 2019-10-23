#![allow(dead_code)]

/***************************************************************************
 * The error submodule of the interpreter defines the types of errors
 * that can occur during program evaluation.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    expr::ExprIdent,
    base::Type,
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
    pub kind: ErrorKind,
}


/**
 * Implementation of runtime errors provides
 * factory methods for creating error messages.
 */
impl RuntimeError {
    /**
     * Runtime error for binary expressions used
     * when operator does not implement the two value types.
     */
    pub fn binary_expr(span: Span, op: BinOp, right: Val, left: Val) -> Self {
        RuntimeError {
            span: span,
            kind: ErrorKind::BinaryExpr(op, left, right),
        }
    }
    
    /**
     * Runtime error for describing that a particular
     * item identifier of specific type is not found in this scope.
     */
    pub fn item_not_found(ident: &ExprIdent, item_types: &'static [&'static str]) -> Self {
        RuntimeError {
            span: ident.span.clone(),
            kind: ErrorKind::ItemNotFound(ident.to_string.clone(), item_types),
        }
    }


    /**
     * Memory errors occur when memory unsafe operations are
     * executed, borrow checker will prevent these.
     */
    pub fn memory_error(span: Span, msg: &'static str) -> Self {
        RuntimeError {
            span: span,
            kind: ErrorKind::MemoryError(msg),
        }
    }


    /**
     * Type errors occur when the expected type differs
     * from the actual type, type checker will prevent these.
     */
    pub fn type_error(span: Span, expected: Type, actual: Type) -> Self {
        RuntimeError {
            span: span,
            kind: ErrorKind::TypeError(expected, actual),
        }
    }
    

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
}


/**
 * Error kind enum defines different types of runtime errors.
 */
#[derive(Debug)]
pub enum ErrorKind {
    BinaryExpr(BinOp, Val, Val),
    ItemNotFound(String, &'static [&'static str]),
    MemoryError(&'static str),
    TypeError(Type, Type),
    Context(&'static str),
}


/**
 * Implementation of ErrorKind.
 */
impl ErrorKind {
    /**
     * Converts ErrorKind into a text description.
     */
    pub fn description(&self) -> String {
        match self {
            ErrorKind::BinaryExpr(op, left, right)
                => format!("cannot {} `{}` to `{}`", op, left.get_type(), right.get_type()),
            ErrorKind::ItemNotFound(id, items)
                => format!("cannot find {:?} `{}` in this scope", items, id),
            ErrorKind::MemoryError(msg)
                => format!("[memory error] {}", msg),
            ErrorKind::TypeError(expect, actual)
                => format!("[type error] expected {}, got {}", expect, actual),
            ErrorKind::Context(ctx)
                => format!("{}", ctx),
        }
    }


    /**
     * Converts ErrorKind into an error code.
     */
    pub fn error_code(&self) -> u32 {
        match self {
            ErrorKind::BinaryExpr(_, _, _) => 1,
            ErrorKind::ItemNotFound(_, _)  => 2,
            ErrorKind::MemoryError(_)      => 3,
            ErrorKind::TypeError(_, _)     => 4,
            ErrorKind::Context(_)          => 5,
        }
    }
}
