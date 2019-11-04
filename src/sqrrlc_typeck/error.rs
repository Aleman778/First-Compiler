
/***************************************************************************
 * The type checker will return Type Errors is violations of the
 * type rules are found. The errors are stored in the environment.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    op::*,
    ty::Type,
};


/**
 * Type errors struct defines the span and kind.
 */
pub struct TypeError {
    pub span: Span,
    pub kind: ErrorKind,
}


/**
 * Implementation of type erros.
 */
impl TypeError {
    /**
     * Creates a new type error with the given span and error kind.
     */
    pub fn new(span: Span, kind: ErrorKind) -> Self {
        TypeError {
            span: span,
            kind: kind,
        }
    }
    

    /**
     * Creates a new error of kind UnexpectedType.
     */
    pub fn mismatched_types(expected: &Type, found: &Type) -> Self {
        TypeError {
            span: found.get_span(),
            kind: ErrorKind::MismatchedType(expected.clone(), found.clone()),
        }
    }
}


/**
 * The kind of type error that occured.
 */
pub enum ErrorKind {
    BinOpNotImplemented(BinOp, Type, Type),
    UnOpNotImplemented(UnOp, Type),
    MismatchedType(Type, Type),
}


/**
 * Implementation of the error kind enum.
 */
impl ErrorKind {
    /**
     * Returns a short description of what the error is.
     */
    pub fn description(&self) -> String {
        match self {
            ErrorKind::BinOpNotImplemented(op, left, right)
                => format!("cannot {} `{}` to `{}`", op, left, right),
            ErrorKind::UnOpNotImplemented(op, right)
                => format!("cannot {} `{}`", op, right),
            ErrorKind::MismatchedType(_, _)
                => String::from("mismatched types"),
        }
    }


    pub fn explanation(&self) -> String {
        match self {
            ErrorKind::BinOpNotImplemented(op, left, right)
                => format!("no implementation for `{} {} {}`", left, op.token(), right),
            ErrorKind::MismatchedType(expected, found)
                => format!("expected `{}`, found `{}`", expected, found),
            _ => String::new(),
        }
    }
}
