
/***************************************************************************
 * Sqrrl compiler error sub module defines a general error trait 
 * that can be used by many parts of the compiler.
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * The error struct in sqrrlc has a span and the kind of error.
 */
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}


/**
 * The different kinds of errors that can occur.
 */
pub enum ErrorKind {
    OutOfScope(&'static str, &'static [&'static str]),
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
            ErrorKind::OutOfScope(ident, items)
                => format!("cannot find {:?} `{}` in this scope", items, ident),
            ErrorKind::Context(ctx)
                => format!("{}", ctx),
        }
    }
}
