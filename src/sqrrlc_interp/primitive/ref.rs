
/***************************************************************************
 * Primitive data type implementation for references
 ***************************************************************************/


use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::span::Span;
use crate::sqrrlc_interp::value::Val;


/**
 * Defines a reference value.
 */
#[derive(Debug, Clone)]
pub struct RefVal {
    pub addr: usize,
    pub span: Span,
}


/**
 * Implementation of reference to provide operations.
 */
impl RefVal {
    /**
     * Dereferencing unary operator.
     */
    pub fn deref(self) -> Option<Val> {
        unimplemented!();
    }
}


/**
 * Partial equality of referenced values.
 */
impl cmp::PartialEq for RefVal {
    fn eq(&self, other: &RefVal) -> bool {
        self.addr == other.addr
    }
}


/**
 * Formatting of references.
 */
impl fmt::Display for RefVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ref {}", self.addr)
    }
}
