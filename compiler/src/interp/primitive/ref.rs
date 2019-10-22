
/***************************************************************************
 * Primitive data type implementation for references
 ***************************************************************************/


use crate::ast::span::Span;
use crate::interp::value::Val;


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
