
/***************************************************************************
 * Primitive data type implementation for references
 ***************************************************************************/


use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::{
    span::Span,
    ty::*,
};
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
    /***********************************************************************
     * Reference unary operations
     ***********************************************************************/

    
    /**
     * Dereferencing unary operator.
     */
    pub fn deref(self) -> Option<Val> {
        unimplemented!();
    }


    /***********************************************************************
     * Helper methods
     ***********************************************************************/
    
    
    /**
     * Get the type information for this boolean value.
     * TODO: fix type kind should be reference.
     */
    pub fn get_type(&self) -> Ty {
        Ty {
            kind: TyKind::None,
            span: self.span.clone(),
        }
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
