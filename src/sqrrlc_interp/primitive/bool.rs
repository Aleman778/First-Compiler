
/***************************************************************************
 * Primitive data type implementation for boolean
 ***************************************************************************/

use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::{
    span::Span,
    ty::*,
};
use crate::sqrrlc_interp::value::Val;


/**
 * Defines the boolean value.
 */
#[derive(Debug, Clone, Copy)]
pub struct BoolVal {
    pub val: bool,
}


/**
 * Implementation of boolean value to provide operations.
 */
impl BoolVal {
    /***********************************************************************
     * Boolean binary operations
     ***********************************************************************/
    
    /**
     * And (logical and) binary operator for boolean values.
     */
    pub fn and(&self, rhs: BoolVal, span: Span) -> Option<Val> {
        Some(Val::from_bool(self.val && rhs.val, span))
    }

    
    /**
     * Or (logical or) binary operator for boolean values.
     */
    pub fn or(&self, rhs: BoolVal, span: Span) -> Option<Val> {
        Some(Val::from_bool(self.val || rhs.val, span))
    }


    /**
     * Eq (equal) binary operator for boolean values.
     */
    pub fn eq(self, rhs: BoolVal, span: Span) -> Option<Val> {
        Some(Val::from_bool(self.val == rhs.val, span))
    }


    /**
     * Ne (not equal) binary operator for boolean values.
     */
    pub fn ne(self, rhs: BoolVal, span: Span) -> Option<Val> {
        Some(Val::from_bool(self.val != rhs.val, span))
    }
    
    
    /***********************************************************************
     * Boolean unary operations
     ***********************************************************************/
    

    /**
     * Not (logical invesion) unary operator for boolean values.
     */
    pub fn not(&self, span: Span) -> Option<Val> {
        Some(Val::from_bool(!self.val, span))
    }


    /***********************************************************************
     * Helper methods
     ***********************************************************************/

    
    /**
     * Get the type information for this boolean value.
     */
    pub fn get_type_kind(&self) -> TyKind {
        TyKind::Bool
    }    
}


/**
 * Formatting of boolean values.
 */
impl fmt::Display for BoolVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}


/**
 * Partial equality of referenced values.
 */
impl cmp::PartialEq for BoolVal {
    fn eq(&self, other: &BoolVal) -> bool {
        self.val == other.val
    }
}
