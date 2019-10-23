
/***************************************************************************
 * Primitive data type implementation for boolean
 ***************************************************************************/


use crate::ast::span::Span;
use crate::interp::value::Val;


/**
 * Defines the boolean value.
 */
#[derive(Debug, Clone)]
pub struct BoolVal {
    pub val: bool,
    pub span: Span,
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
}
