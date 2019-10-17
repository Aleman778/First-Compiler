
/***************************************************************************
 * Value submodule represents a value returned from evaluation.
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * The value returned from evaluating an AST Node.
 */
pub enum Val {
    /// I32 is a 32-bit signed integer value
    I32 {
        val: i32,
        span: Span,
    },
    
    /// Bool is a boolean value
    Bool {
        val: bool,
        span: Span,
    },

    /// Addr is an unsigned integer value of either 32- or 64-bits
    Addr {
        addr: usize,
        span: Span,
    },

    /// Void is an empty value
    Void {
        span: Span,
    },
}


/**
 * Implementation of the value enum
 */
impl Val {
    /**
     * Constructs an i32 value.
     */
    pub fn from_i32(val: i32, span: Span) -> Self {
        Val::I32 {
            val: val,
            span: span,
        }
    }

    
    /**
     * Constructs a bool value.
     */
    pub fn from_bool(val: bool, span: Span) -> Self {
        Val::Bool {
            val: val,
            span: span,
        }
    }


    /**
     * Constructs an address value.
     */
    pub fn from_addr(addr: usize, span: Span) -> Self {
        Val::Addr {
            addr: addr,
            span: span,
        }
    }
    
    
    /**
     * Constructs a void value.
     */
    pub fn from_void(span: Span) -> Self {
        Val::Void {
            span: span,
        }
    }
    

    /**
     * Returns true if the value is of type void, false otherwise.
     */
    pub fn is_void(&self) -> bool {
        match self {
            Val::Void{span: _} => true,
            _ => false,
        }
    }
}
