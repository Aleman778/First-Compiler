#![allow(dead_code)]

/***************************************************************************
 * Value submodule represents a value returned from evaluation.
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * The value returned from evaluating an AST Node.
 */
#[derive(Debug, Clone)]
pub enum Val {
    /// I32 is a 32-bit signed integer value
    Int {
        val: i32,
        span: Span,
    },
    
    /// Bool is a boolean value
    Bool {
        val: bool,
        span: Span,
    },

    /// Reference is an unsigned integer value of either 32- or 64-bits
    Ref {
        addr: usize,
        span: Span,
    },

    /// Void is an empty value
    Void {
        span: Span,
    },

    /// None is nothing, denotes an unallocated value.
    None,
}


/**
 * Implementation of the value enum
 */
impl Val {
    /**
     * Constructs an i32 value.
     */
    pub fn from_i32(val: i32, span: Span) -> Self {
        Val::Int {
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
     * Constructs an reference value.
     */
    pub fn from_ref(addr: usize, span: Span) -> Self {
        Val::Ref {
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


    pub fn get_span(&self) -> Span {
        match self {
            Val::Int{val: _, span}  => span.clone(),
            Val::Bool{val: _, span} => span.clone(),
            Val::Ref{addr: _, span} => span.clone(),
            Val::Void{span}         => span.clone(),
            Val::None               => Span::new_empty(),
        }
    }
}
