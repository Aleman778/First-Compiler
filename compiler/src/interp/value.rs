#![allow(dead_code)]

/***************************************************************************
 * Value submodule represents a value returned from evaluation.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    base::Type,
};
use crate::interp::primitive::{
    int::IntVal,
    bool::BoolVal,
    r#ref::RefVal,
};


/**
 * The value returned from evaluating an AST Node.
 */
#[derive(Debug, Clone)]
pub enum Val {
    /// Int is a 32-bit signed integer value
    Int(IntVal),
    
    /// Bool is a boolean value
    Bool(BoolVal),

    /// Reference is an unsigned integer value of either 32- or 64-bits
    Ref(RefVal),

    /// Void is an empty value
    Void(Span),

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
        Val::Int(IntVal::Int32 {
            val: val,
            span: span,
        })
    }


    /**
     * Constructs an i64 value.
     */
    pub fn from_i64(val: i64, span: Span) -> Self {
        Val::Int(IntVal::Int64 {
            val: val,
            span: span,
        })
    }

    
    /**
     * Constructs a bool value.
     */
    pub fn from_bool(val: bool, span: Span) -> Self {
        Val::Bool(BoolVal {
            val: val,
            span: span,
        })
    }


    /**
     * Constructs an reference value.
     */
    pub fn from_ref(addr: usize, span: Span) -> Self {
        Val::Ref(RefVal {
            addr: addr,
            span: span,
        })
    }
    

    /**
     * Returns true if the value is of type void, false otherwise.
     */
    pub fn is_void(&self) -> bool {
        match self {
            Val::Void(_) => true,
            _ => false,
        }
    }


    /**
     * Returns the integer value, if value is not of integer
     * type then a type error is returned instead.
     */
    pub fn get_int(&self) -> Option<IntVal> {
        match self {
            Val::Int(val) => Some(val.clone()),
            _ => None,
        }
    }

    
    /**
     * Returns the bool value, if value is not of boolean
     * type then a type error is returned instead.
     */    
    pub fn get_bool(&self) -> Option<BoolVal> {
        match self {
            Val::Bool(val) => Some(val.clone()),
            _ => None,
        }
    }
    

    /**
     * Returns the span information associated with the value.
     * Note span is empty if this has no value i.e. `Val::None`.
     */
    pub fn get_span(&self) -> Span {
        match self {
            Val::Int(val)   => val.get_span(),
            Val::Bool(val)  => val.span.clone(),
            Val::Ref(val)   => val.span.clone(),
            Val::Void(span) => span.clone(),
            Val::None       => Span::new_empty(),
        }
    }


    /**
     * Returns the type of this value.
     * TODO: Int value should not have type Int32?
     */
    pub fn get_type(&self) -> Type {
        match self {
            Val::Int(val) => Type::Int32{span: val.get_span().clone()},
            Val::Bool(val) => Type::Bool{span: val.span.clone()},
            _ => panic!("has no type"), //TODO: Add more flexible type system to avoid panic.
        }
    }
    

    /**
     * Perform the add binary operation.
     */
    pub fn add(&self, rhs: &Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.add(&rhs.get_int()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the sub binary operation.
     */
    pub fn sub(&self, rhs: &Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.sub(&rhs.get_int()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the div binary operation.
     */
    pub fn div(&self, rhs: &Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.div(&rhs.get_int()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the mul binary operation.
     */
    pub fn mul(&self, rhs: &Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.mul(&rhs.get_int()?, span),
            _ => None,
        }
    }


    /**
     * Perform the power of binary operation.
     */
    pub fn pow(&self, rhs: &Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.pow(&rhs.get_int()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the modulo binary operation.
     */
    pub fn modulo(&self, rhs: &Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.modulo(&rhs.get_int()?, span),
            _ => None,
        }
    }
}
