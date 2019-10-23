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
    /***********************************************************************
     * Constructors
     ***********************************************************************/

    
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


    /***********************************************************************
     * Binary operations
     ***********************************************************************/
    

    /**
     * Perform the add binary operation.
     */
    pub fn add(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.add(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the sub binary operation.
     */
    pub fn sub(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.sub(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the div binary operation.
     */
    pub fn div(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.div(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the mul binary operation.
     */
    pub fn mul(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.mul(rhs.get_int_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the power of binary operation.
     */
    pub fn pow(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.pow(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the modulo binary operation.
     */
    pub fn r#mod(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.r#mod(rhs.get_int_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the logical and binary operation.
     */
    pub fn and(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Bool(val) => val.and(rhs.get_bool_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the logical or binary operation.
     */
    pub fn or(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Bool(val) => val.or(rhs.get_bool_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the equal binary operation.
     */
    pub fn eq(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.eq(rhs.get_int_val()?, span),
            Val::Bool(val) => val.eq(rhs.get_bool_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the not equal binary operation.
     */
    pub fn ne(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.ne(rhs.get_int_val()?, span),
            Val::Bool(val) => val.ne(rhs.get_bool_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the less than binary operation.
     */
    pub fn lt(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.lt(rhs.get_int_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the less equal binary operation.
     */
    pub fn le(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.le(rhs.get_int_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the greater than binary operation.
     */
    pub fn gt(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.gt(rhs.get_int_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the greater equal binary operation.
     */
    pub fn ge(self, rhs: Val, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.ge(rhs.get_int_val()?, span),
            _ => None,
        }
    }


    /***********************************************************************
     * Unary operations
     ***********************************************************************/
    

    /**
     * Perform the negation unary operation.
     */
    pub fn neg(self, span: Span) -> Option<Self> {
        match self {
            Val::Int(val) => val.neg(span),
            _ => None,
        } 
    }


    /**
     * Perform the logical inversion unary operation.
     */
    pub fn not(self, span: Span) -> Option<Self> {
        match self {
            Val::Bool(val) => val.not(span),
            _ => None,
        } 
    }


    /**
     * Perform the dereferencing unary operation.
     */
    pub fn deref(self, _span: Span) -> Option<Self> {
        unimplemented!();
    }


    /**
     * Perform the reference unary operation.
     */
    pub fn r#ref(self, _span: Span) -> Option<Self> {
        unimplemented!();
    }
    

    /***********************************************************************
     * Helper methods
     ***********************************************************************/
    

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
     * type then None is returned instead.
     */
    pub fn get_int_val(&self) -> Option<IntVal> {
        match self {
            Val::Int(val) => Some(val.clone()),
            _ => None,
        }
    }


    /**
     * Returns the i32 value, if value is not an i32
     * type then None is returned instead.
     */
    pub fn get_i32(&self) -> Option<i32> {
        match self.get_int_val()? {
            IntVal::Int32{val, span: _} => Some(val),
            _ => None,
        }
    }


    /**
     * Returns the i64 value, if value is not an i64
     * type then None is returned instead.
     */
    pub fn get_i64(&self) -> Option<i64> {
        match self.get_int_val()? {
            IntVal::Int64{val, span: _} => Some(val),
            _ => None,
        }
    }
    
    
    /**
     * Returns the bool value, if value is not of boolean
     * type then None is returned instead.
     */    
    pub fn get_bool_val(&self) -> Option<BoolVal> {
        match self {
            Val::Bool(val) => Some(val.clone()),
            _ => None,
        }
    }


    /**
     * Returns the actual boolean value, if value is of
     * boolean type then None is returned instead.
     */
    pub fn get_bool(&self) -> Option<bool> {
        Some(self.get_bool_val()?.val)
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
}
