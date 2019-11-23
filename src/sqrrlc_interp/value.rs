#![allow(dead_code)]

/***************************************************************************
 * Value submodule represents a value returned from evaluation.
 ***************************************************************************/


use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::{
    span::Span,
    ty::*,
};
use crate::sqrrlc_interp::{
    env::RuntimeEnv,
    primitive::{
        int::IntVal,
        bool::BoolVal,
        r#ref::RefVal,
    },
    IResult,
};


/**
 * The value returned from evaluating an AST Node.
 */
#[derive(Debug, Clone)]
pub struct Val {
    /// The actual data storage for the value.
    pub data: ValData,

    /// Optionally provide the identifier from where this value resides from.
    pub ident: Option<String>,

    /// The span informtion, e
    pub span: Span,
}


/**
 * The value kind defines different types of values.
 */
#[derive(Debug, Clone)]
pub enum ValData {
    /// Int defines various different integer values
    Int(IntVal),
    
    /// Bool is a boolean value
    Bool(BoolVal),

    /// Reference is an unsigned integer value of either 32- or 64-bits
    Ref(RefVal),

    /// Void is an empty value
    Void,

    /// Continue has no value, it continues to next iteration.
    Continue,

    /// Break has no value, it breaks out of a loop.
    Break,
    
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
     * Create a new empty value containing no data and no span info.
     */
    pub fn new() -> Self {
        Val{data: ValData::None, ident: None, span: Span::new_empty()}
    }

    
    /**
     * Constructs an value containing data and span information.
     */
    pub fn from_data(data: ValData, ident: Option<String>, span: Span) -> Self {
        Val{data, ident, span}
    }

    
    /**
     * Constructs an i32 value.
     */
    pub fn from_i32(val: i32, span: Span) -> Self {
        Val {
            data: ValData::Int(IntVal::Int32(val)),
            ident: None,
            span,
        }
    }


    /**
     * Constructs an i64 value.
     */
    pub fn from_i64(val: i64, span: Span) -> Self {
        Val {
            data: ValData::Int(IntVal::Int64(val)),
            ident: None,
            span,
        }
    }

    
    /**
     * Constructs a bool value.
     */
    pub fn from_bool(val: bool, span: Span) -> Self {
        Val {
            data: ValData::Bool(BoolVal{val}),
            ident: None,
            span,
        }
    }


    /**
     * Constructs an reference value from the memory
     * address and the type of the value referenced to.
     */
    pub fn from_ref(addr: usize, ref_ty: Ty, mutable: bool, span: Span) -> Self {
        Val {
            data: ValData::Ref(RefVal {
                addr: addr,
                ref_ty,
                mutable
            }),
            ident: None,
            span,
        }
    }


    /***********************************************************************
     * Binary operations
     ***********************************************************************/
    

    /**
     * Perform the add binary operation.
     */
    pub fn add(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.add(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the sub binary operation.
     */
    pub fn sub(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.sub(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the div binary operation.
     */
    pub fn div(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.div(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the mul binary operation.
     */
    pub fn mul(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.mul(rhs.get_int_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the power of binary operation.
     */
    pub fn pow(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.pow(rhs.get_int_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the modulo binary operation.
     */
    pub fn r#mod(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.r#mod(rhs.get_int_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the logical and binary operation.
     */
    pub fn and(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Bool(val) => val.and(rhs.get_bool_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the logical or binary operation.
     */
    pub fn or(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Bool(val) => val.or(rhs.get_bool_val()?, span),
            _ => None,
        }
    }
    

    /**
     * Perform the equal binary operation.
     */
    pub fn eq(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.eq(rhs.get_int_val()?, span),
            ValData::Bool(val) => val.eq(rhs.get_bool_val()?, span),
            _ => None,
        }
    }


    /**
     * Perform the not equal binary operation.
     */
    pub fn ne(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.ne(rhs.get_int_val()?, span),
            ValData::Bool(val) => val.ne(rhs.get_bool_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the less than binary operation.
     */
    pub fn lt(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.lt(rhs.get_int_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the less equal binary operation.
     */
    pub fn le(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.le(rhs.get_int_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the greater than binary operation.
     */
    pub fn gt(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.gt(rhs.get_int_val()?, span),
            _ => None,
        }
    }

    
    /**
     * Perform the greater equal binary operation.
     */
    pub fn ge(self, rhs: Val, span: Span) -> Option<Self> {
        match self.data {
            ValData::Int(val) => val.ge(rhs.get_int_val()?, span),
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
        match self.data {
            ValData::Int(val) => val.neg(span),
            _ => None,
        } 
    }


    /**
     * Perform the logical inversion unary operation.
     */
    pub fn not(self, span: Span) -> Option<Self> {
        match self.data {
            ValData::Bool(val) => val.not(span),
            _ => None,
        } 
    }


    /**
     * Perform the dereferencing unary operation.
     */
    pub fn deref(self, span: Span, env: &mut RuntimeEnv) -> IResult<Option<Self>> {
        match self.data {
            ValData::Ref(r) => Ok(Some(r.deref(span, env)?)),
            _ => Ok(None),
        }
    }
    

    /***********************************************************************
     * Helper methods
     ***********************************************************************/


    /**
     * Returns true of this instance has a value.
     */
    pub fn has_data(&self) -> bool {
        self.data.has_data()
    }


    /**
     * Returns true if val is continue, otherwise false.
     */
    pub fn is_continue(&self) -> bool {
        if let ValData::Continue = self.data {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Returns true if val is break, otherwise false.
     */    
    pub fn is_break(&self) -> bool {
        if let ValData::Break = self.data {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Returns true if val is void, otherwise false.
     */
    pub fn is_void(&self) -> bool {
        if let ValData::Void = self.data {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Returns true if val is none, otherwise false.
     */
    pub fn is_none(&self) -> bool {
        if let ValData::None = self.data {
            return true;
        } else {
            return false;
        }
    }
    

    /**
     * Returns the integer value, if value is not of integer
     * type then None is returned instead.
     */
    pub fn get_int_val(&self) -> Option<IntVal> {
        match self.data {
            ValData::Int(val) => Some(val.clone()),
            _ => None,
        }
    }


    /**
     * Returns the i32 value, if value is not an i32
     * type then None is returned instead.
     */
    pub fn get_i32(&self) -> Option<i32> {
        match self.get_int_val()? {
            IntVal::Int32(val) => Some(val),
            _ => None,
        }
    }


    /**
     * Returns the i64 value, if value is not an i64
     * type then None is returned instead.
     */
    pub fn get_i64(&self) -> Option<i64> {
        match self.get_int_val()? {
            IntVal::Int64(val) => Some(val),
            _ => None,
        }
    }
    
    
    /**
     * Returns the bool value, if value is not of boolean
     * type then None is returned instead.
     */    
    pub fn get_bool_val(&self) -> Option<BoolVal> {
        match self.data {
            ValData::Bool(val) => Some(val.clone()),
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
     * Returns the reference value, if value is not of reference
     * type the None is returned instead.
     */
    pub fn get_ref_val(&self) -> Option<RefVal> {
        match &self.data {
            ValData::Ref(val) => Some(val.clone()),
            _ => None,
        }
    }


    /**
     * Returns the type of this value.
     */
    pub fn get_type(&self) -> Ty {
        let ty_kind = self.data.get_type_kind();
        Ty{kind: ty_kind, span: self.span}
    }
}


/**
 * Implementation of value data.
 */
impl ValData {
    /**
     * Returns true of this instance has a value.
     */
    pub fn has_data(&self) -> bool {
        match self {
            ValData::Continue |
            ValData::Break |
            ValData::Void |
            ValData::None => false,
            _ => true,
        }
    }


    pub fn get_type_kind(&self) -> TyKind {
        match self {
            ValData::Int(val) => val.get_type_kind(),
            ValData::Bool(_) => TyKind::Bool,
            ValData::Ref(val) => TyKind::Ref(
                TypeRef {
                    mutable: val.mutable,
                    elem: Box::new(val.ref_ty.clone()),
                }
            ),
            _ => TyKind::None,
        }
    }
}



/**
 * Display formatting for values.
 */
impl fmt::Display for ValData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValData::Int(val)  => write!(f, "{}", val),
            ValData::Bool(val) => write!(f, "{}", val),
            ValData::Ref(val)  => write!(f, "{}", val),
            ValData::Void      => write!(f, "Void"),
            ValData::Continue  => write!(f, "Continue"),
            ValData::Break     => write!(f, "Break"),
            ValData::None      => write!(f, "None"),
        }
    }
}


/**
 * Comparing partial equality of values.
 */
impl cmp::PartialEq for Val {
    fn eq(&self, other: &Val) -> bool {
        match &self.data {
            ValData::Int(val)  => val == &other.get_int_val().unwrap(),
            ValData::Bool(val) => val == &other.get_bool_val().unwrap(),
            ValData::Ref(val)  => val == &other.get_ref_val().unwrap(),
            ValData::Void      => other.is_void(),
            ValData::Continue  => other.is_continue(),
            ValData::Break     => other.is_break(),
            ValData::None      => other.is_none(),
        }
    }
}
