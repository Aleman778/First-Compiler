
/***************************************************************************
 * Primitive data types implementations for integer
 ***************************************************************************/

use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::{
    span::Span,
    ty::*,
};
use crate::sqrrlc_interp::value::Val;


/**
 * Defines different types of integer primitive values.
 */
#[derive(Debug, Clone)]
pub enum IntVal {
    /// 32 bit unsigned integer
    Int32{val: i32, span: Span},

    /// 64 bit unsigned integer
    Int64{val: i64, span: Span},
}


/**
 * Implementation of the integer values and their
 * respective operation implementations.
 */
impl IntVal {
    /***********************************************************************
     * Integer binary operations
     ***********************************************************************/

    
    /**
     * Add (addition) binary operation for integers.
     */
    pub fn add(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val + rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val + rhs.get_i64()?, span)),
        }
    }


    /**
     * Sub (subtraction) binary operation for integers.
     */
    pub fn sub(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val - rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val - rhs.get_i64()?, span)),
        }
    }


    /**
     * Div (division) binary operation for integers.
     */
    pub fn div(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val / rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val / rhs.get_i64()?, span)),
        }
    }

    
    /**
     * Mul (multiplication) binary operation for integers.
     */
    pub fn mul(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val * rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val * rhs.get_i64()?, span)),
        }
    }


    /**
     * Pow (power of) binary operation for integers.
     */
    pub fn pow(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val.pow(rhs.get_i32()? as u32), span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val.pow(rhs.get_i32()? as u32), span)),
        }
    }

    
    /**
     * Mod (modulo) binary operation for integers.
     */
    pub fn r#mod(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val % rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val % rhs.get_i64()?, span)),
        }
    }


    /**
     * Eq (equal) binary operation for integers.
     */
    pub fn eq(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_bool(val == rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_bool(val == rhs.get_i64()?, span)),
        }
    }


    /**
     * Ne (not equal) binary operation for integers.
     */
    pub fn ne(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_bool(val != rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_bool(val != rhs.get_i64()?, span)),
        }
    }


    /**
     * Lt (less than) binary operation for integers.
     */
    pub fn lt(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_bool(val < rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_bool(val < rhs.get_i64()?, span)),
        }
    }
    

    /**
     * Le (less than or equal)  binary operation for integers.
     */
    pub fn le(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_bool(val <= rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_bool(val <= rhs.get_i64()?, span)),
        }
    }

    
    /**
     * Gt (greater than) binary operation for integers.
     */
    pub fn gt(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_bool(val > rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_bool(val > rhs.get_i64()?, span)),
        }
    }

    
    /**
     * Ge (greater than or equal) binary operation for integers.
     */
    pub fn ge(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_bool(val >= rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_bool(val >= rhs.get_i64()?, span)),
        }
    }


    /***********************************************************************
     * Integer unary operations
     ***********************************************************************/


    /**
     * Neg (negation) unary operation for integers.
     */
    pub fn neg(self, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(-val, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(-val, span)),
        }
    }
    

    
    /***********************************************************************
     * Helper methods
     ***********************************************************************/
    
    
    /**
     * Returns the span information of the integer value.
     */
    pub fn get_span(&self) -> Span {
        match self {
            IntVal::Int32{val: _, span} => span.clone(),
            IntVal::Int64{val: _, span} => span.clone(),
        }
    }


    /**
     * Get the type information for this integer value.
     */
    pub fn get_type(&self) -> Ty {
        match self {
            IntVal::Int32{val: _, span} => Ty {
                kind: TyKind::Int(IntTy::I32),
                span: span.clone(),
            },
            IntVal::Int64{val: _, span} => Ty {
                kind: TyKind::Int(IntTy::I64),
                span: span.clone(),
            },
        }
    }
    
    
    /**
     * Get i32 value from integer value.
     */
    fn get_i32(&self) -> Option<i32> {
        match self {
            IntVal::Int32{val, span: _} => Some(*val),
            _ => None,
        }
    }


    /**
     * Get the i64 value.
     */
    fn get_i64(&self) -> Option<i64> {
        match self {
            IntVal::Int64{val, span: _} => Some(*val),
            _ => None,
        }
    }
}


/**
 * Formatting display of integer values e.g. 34i32
 */
impl fmt::Display for IntVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntVal::Int32{val, span: _} => write!(f, "{} (i32)", val),
            IntVal::Int64{val, span: _} => write!(f, "{} (i64)", val),
        }
    }
}


/**
 * Partial equality of referenced values.
 */
impl cmp::PartialEq for IntVal {
    fn eq(&self, other: &IntVal) -> bool {
        match self {
            IntVal::Int32{val, span: _} => *val == other.get_i32().unwrap(),
            IntVal::Int64{val, span: _} => *val == other.get_i64().unwrap(),
        }
    }
}
