
/***************************************************************************
 * Primitive data types implementations for integer
 ***************************************************************************/


use crate::ast::span::Span;
use crate::interp::value::Val;


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
 * Implementation of the integer values.
 */
impl IntVal {
    /**
     * Add binary operation for integers.
     */
    pub fn add(&self, rhs: &IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val + rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val + rhs.get_i64()?, span)),
        }
    }


    /**
     * Sub binary operation for integers.
     */
    pub fn sub(&self, rhs: &IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val - rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val - rhs.get_i64()?, span)),
        }
    }


    /**
     * Div binary operation for integers.
     */
    pub fn div(&self, rhs: &IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val / rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val / rhs.get_i64()?, span)),
        }
    }

    
    /**
     * Mul binary operation for integers.
     */
    pub fn mul(&self, rhs: &IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val * rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val * rhs.get_i64()?, span)),
        }
    }


    /**
     * Pow binary operation for integers.
     */
    pub fn pow(&self, rhs: &IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val.pow(rhs.get_i32()? as u32), span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val.pow(rhs.get_i32()? as u32), span)),
        }
    }

    
    /**
     * Mod binary operation for integers.
     */
    pub fn modulo(&self, rhs: &IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32{val, span: _} => Some(Val::from_i32(val % rhs.get_i32()?, span)),
            IntVal::Int64{val, span: _} => Some(Val::from_i64(val % rhs.get_i64()?, span)),
        }
    }


    /**
     * Returns the span information of the integer value.
     */
    pub fn get_span(&self) -> Span {
        match self {
            IntVal::Int32{val: _, span: span} => span.clone(),
            IntVal::Int64{val: _, span: span} => span.clone(),
        }
    }
    
    
    /**
     * Get i32 value from integer value
     */
    fn get_i32(&self) -> Option<i32> {
        match self {
            IntVal::Int32{val, span: _} => Some(*val),
            _ => None,
        }
    }


    /**
     * Get the i64 value
     */
    fn get_i64(&self) -> Option<i64> {
        match self {
            IntVal::Int64{val, span: _} => Some(*val),
            _ => None,
        }
    }
}
