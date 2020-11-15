use std::fmt;
use std::cmp;
use crate::ast::*;
use crate::span::Span;
use crate::interp::{RuntimeEnv, IResult};

/**
 * The value returned from evaluating an AST Node.
 */
#[derive(Debug, Clone)]
pub struct Val {
    /// The actual data storage for the value.
    pub data: ValData,

    /// Optionally provide the identifier from where this value resides from.
    pub ident: Option<String>,

    /// The span informtion.
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

/***************************************************************************
 * Primitive data types implementations for integer
 ***************************************************************************/

/**
 * Defines different types of integer primitive values.
 */
#[derive(Debug, Clone, Copy)]
pub enum IntVal {
    /// 32 bit unsigned integer
    Int32(i32),
    /// 64 bit unsigned integer
    Int64(i64),
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
            IntVal::Int32(val) => Some(Val::from_i32(val + rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_i64(val + rhs.get_i64()?, span)),
        }
    }

    /**
     * Sub (subtraction) binary operation for integers.
     */
    pub fn sub(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_i32(val - rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_i64(val - rhs.get_i64()?, span)),
        }
    }

    /**
     * Div (division) binary operation for integers.
     */
    pub fn div(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_i32(val / rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_i64(val / rhs.get_i64()?, span)),
        }
    }

    /**
     * Mul (multiplication) binary operation for integers.
     */
    pub fn mul(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_i32(val * rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_i64(val * rhs.get_i64()?, span)),
        }
    }

    /**
     * Pow (power of) binary operation for integers.
     */
    pub fn pow(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_i32(val.pow(rhs.get_i32()? as u32), span)),
            IntVal::Int64(val) => Some(Val::from_i64(val.pow(rhs.get_i32()? as u32), span)),
        }
    }

    /**
     * Mod (modulo) binary operation for integers.
     */
    pub fn r#mod(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_i32(val % rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_i64(val % rhs.get_i64()?, span)),
        }
    }

    /**
     * Eq (equal) binary operation for integers.
     */
    pub fn eq(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_bool(val == rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_bool(val == rhs.get_i64()?, span)),
        }
    }

    /**
     * Ne (not equal) binary operation for integers.
     */
    pub fn ne(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_bool(val != rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_bool(val != rhs.get_i64()?, span)),
        }
    }

    /**
     * Lt (less than) binary operation for integers.
     */
    pub fn lt(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_bool(val < rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_bool(val < rhs.get_i64()?, span)),
        }
    }

    /**
     * Le (less than or equal)  binary operation for integers.
     */
    pub fn le(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_bool(val <= rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_bool(val <= rhs.get_i64()?, span)),
        }
    }

    /**
     * Gt (greater than) binary operation for integers.
     */
    pub fn gt(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_bool(val > rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_bool(val > rhs.get_i64()?, span)),
        }
    }

    /**
     * Ge (greater than or equal) binary operation for integers.
     */
    pub fn ge(self, rhs: IntVal, span: Span) -> Option<Val> {
        match self {
            IntVal::Int32(val) => Some(Val::from_bool(val >= rhs.get_i32()?, span)),
            IntVal::Int64(val) => Some(Val::from_bool(val >= rhs.get_i64()?, span)),
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
            IntVal::Int32(val) => Some(Val::from_i32(-val, span)),
            IntVal::Int64(val) => Some(Val::from_i64(-val, span)),
        }
    }

    /***********************************************************************
     * Helper methods
     ***********************************************************************/

    /**
     * Get the type information for this integer value.
     */
    pub fn get_type_kind(&self) -> TyKind {
        match self {
            IntVal::Int32(_) => TyKind::Int(IntTy::I32),
            IntVal::Int64(_) => TyKind::Int(IntTy::I64),
        }
    }

    /**
     * Get i32 value from integer value.
     */
    fn get_i32(&self) -> Option<i32> {
        match self {
            IntVal::Int32(val) => Some(*val),
            _ => None,
        }
    }

    /**
     * Get the i64 value.
     */
    fn get_i64(&self) -> Option<i64> {
        match self {
            IntVal::Int64(val) => Some(*val),
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
            IntVal::Int32(val) => write!(f, "{} (i32)", val),
            IntVal::Int64(val) => write!(f, "{} (i64)", val),
        }
    }
}

/**
 * Partial equality of referenced values.
 */
impl cmp::PartialEq for IntVal {
    fn eq(&self, other: &IntVal) -> bool {
        match self {
            IntVal::Int32(val) => *val == other.get_i32().unwrap(),
            IntVal::Int64(val) => *val == other.get_i64().unwrap(),
        }
    }
}

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

/**
 * Defines a reference value.
 */
#[derive(Debug, Clone)]
pub struct RefVal {
    /// The memory address where this reference points to.
    pub addr: usize,

    /// The type that this
    pub ref_ty: Ty,

    /// Is this a mutable reference?
    pub mutable: bool,
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
    pub fn deref(self, span: Span, env: &mut RuntimeEnv) -> IResult<Val> {
        match env.load_val(self.addr) {
            Ok(val_data) => {
                let val = Val::from_data(val_data, None, span);
                let val_ty = val.get_type();
                if val_ty != self.ref_ty {
                    let mut err = env.fatal_error(span, "mismatched type reference");
                    err.span_label(span, &format!("referenced type is {}, but it was actually {}", self.ref_ty, val_ty));
                    Err(err)
                } else {
                    Ok(val)
                }
            },
            Err(mut err) => {
                err.primary_span(span);
                Err(err)
            },
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
        let mut ty_prefix = String::from("&");
        if self.mutable {
            ty_prefix.push_str("mut ");
        }
        write!(f, "{} ({}{})", self.addr, ty_prefix, self.ref_ty)
    }
}
