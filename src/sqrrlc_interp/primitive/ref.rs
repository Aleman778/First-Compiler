
/***************************************************************************
 * Primitive data type implementation for references
 ***************************************************************************/


use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::{
    span::Span,
    ty::*,
};
use crate::sqrrlc_interp::{
    value::Val,
    env::RuntimeEnv,
    IResult,
};


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
                    let mut err = struct_span_fatal!(env.sess, span, "mismatched type reference");
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
