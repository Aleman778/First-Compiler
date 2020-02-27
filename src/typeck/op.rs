
/***************************************************************************
 * Type checker implementation for operators
 ***************************************************************************/


use crate::ast::{
    span::Span,
    ty::*,
    op::*,
};
use crate::typeck::{
    TyCtxt,
};


/**
 * Type checking implementation of binary operators.
 */
impl BinOp {
    /**
     * Check the types of the left and right expression agains the implemented
     * types supported for this operator. Reports an error if this operator
     * does not implement the given types.
     */
    pub fn check_type(&self, lhs_ty: &Ty, rhs_ty: &Ty, tcx: &mut TyCtxt) {
        let (span, ok) = match self {
            BinOp::Add{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Sub{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Mul{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Div{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Pow{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Mod{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::And{span} => (span, lhs_ty.check_bool(rhs_ty)),
            BinOp::Or{span}  => (span, lhs_ty.check_bool(rhs_ty)),
            BinOp::Eq{span}  => (span, lhs_ty == rhs_ty),
            BinOp::Ne{span}  => (span, lhs_ty == rhs_ty),
            BinOp::Lt{span}  => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Le{span}  => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Gt{span}  => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Ge{span}  => (span, lhs_ty.check_int(rhs_ty)),
        };
        if !ok {
            let mut err = struct_span_err!(tcx.sess, *span, "cannot {} `{}` to `{}`", self, lhs_ty, rhs_ty);
            err.span_label(*span, &format!("no implementation for `{} {} {}`", lhs_ty, self.token(), rhs_ty));
            tcx.sess.emit(&err);
        }
    }

    
    /**
     * Infer the type that is resulted from the operation.
     * Special case for using comparators, needs to convert type to bool.
     */
    pub fn infer_type(&self, lhs_ty: &Ty, rhs_ty: &Ty) -> Ty {
        match self {
            BinOp::Eq{span: _} |
            BinOp::Ne{span: _} |
            BinOp::Lt{span: _} | 
            BinOp::Le{span: _} |
            BinOp::Gt{span: _} |
            BinOp::Ge{span: _} => Ty {
                kind: TyKind::Bool,
                span: Span::from_bounds(
                    lhs_ty.span.start,
                    rhs_ty.span.end,
                    lhs_ty.span.loc,
                ),
            },
            _ => Ty {
                kind: lhs_ty.kind.clone(),
                span: Span::from_bounds(
                    lhs_ty.span.start,
                    rhs_ty.span.end,
                    lhs_ty.span.loc
                ),
            }
        }
    }
}


/**
 * Type checking implementation for unary operators.
 * 
 */
impl UnOp {
    /**
     * Check the type given agains the implemented types supported for this operator.
     * Reports an error if this operator is not implemented for the given type.
     */
    pub fn check_type(&self, mut ty: Ty, tcx: &mut TyCtxt) -> Ty {
        let (span, ok) = match self {
            UnOp::Neg{span} => (span, ty.is_int()),
            UnOp::Not{span} => (span, ty.kind == TyKind::Bool),
            UnOp::Deref{span} => match ty.get_ref() {
                Some(r) => {
                    ty = (*r.elem).clone();
                    (span, true)
                },
                None => (span, false),
            }
        };
        if !ok {
            let mut err = struct_span_err!(tcx.sess, *span, "type `{}` cannot be {}", ty, self);
            err.span_label(*span, &format!("no implementation for `{}{}`", self.token(), ty));
            tcx.sess.emit(&err);
        }
        ty
    }
}



/**
 * Check type operator implementations.
 */
impl Ty {
    /**
     * Checks if this is an integer type and if the rhs type
     * is also the same type of integer.
     */
    fn check_int(&self, rhs_ty: &Ty) -> bool {
        match &self.kind {
            TyKind::Int(int) =>
                match int {
                    IntTy::I32 => rhs_ty.is_i32(),
                    IntTy::I64 => rhs_ty.is_i64(),
                },
            _ => false,
        }
    }


    /**
     * Checks if this is a boolean type and that the rhs type is
     * also a boolean.
     */
    fn check_bool(&self, rhs_ty: &Ty) -> bool {
        match self.kind {
            TyKind::Bool => rhs_ty.is_bool(),
            _ => false,
        }
    }


    /**
     * Check if this type is an integer type.
     */
    fn is_int(&self) -> bool{
        match &self.kind {
            TyKind::Int(_) => true,
            _ => false,
        }
    }
}
