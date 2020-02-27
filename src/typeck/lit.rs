
/***************************************************************************
 * Type checker implementation for literals
 ***************************************************************************/


use crate::ast::{
    ty::*,
    lit::*,
};
use crate::typeck::{
    TyCtxt,
    TypeChecker,
};


/**
 * General type checking implementation for literals.
 */
impl TypeChecker for Lit {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Lit::Int(lit) => lit.check_type(tcx),
            Lit::Bool(lit) => lit.check_type(tcx),
            Lit::Str(lit) => {
                let err = struct_span_fatal!(tcx.sess,
                                             lit.span,
                                             "unsupported literal type by the type checker");
                tcx.sess.emit(&err);
                Ty::new()
            }
        }
    }
}


/**
 * Type checking implementation for literal integers.
 */
impl TypeChecker for LitInt {
    fn check_type(&self, _tcx: &mut TyCtxt) -> Ty {
        Ty {
            kind: TyKind::Int(IntTy::I32),
            span: self.span,
        }
    }
}


/**
 * Type checking implementation for literal boolean.
 */
impl TypeChecker for LitBool {
    fn check_type(&self, _tcx: &mut TyCtxt) -> Ty {
        Ty {
            kind: TyKind::Bool,
            span: self.span,
        }
    }
}
