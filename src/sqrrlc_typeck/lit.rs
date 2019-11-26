
/***************************************************************************
 * Type checker implementation for literals
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::*,
    lit::*,
};
use crate::sqrrlc_typeck::{
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
