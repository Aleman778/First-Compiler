
/***************************************************************************
 * Type checker implementation for base AST nodes.
 ***************************************************************************/


use crate::ast::{
    ty::Ty,
    base::*,
};
use crate::typeck::{
    TyCtxt,
    TypeChecker,
};


/**
 * Check the entire file for type errors.
 */
impl TypeChecker for File {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        for item in &self.items {
            item.check_type(tcx);
        }
        Ty::new()
    }
}


/**
 * Check this function item for type erros.
 */
impl TypeChecker for Item {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Item::Fn(func) => { func.check_type(tcx); },
            _ => { },
        };
        Ty::new()
    }
}


/**
 * Check the contents of this function for type errors.
 */
impl TypeChecker for FnItem {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let ret_ty = self.block.check_type(tcx);
        match tcx.sym.find_fn_symbol(&self.ident) {
            Some(fn_sym) => if fn_sym.output != ret_ty {
                if ret_ty.is_none() {
                    let mut err = struct_span_err!(tcx.sess, fn_sym.output.span, "mismatched types");
                    err.span_label(fn_sym.output.span, &format!("expected {} found ()", fn_sym.output));
                    if ret_ty.span.is_empty() {
                        err.span_label(
                            self.ident.span,
                            "implicitly returns `()` as its body has no tail or `return` expression"
                        );
                    } else {
                        err.span_label(ret_ty.span, "return type is not ()");
                    }
                    tcx.sess.emit(&err);
                } else {
                    mismatched_return_type_err!(tcx.sess, fn_sym, ret_ty);
                }
            },
            None => { }
        }
        
        Ty::new()
    }
}
