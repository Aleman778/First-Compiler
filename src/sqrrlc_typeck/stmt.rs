
/***************************************************************************
 * Type checker implementation for statements.
 ***************************************************************************/


use crate::sqrrlc::symbol::{
    VarSymbol,
    Symbol,
};
use crate::sqrrlc_ast::{
    ty::*,
    stmt::*,
    expr::Expr,
};
use crate::sqrrlc_typeck::{
    TypeChecker,
    TyCtxt,
};


/**
 * Type checker implementation for block statements.
 */
impl TypeChecker for Block {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        tcx.sym.next_scope();
        let mut ret_ty = Ty::new();
        for i in 0..self.stmts.len() {
            ret_ty = self.stmts[i].check_type(tcx);
            if !ret_ty.is_none() {
                if i < self.stmts.len() - 1 {
                    mismatched_types_err!(tcx.sess, ret_ty.span, TyKind::None, &ret_ty);
                }
            }
        }
        tcx.sym.prev_scope();
        ret_ty.span = self.span.clone();
        ret_ty
    }
}


/**
 * Type checker for statements.
 */
impl TypeChecker for Stmt {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Stmt::Local(local) => local.check_type(tcx),
            Stmt::Item(item) => item.check_type(tcx),
            Stmt::Semi(expr) => {
                let ret_ty = expr.check_type(tcx);
                if let Expr::Return(_) = expr {
                    ret_ty
                } else {
                    Ty::new()
                }
            },
            Stmt::Expr(expr) => expr.check_type(tcx),
        }
    }
}


/**
 * Type checker implementation for let binding expressions.
 */
impl TypeChecker for Local {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match &*self.init {
            Some(init) => {
                let init_ty = init.check_type(tcx);
                let mut symbol = VarSymbol::new();
                symbol.ty = self.ty.clone();
                tcx.sym.push_symbol(&self.ident, Symbol::Var(symbol));
                if init_ty != self.ty {
                    println!("{:#?}", init_ty.span);
                    mismatched_types_err!(tcx.sess, init_ty.span, self.ty, init_ty);
                }
            },
            None => { },
        };
        Ty::new()
    }
}
