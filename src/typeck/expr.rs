
/***************************************************************************
 * Type checker implementation for expressions.
 ***************************************************************************/


use std::cmp::min;
use crate::ast::{
    ty::*,
    expr::*,
};
use crate::typeck::{
    TypeChecker,
    TyCtxt,
};


/**
 * Genreal type checker implementation for expressions.
 */
impl TypeChecker for Expr {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Expr::Assign(expr)    => expr.check_type(tcx),
            Expr::Binary(expr)    => expr.check_type(tcx),
            Expr::Block(expr)     => expr.check_type(tcx),
            Expr::Call(expr)      => expr.check_type(tcx),
            Expr::Ident(expr)     => expr.check_type(tcx),
            Expr::If(expr)        => expr.check_type(tcx),
            Expr::Lit(expr)       => expr.check_type(tcx),
            Expr::Paren(expr)     => expr.check_type(tcx),
            Expr::Reference(expr) => expr.check_type(tcx),
            Expr::Return(expr)    => expr.check_type(tcx),
            Expr::Unary(expr)     => expr.check_type(tcx),
            Expr::While(expr)     => expr.check_type(tcx),
            _ => Ty::new(),
        }
    }
}


/**
 * Type checker implementation for assignment expressions.
 */
impl TypeChecker for ExprAssign {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let prev = self.left.check_type(tcx);
        let new = self.right.check_type(tcx);
        if prev != new {
            mismatched_types_err!(tcx.sess, new.span, prev, new);
        }
        Ty::new()
    }
}


/**
 * Type checker implementation for binary expressions.
 */
impl TypeChecker for ExprBinary {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let left = self.left.check_type(tcx);
        let right = self.right.check_type(tcx);
        self.op.check_type(&left, &right, tcx);
        self.op.infer_type(&left, &right)
    }
}


/**
 * Type checker implementation for block expressions.
 */
impl TypeChecker for ExprBlock {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        self.block.check_type(tcx)
    }
}


/**
 * Type checker implementation for function call expressions.
 */
impl TypeChecker for ExprCall {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match tcx.sym.find_fn_symbol(&self.ident) {
            Some(fn_sym) => {
                let fn_inputs = fn_sym.inputs.clone();
                let fn_output = fn_sym.output.clone();
                let mut arg_tys = Vec::new();
                for i in 0..min(self.args.len(), fn_inputs.len()) {
                    let ty = self.args[i].check_type(tcx);
                    arg_tys.push(ty);
                }
                for i in 0..min(arg_tys.len(), fn_inputs.len()) {
                    if fn_inputs[i] != arg_tys[i] {
                        mismatched_types_err!(tcx.sess, arg_tys[i].span, fn_inputs[i], arg_tys[i]);
                    }
                }
                let mut out_ty = fn_output.clone();
                out_ty.span = self.span;
                out_ty
            },
            None => Ty::new(),
        }
    }
}


/**
 * Type checker implementation for identifier expressions.
 */
impl TypeChecker for ExprIdent {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match tcx.sym.find_var_symbol(&self) {
            Some(var) => {
                let mut id_ty = var.ty.clone();
                id_ty.span = self.span;
                id_ty
            },
            None => Ty::new(),
        }
    }
}


/**
 * Type checker implementation for if expressions.
 */
impl TypeChecker for ExprIf {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let cond_type = (*self.cond).check_type(tcx);
        if cond_type.kind != TyKind::Bool {
            mismatched_types_err!(tcx.sess, cond_type.span, TyKind::Bool, cond_type);
        }
        let then_ty = self.then_block.check_type(tcx);
        match &self.else_block {
            Some(block) => {
                let else_ty = block.check_type(tcx);
                if then_ty != else_ty {
                    mismatched_types_err!(tcx.sess, else_ty.span, then_ty, else_ty);
                }
            },
            None => { },
        };
        then_ty
    }
}


/**
 * Type checker implementation for literal expressions.
 */
impl TypeChecker for ExprLit {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        self.lit.check_type(tcx)
    }
}



/**
 * Type checker implementation for parenthesized expressions.
 */
impl TypeChecker for ExprParen {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        (*self.expr).check_type(tcx)
    }
}


/**
 * Type checker implementation for return expressions.
 */
impl TypeChecker for ExprReturn {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match &*self.expr {
            Some(expr) => {
                let ret_ty = expr.check_type(tcx);
                match tcx.sym.find_current_fn_symbol() {
                    Some(fn_sym) => if fn_sym.output != ret_ty {
                        mismatched_return_type_err!(tcx.sess, fn_sym, ret_ty);
                        fn_sym.output.clone()
                    } else {
                        ret_ty
                    }
                    None => ret_ty
                }
            }
            None => {
                let mut ret_ty = Ty::new();
                ret_ty.span = self.span;
                ret_ty
            }
        }
    }
}


/**
 * Type checker implementation for reference expressions.
 */
impl TypeChecker for ExprReference {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let ty = (*self.expr).check_type(tcx);
        if ty.is_none() {
            let mut err = struct_span_err!(tcx.sess, self.span, "cannot reference none type");
            err.span_label(ty.span, "expected a typed value here, got nothing");
            tcx.sess.emit(&err);
            Ty::new()
        } else {
            Ty {
                span: self.span,
                kind: TyKind::Ref(TypeRef {
                    mutable: self.mutable,
                    elem: Box::new(ty),
                }),
            }
        }
    }
}


/**
 * Type checker implementation for unary expressions.
 */
impl TypeChecker for ExprUnary {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let ty = self.right.check_type(tcx);
        self.op.check_type(ty, tcx)
    }
}


/**
 * Type checker implementation for while expressions.
 */
impl TypeChecker for ExprWhile {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let cond_type = self.cond.check_type(tcx);
        if cond_type.kind != TyKind::Bool {
            mismatched_types_err!(tcx.sess, cond_type.span, TyKind::Bool, cond_type);
        }
        self.block.check_type(tcx);
        Ty::new()
    }
}
