
/***************************************************************************
 * Type checker implementation for expressions.
 ***************************************************************************/


use std::cmp::min;
use crate::sqrrlc_ast::{
    ty::*,
    expr::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    error::TypeError,
    TypeChecker,
};


/**
 * Genreal type checker implementation for expressions.
 */
impl TypeChecker for Expr {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        match self {
            Expr::Assign(expr) => expr.check_type(env),
            Expr::Binary(expr) => expr.check_type(env),
            Expr::Block(expr)  => expr.check_type(env),
            Expr::Call(expr)   => expr.check_type(env),
            Expr::Ident(expr)  => expr.check_type(env),
            Expr::If(expr)     => expr.check_type(env),
            Expr::Lit(expr)    => expr.check_type(env),
            Expr::Local(expr)  => expr.check_type(env),
            Expr::Paren(expr)  => expr.check_type(env),
            Expr::Return(expr) => expr.check_type(env),
            Expr::Unary(expr)  => expr.check_type(env),
            Expr::While(expr)  => expr.check_type(env),
            _ => Ty::new(),
        }
    }
}


/**
 * Type checker implementation for assignment expressions.
 */
impl TypeChecker for ExprAssign {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let prev = self.ident.check_type(env);
        let new = self.expr.check_type(env);
        if prev != new {
            env.err(TypeError::mismatched_types(prev.kind, &new));
        }
        Ty::new()
    }
}


/**
 * Type checker implementation for binary expressions.
 */
impl TypeChecker for ExprBinary {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let left = self.left.check_type(env);
        let right = self.right.check_type(env);
        self.op.check_type(&left, &right, env);
        left.clone()
    }
}


/**
 * Type checker implementation for block expressions.
 */
impl TypeChecker for ExprBlock {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        env.next_scope();
        for i in 0..self.stmts.len() {
            let ret_ty = self.stmts[i].check_type(env);
            if ret_ty.kind != TyKind::None {
                if i == self.stmts.len() - 1 {
                    env.check_ret_ty(ret_ty);
                } else {
                    env.err(TypeError::mismatched_types(TyKind::None, &ret_ty));
                }
            }
        }
        env.prev_scope();
        Ty::new()
    }
}


/**
 * Type checker implementation for function call expressions.
 */
impl TypeChecker for ExprCall {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        match env.table.find_fn_symbol(&self.ident) {
            Some(fn_sym) => {
                let fn_inputs = fn_sym.inputs.clone();
                let fn_output = fn_sym.output.clone();
                let mut arg_tys = Vec::new();
                for i in 0..min(self.args.len(), fn_inputs.len()) {
                    let ty = self.args[i].check_type(env);
                    arg_tys.push(ty);
                }
                for i in 0..min(arg_tys.len(), fn_inputs.len()) {
                    if fn_inputs[i] != arg_tys[i] {
                        env.err(TypeError::mismatched_types(fn_inputs[i].kind.clone(), &arg_tys[i]));
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
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        match env.table.find_var_symbol(&self) {
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
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let cond_type = (*self.cond).check_type(env);
        if cond_type.kind != TyKind::Bool {
            env.err(TypeError::mismatched_types(TyKind::Bool, &cond_type));
        }
        self.then_block.check_type(env);
        match &self.else_block {
            Some(block) => { block.check_type(env); },
            None => { },
        };
        Ty::new()
    }
}


/**
 * Type checker implementation for literal expressions.
 */
impl TypeChecker for ExprLit {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        self.lit.check_type(env)
    }
}


/**
 * Type checker implementation for let binding expressions.
 */
impl TypeChecker for ExprLocal {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let init_ty = self.init.check_type(env);
        if init_ty != self.ty {
            env.err(TypeError::mismatched_types(self.ty.kind.clone(), &init_ty));
        }
        Ty::new()
    }
}


/**
 * Type checker implementation for parenthesized expressions.
 */
impl TypeChecker for ExprParen {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        (*self.expr).check_type(env)
    }
}


/**
 * Type checker implementation for return expressions.
 */
impl TypeChecker for ExprReturn {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        match &*self.expr {
            Some(expr) => {
                let ret_ty = expr.check_type(env);
                env.check_ret_ty(ret_ty);
                Ty::new()
            },
            None => Ty::new(),
        }
    }
}


/**
 * Type checker implementation for unary expressions.
 */
impl TypeChecker for ExprUnary {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let ty = self.right.check_type(env);
        self.op.check_type(&ty, env);
        ty
    }
}


/**
 * Type checker implementation for while expressions.
 */
impl TypeChecker for ExprWhile {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let cond_type = self.cond.check_type(env);
        if cond_type.kind != TyKind::Bool {
            env.err(TypeError::mismatched_types(TyKind::Bool, &cond_type));
        }
        self.block.check_type(env);
        Ty::new()
    }
}
