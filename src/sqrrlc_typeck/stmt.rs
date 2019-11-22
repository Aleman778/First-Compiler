
/***************************************************************************
 * Type checker implementation for statements.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::*,
    stmt::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    error::TypeError,
    TypeChecker,
};


/**
 * Type checker implementation for block statements.
 */
impl TypeChecker for Block {
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
 * Type checker for statements.
 */
impl TypeChecker for Stmt {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        match self {
            Stmt::Local(local) => local.check_type(env),
            Stmt::Item(item) => item.check_type(env),
            Stmt::Expr(expr) => expr.check_type(env),
        }
    }
}


/**
 * Type checker implementation for let binding expressions.
 */
impl TypeChecker for Local {
    fn check_type(&self, env: &mut TypeEnv) -> Ty {
        let init_ty = self.init.check_type(env);
        if init_ty != self.ty {
            env.err(TypeError::mismatched_types(self.ty.kind.clone(), &init_ty));
        }
        Ty::new()
    }
}
