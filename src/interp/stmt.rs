
/***************************************************************************
 * Statement interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::ast::{
    stmt::*,
    expr::Expr,
};
use crate::interp::{
    value::*,
    env::RuntimeEnv,
    IResult,
    Eval,
};


/**
 * Evaluates the block expression with the given runtime env
 * and pushes the new block if the given new_scope is true.
 */
impl Eval for Block {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let mut ret_val = Val::new();
        for stmt in &self.stmts {
            let val = stmt.eval(env)?;
            match val.data {
                ValData::None => continue,
                _ => ret_val = val,
            };
            break;
        }
        Ok(ret_val)
    }
}


/**
 * Evaluates an statement
 */
impl Eval for Stmt {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Stmt::Local(local) => local.eval(env),
            Stmt::Item(_item) => Err(struct_fatal!(env.sess, "items in functions are not supported by the interpreter")),
            Stmt::Semi(expr) => {
                let ret_val = expr.eval(env)?;
                // Only perform explicit returns e.g. return expression.
                Ok(match expr {
                    Expr::Return(_) => ret_val,
                    Expr::Continue(_) => ret_val,
                    Expr::Break(_) => ret_val,
                    _ => Val::new(),
                })
            },
            Stmt::Expr(expr) => expr.eval(env),
        }
    }
}


/**
 * Evaluates a local variable assignment.
 */
impl Eval for Local {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = match &*self.init {
            Some(init) => {
                let val = init.eval(env)?;
                let val_ty = val.get_type();
                if val_ty != self.ty {
                    return Err(mismatched_types_fatal!(env.sess, val.span, self.ty, val_ty));
                }
                val
            }
            None => Val::new(),
        };
        env.store_var(&self.ident, self.mutable, &val)?;
        Ok(Val::new())
    }
}
