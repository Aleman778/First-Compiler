
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::ast::{
    expr::*,
};
use crate::interp::{
    value::Val,
    env::Env,
    IResult,
    Eval,
};


/**
 * Evaluate an expression.
 */
impl Eval for Expr {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        match self {
            Expr::Binary(binary) => binary.eval(env),
            Expr::Lit(literal) => literal.eval(env),
            Expr::Local(local) => local.eval(env),
            Expr::Unary(unary) => unary.eval(env),
            _ => Ok(Val::None),
        }
    }
}


impl Eval for ExprAssign {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let val = (*self.expr).eval(env)?;
        
    }
}


impl Eval for  {
    fn eval(&self, env: &mut Env) -> IResult<Val> {

    }
}

/**
 * Evaluates a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;
        self.op.eval(left, right, self.span.clone())
    }
}


impl Eval for ExprBlock {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        for expr in &self.stmts {
            let val = expr.eval(env)?;
            match val {
                Val::None => continue,
                _ => return Ok(val),
            };
        }
        Ok(Val::None)
    }
}


/**
 * Evaluates a literal expression.
 */
impl Eval for ExprLit {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        self.lit.eval(env)
    }
}


/**
 * Evaluates a local variable assignment.
 */
impl Eval for ExprLocal {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let val = (*self.init).eval(env)?;
        env.store_var(&self.ident, val)?;
        Ok(Val::None)
    }
}



/**
 * Evaluates a unary expression.
 */
impl Eval for ExprUnary {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let right = self.right.eval(env)?;
        self.op.eval(right, self.span.clone())
    }
}
