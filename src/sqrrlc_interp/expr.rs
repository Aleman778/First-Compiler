
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::TyKind,
    base::Item,
    expr::*,
};
use crate::sqrrlc_interp::{
    value::*,
    scope::Scope,
    env::RuntimeEnv,
    IResult,
    Eval,
};


/**
 * Evaluate an expression.
 */
impl Eval for Expr {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Expr::Assign(expr)    => expr.eval(env),
            Expr::Binary(expr)    => expr.eval(env),
            Expr::Block(expr)     => expr.eval(env),
            Expr::Break(expr)     => expr.eval(env),
            Expr::Call(expr)      => expr.eval(env),
            Expr::Continue(expr)  => expr.eval(env),
            Expr::Ident(expr)     => expr.eval(env),
            Expr::If(expr)        => expr.eval(env),
            Expr::Lit(expr)       => expr.eval(env),
            Expr::Paren(expr)     => expr.eval(env),
            Expr::Reference(expr) => expr.eval(env),
            Expr::Return(expr)    => expr.eval(env),
            Expr::Unary(expr)     => expr.eval(env),
            Expr::While(expr)     => expr.eval(env),
        }
    }
}


/**
 * Evaluates an assignment expresson
 */
impl Eval for ExprAssign {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = (*self.expr).eval(env)?;
        env.assign_var(&self.ident, &val)?;
        Ok(Val::new())
    }
}


/**
 * Evaluates a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;
        self.op.eval(env, left, right, self.span)
    }
}


/**
 * Evaluates a block expression.
 */
impl Eval for ExprBlock {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        env.push_block(Scope::new(env.sess, self.span))?;
        let ret_val = self.block.eval(env)?;
        env.pop_block()?;
        Ok(ret_val)
    }
}


/**
 * Evaluates break expression.
 */
impl Eval for ExprBreak {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_data(ValData::Break, self.span))
    }
}


/**
 * Evaluates a function call.
 */
impl Eval for ExprCall {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let mut values = Vec::new();
        for arg in &self.args {
            let val = arg.eval(env)?;
            values.push(val);
        }
        let item = env.load_item(&self.ident)?;
        match item.eval_func(values, env) {
            Ok(val) => Ok(val),
            Err(mut err) => {
                let len = match item {
                    Item::Fn(func) => func.decl.inputs.len(),
                    Item::ForeignFn(func) => func.decl.inputs.len(),
                };
                if self.args.len() != len {
                    err.primary_span(self.span);
                    err.span_label(self.span, &format!("expected {} parameters", len));
                }
                Err(err)
            },
        }
    }
}


/**
 * Evaluates continue expression.
 */
impl Eval for ExprContinue {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_data(ValData::Continue, self.span))
    }
}


/**
 * Evaluates to value by loading from memory using this identifier.
 */
impl Eval for ExprIdent {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        env.load_var(&self)
    }
}


/**
 * Evaluates an if statement.
 */
impl Eval for ExprIf {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let value = (*self.cond).eval(env)?;
        match value.get_bool() {
            Some(cond) => {
                if cond {
                    env.push_block(Scope::new(env.sess, self.span))?;
                    let result = self.then_block.eval(env);
                    env.pop_block()?;
                    result
                } else {
                    match self.else_block.clone() {
                        Some(block) => {
                            env.push_block(Scope::new(env.sess, self.span))?;
                            let result = block.eval(env);
                            env.pop_block()?;
                            result
                        },
                        None => Ok(Val::new()),
                    }
                }
            },
            None => Err(mismatched_types_fatal!(env.sess, value.span, TyKind::Bool, value.get_type())),
        }
    }
}


/**
 * Evaluates a literal expression.
 */
impl Eval for ExprLit {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        self.lit.eval(env)
    }
}


/**
 * Evaluates a parenthesized expression.
 */
impl Eval for ExprParen {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        (*self.expr).eval(env)
    }
}


/**
 * Evaluates a reference expression.
 */
impl Eval for ExprReference {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        // let val = (*self.expr).eval(env)?;
        // Ok(Val::from_ref())
        Ok(Val::new())
    }
}


/**
 * Evaluates a return statement.
 */
impl Eval for ExprReturn {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match &*self.expr {
            Some(expr) => expr.eval(env),
            None => Ok(Val::from_data(ValData::Void, self.span)),
        }
    }
}

/**
 * Evaluates a unary expression.
 */
impl Eval for ExprUnary {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let right = self.right.eval(env)?;
        self.op.eval(env, right, self.span)
    }
}


/**
 * Evaluates a while loop.
 */
impl Eval for ExprWhile {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        loop {
            let value = (*self.cond).eval(env)?;
            match value.get_bool() {
                Some(cond) => {
                    if cond {
                        env.push_block(Scope::new(env.sess, self.span))?;
                        let val = self.block.eval(env)?;
                        env.pop_block()?;
                        match val.data {
                            ValData::Continue => continue,
                            ValData::Break => break,
                            ValData::None => continue,
                            _ => return Ok(val),
                        };
                    } else {
                        break;
                    }
                },
                None => {
                    return Err(mismatched_types_fatal!(env.sess, value.span, TyKind::Bool, value.get_type()));
                },
            };
        }
        Ok(Val::new())
    }
}
