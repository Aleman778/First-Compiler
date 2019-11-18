
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    base::Item,
    expr::*,
};
use crate::sqrrlc_interp::{
    value::Val,
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
            Expr::Assign(expr)   => expr.eval(env),
            Expr::Binary(expr)   => expr.eval(env),
            Expr::Block(expr)    => expr.eval(env, true),
            Expr::Break(expr)    => expr.eval(env),
            Expr::Call(expr)     => expr.eval(env),
            Expr::Continue(expr) => expr.eval(env),
            Expr::Ident(expr)    => expr.eval(env),
            Expr::If(expr)       => expr.eval(env),
            Expr::Lit(expr)      => expr.eval(env),
            Expr::Local(expr)    => expr.eval(env), 
            Expr::Paren(expr)    => expr.eval(env),
            Expr::Return(expr)   => expr.eval(env),
            Expr::Unary(expr)    => expr.eval(env),
            Expr::While(expr)    => expr.eval(env),
        }
    }
}


/**
 * Evaluates an assignment expresson
 */
impl Eval for ExprAssign {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = (*self.expr).eval(env)?;
        env.assign_var(&self.ident, val)?;
        Ok(Val::None)
    }
}


/**
 * Evaluates a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;
        self.op.eval(env, left, right, self.span.clone())
    }
}


/**
 * Evaluates a block expression.
 */
impl ExprBlock {
    /**
     * Evaluates the block expression with the given runtime env
     * and pushes the new block if the given new_scope is true.
     */
    pub fn eval(&self, env: &mut RuntimeEnv, new_scope: bool) -> IResult<Val> {
        if new_scope {
            env.push_block(Scope::new(env.sess, self.span.clone()))?;
        }
        let mut ret_val = Val::None;
        for expr in &self.stmts {
            let val = expr.eval(env)?;
            match val {
                Val::None => continue,
                _ => ret_val = val,
            };
        }
        if new_scope {
            env.pop_block()?;
        }
        Ok(ret_val)
    }
}


/**
 * Evaluates break expression. TODO: fix later.
 */
impl Eval for ExprBreak {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::Break(self.span.clone()))
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
                // Include function call span label
                err.primary_span(self.span);
                let len = match item {
                    Item::Fn(func) => func.decl.inputs.len(),
                    Item::ForeignFn(func) => func.decl.inputs.len(),
                };
                err.span_label(self.span, &format!("expected {} parameters", len));
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
        Ok(Val::Continue(self.span.clone()))
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
                    let result = self.then_block.eval(env, true);
                    result
                } else {
                    match self.else_block.clone() {
                        Some(block) => {
                            let result = block.eval(env, true);
                            result
                        },
                        None => Ok(Val::None),
                    }
                }
            },
            None => {
                let mut err = struct_span_fatal!(env.sess, self.span, "mismatched_types");
                err.span_label(self.span, &format!("expected bool, found {}", value.get_type()));
                Err(err)
            },
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
 * Evaluates a local variable assignment.
 */
impl Eval for ExprLocal {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = (*self.init).eval(env)?;
        env.store_var(&self.ident, val)?;
        Ok(Val::None)
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
 * Evaluates a return statement.
 */
impl Eval for ExprReturn {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match &*self.expr {
            Some(expr) => expr.eval(env),
            None => Ok(Val::Void(self.span.clone())),
        }
    }
}

/**
 * Evaluates a unary expression.
 */
impl Eval for ExprUnary {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let right = self.right.eval(env)?;
        self.op.eval(env, right, self.span.clone())
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
                        let val = self.block.eval(env, true)?;
                        match val {
                            Val::Continue(_) => continue,
                            Val::Break(_) => break,
                            Val::None => continue,
                            _ => return Ok(val),
                        };
                    } else {
                        break;
                    }
                },
                None => {
                    let mut err = struct_span_fatal!(env.sess, self.span, "mismatched_types");
                    err.span_label(self.span, &format!("expected bool, found {}", value.get_type()));
                    return Err(err);
                },
            };
        }
        Ok(Val::None)
    }
}
