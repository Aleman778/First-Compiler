
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    base::Type,
    expr::*,
};
use crate::interp::{
    error::RuntimeError,
    scope::Scope,
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
            Expr::Assign(expr)   => expr.eval(env),
            Expr::Binary(expr)   => expr.eval(env),
            Expr::Block(expr)    => expr.eval(env),
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
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let val = (*self.expr).eval(env)?;
        env.assign_var(&self.ident, val)?;
        Ok(Val::None)
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


/**
 * Evaluates a block expression.
 */
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
 * Evaluates break expression. TODO: fix later.
 */
impl Eval for ExprBreak {
    fn eval(&self, _env: &mut Env) -> IResult<Val> {
        Ok(Val::Break(self.span.clone()))
    }
}


/**
 * Evaluates a function call.
 */
impl Eval for ExprCall {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let mut values = Vec::new();
        for arg in &self.args {
            let val = arg.eval(env)?;
            values.push(val);
        }
        let item = env.load_item(&self.ident)?;
        item.eval_func(values, env)
    }
}


/**
 * Evaluates continue expression.
 */
impl Eval for ExprContinue {
    fn eval(&self, _env: &mut Env) -> IResult<Val> {
        Ok(Val::Continue(self.span.clone()))
    }
}


/**
 * Evaluates to value by loading from memory using this identifier.
 */
impl Eval for ExprIdent {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        env.load_var(&self)
    }
}


/**
 * Evaluates an if statement.
 */
impl Eval for ExprIf {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let value = (*self.cond).eval(env)?;
        match value.get_bool() {
            Some(cond) => {
                if cond {
                    env.push_block(Scope::new())?;
                    let result = self.then_block.eval(env);
                    env.pop_block()?;
                    result
                } else {
                    match self.else_block.clone() {
                        Some(block) => {
                            env.push_block(Scope::new())?;
                            let result = block.eval(env);
                            env.pop_block()?;
                            result
                        },
                        None => Ok(Val::None),
                    }
                }
            },
            None => Err(RuntimeError::type_error(
                self.span.clone(), value.get_type(),
                Type::Bool{span: Span::new_empty()}
            )),
        }
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
 * Evaluates a parenthesized expression.
 */
impl Eval for ExprParen {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        (*self.expr).eval(env)
    }
}


/**
 * Evaluates a return statement.
 */
impl Eval for ExprReturn {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
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
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        let right = self.right.eval(env)?;
        self.op.eval(right, self.span.clone())
    }
}


/**
 * Evaluates a while loop.
 */
impl Eval for ExprWhile {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        loop {
            let value = (*self.cond).eval(env)?;
            match value.get_bool() {
                Some(cond) => {
                    if cond {
                        env.push_block(Scope::new())?;
                        let val = self.block.eval(env)?;
                        env.pop_block()?;
                        match val {
                            Val::Continue(_) => continue,
                            Val::Break(_) => break,
                            Val::None => continue,
                            _ => return Ok(val),
                        };
                    } else {
                        break;
                    }
                }
                None => return Err(RuntimeError::type_error(
                    self.span.clone(), value.get_type(),
                    Type::Bool{span: Span::new_empty()}
                )),
            };
        }
        Ok(Val::None)
    }
}
