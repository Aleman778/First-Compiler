
/***************************************************************************
 * Expression interpreter implementation defines evaluation methods
 * using the structural operational semantics for this language.
 ***************************************************************************/


use crate::ast::{
    op::UnOp,
    ty::TyKind,
    base::Item,
    expr::*,
};
use crate::interp::{
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


impl Expr {
    /**
     * Evaluates the memory address of a given expression.
     */
    fn eval_addr(&self, env: &mut RuntimeEnv) -> IResult<usize> {
        match self {
            Expr::Ident(ident) => {
                env.address_of(&ident, true)
            },
            Expr::Unary(unary) => {
                match unary.op {
                    UnOp::Deref{span: _} => {
                        let addr = (*unary.right).eval_addr(env)?;
                        match env.load_val(addr)? {
                            ValData::Ref(r) => {
                                if r.mutable {
                                    Ok(r.addr)
                                } else {
                                    let mut err = struct_span_fatal!(
                                        env.sess,
                                        self.get_span(),
                                        "cannot assign to variable using immutable reference");
                                    err.span_label(r.ref_ty.span, "help: consider changing this to be a mutable reference");
                                    err.span_label(self.get_span(), "variable is a reference, so data it refers to cannot be written");
                                    Err(err)
                                }
                            }
                            _ => Err(struct_fatal!(env.sess, "invalid expression")),
                        }
                    },
                    _ => Err(struct_fatal!(env.sess, "invalid expression")),
                }
            },
            _ => Err(struct_fatal!(env.sess, "invalid expression")),
        }
    }
}


/**
 * Evaluates an assignment expresson
 */
impl Eval for ExprAssign {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let addr = match (*self.left).eval_addr(env) {
            Ok(addr) => addr,
            Err(mut err) => {
                if err.message[0].text == "invalid expression" {
                    err.message[0].text = String::from("invalid left-hand side expression");
                    err.primary_span(self.span);
                    err.span_label(self.span, "left-hand of expression not valid");
                }
                return Err(err);
            }
        };
        let val = (*self.right).eval(env)?;
        env.assign_var(addr, &val)?;
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
        env.push_block(Scope::new(self.span))?;
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
        Ok(Val::from_data(ValData::Break, None, self.span))
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
                    _ => 0,
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
        Ok(Val::from_data(ValData::Continue, None, self.span))
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
                    env.push_block(Scope::new(self.span))?;
                    let result = self.then_block.eval(env);
                    env.pop_block()?;
                    result
                } else {
                    match self.else_block.clone() {
                        Some(block) => {
                            env.push_block(Scope::new(self.span))?;
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
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = (*self.expr).eval(env)?;
        if !val.has_data() {
            Ok(val)
        } else {
            let ref_ty = val.get_type();
            match val.ident {
                Some(name) => {
                    // Reference to an already existing variable.
                    let ident = ExprIdent {
                        to_string: name,
                        span: val.span,
                    };
                    let addr = env.address_of(&ident, true)?;
                    let new_val = Val::from_ref(addr, ref_ty, self.mutable, self.span);
                    Ok(new_val)
                },
                None => {
                    // Reference to a new variable without identifier.
                    let addr = env.store_val(&val)?;
                    let new_val = Val::from_ref(addr, ref_ty, self.mutable, self.span);
                    Ok(new_val)
                },
            }
        }
    }
}


/**
 * Evaluates a return statement.
 */
impl Eval for ExprReturn {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match &*self.expr {
            Some(expr) => expr.eval(env),
            None => Ok(Val::from_data(ValData::Void, None, self.span)),
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
                        env.push_block(Scope::new(self.span))?;
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
