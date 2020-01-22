
/***************************************************************************
 * Base evaluations is used to evaluate the main parts of the AST
 * e.g. the a file, function declarations etc.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    base::*,
    ty::*,
};
use crate::sqrrlc_interp::{
    env::RuntimeEnv,
    value::*,
    debug::*,
    IResult,
    Eval,
};


/**
 * Evaluates a source file and executes the main method.
 */
impl File {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv<'a>) {
        for item in &self.items {
            env.store_item(item.clone());
        }
        match env.load_main() {
            Ok(func) => {
                if let Err(diagnostic) = func.eval(Vec::new(), env) {
                    env.sess.emit(&diagnostic);
                    return;
                }
            },
            Err(diagnostic) => env.sess.emit(&diagnostic),
        };
    }
}


/**
 * Implementation of item.
 */
impl Item {
    /**
     * Evaluates a function item.
     */
    #[allow(unreachable_patterns)]
    pub fn eval_func<'a>(&self, values: Vec<Val>, env: &mut RuntimeEnv<'a>) -> IResult<Val> {
        match self {
            Item::Fn(func) => func.eval(values, env),
            Item::ForeignFn(func) => func.eval(values, env),
            _ => {
                let ident = self.get_ident();
                let mut err = struct_span_fatal!(
                    env.sess,
                    ident.span,
                    "cannot find function `{}` in this scope",
                    ident.to_string
                );
                err.span_label(ident.span, "not found in this scope");
                Err(err)
            },
        }
    }
}


/**
 * Evaluates a function item.
 */

impl FnItem {
    fn eval<'a>(&self, values: Vec<Val>, env: &mut RuntimeEnv<'a>) -> IResult<Val> {
        env.push_func(&self, values)?;
        let val = self.block.eval(env)?;
        env.pop_func()?;
        match val.data {
            ValData::Continue |
            ValData::Break => Err(struct_span_fatal!(env.sess, val.span, "cannot break outside loop")),
            _ => Ok(val),
        }
    }
}


/**
 * Evaluates a foreign function item.
 */
impl ForeignFnItem {
    fn eval(&self, values: Vec<Val>, env: &mut RuntimeEnv) -> IResult<Val> {
        match self.ident.to_string.as_str() {
            "trace" => {
                trace(env);
            },
            "print_int" => {
                match values[0].get_i32() {
                    Some(arg) => print_int(env, arg),
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
                };
            },
            "print_bool" => {
                match values[0].get_bool() {
                    Some(arg) => print_bool(env, arg),
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Bool, values[0].get_type())),
                };
            },
            _ => return Err(struct_fatal!(env.sess, "is not a debug function")),
        };
        Ok(Val::new())
    }
}
