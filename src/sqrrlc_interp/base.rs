
/***************************************************************************
 * Base evaluations is used to evaluate the main parts of the AST
 * e.g. the a file, function declarations etc.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    base::*,
};
use crate::sqrrlc_interp::{
    value::Val,
    env::RuntimeEnv,
    debug::*,
    IResult,
};


/**
 * Evaluates a source file and executes the main method.
 */
impl File {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv<'a>) {
        for item in &self.items {
            env.store_item(item.clone());
        }
        match env.push_main() {
            Ok(func) => {
                if let Err(diagnostic) = func.block.eval(env, false) {
                    env.sess.emit(&diagnostic);
                    return;
                }
                if let Err(diagnostic) = env.pop_func() {
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
    pub fn eval_func(&self, values: Vec<Val>, env: &mut RuntimeEnv) -> IResult<Val> {
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
    fn eval(&self, values: Vec<Val>, env: &mut RuntimeEnv) -> IResult<Val> {
        env.push_func(&self, values)?;
        let result = self.block.eval(env, false)?;
        env.pop_func()?;
        Ok(result)
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
                    Some(arg) => print_int(arg),
                    None => {
                        let mut err = struct_span_fatal!(env.sess, self.span, "mismatched_types");
                        err.span_label(self.span, &format!("expected i32, found {}", values[0].get_type()));
                        return Err(err);
                    },
                };
            },
            "print_bool" => {
                match values[0].get_bool() {
                    Some(arg) => print_bool(arg),
                    None => {
                        let mut err = struct_span_fatal!(env.sess, self.span, "mismatched_types");
                        err.span_label(self.span, &format!("expected bool, found {}", values[0].get_type()));
                        return Err(err);
                    },
                };
            }
            _ => {
                return Err(struct_span_fatal!(env.sess, self.span, "not a debug function"));
            }
        };
        Ok(Val::new())
    }
}
