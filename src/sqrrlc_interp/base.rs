
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
            match item {
                Item::Fn(_) => env.store_item(item.clone()),
                Item::ForeignMod(module) => module.eval(env),
                _ => { },
            }
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
                    Some(arg) => print_int(arg),
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
                };
            },
            "print_bool" => {
                match values[0].get_bool() {
                    Some(arg) => print_bool(arg),
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Bool, values[0].get_type())),
                };
            },
            "assert" => {
                match values[0].get_bool() {
                    Some(arg) => assert(arg),
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Bool, values[0].get_type())),
                };
            },
            "assert_eq_int" => {
                match values[0].get_i32() {
                    Some(arg0) => match values[1].get_i32() {
                        Some(arg1) => assert_eq_int(arg0, arg1),
                        None => return Err(mismatched_types_fatal!(
                            env.sess, values[1].span, TyKind::Int(IntTy::I32), values[1].get_type()))
                    }
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
                };
            },
            "assert_eq_bool" => {
                match values[0].get_bool() {
                    Some(arg0) => match values[1].get_bool() {
                        Some(arg1) => assert_eq_bool(arg0, arg1),
                        None => return Err(mismatched_types_fatal!(
                            env.sess, values[1].span, TyKind::Bool, values[1].get_type()))
                    }
                    None => return Err(mismatched_types_fatal!(
                        env.sess, values[0].span, TyKind::Bool, values[0].get_type()))
                };
            },
            _ => return Err(struct_fatal!(env.sess, "is not a debug function")),
        };
        Ok(Val::new())
    }
}


impl ForeignModItem {
    fn eval(&self, env: &mut RuntimeEnv) {
        for item in &self.items {
            env.store_item(Item::ForeignFn(item.clone()));
        }
    }
}
