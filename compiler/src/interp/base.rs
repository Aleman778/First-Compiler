
/***************************************************************************
 * Base evaluations is used to evaluate the main parts of the AST
 * e.g. the a file, function declarations etc.
 ***************************************************************************/


use crate::ast::{
    base::*,
};
use crate::interp::{
    error::RuntimeError,
    value::Val,
    env::Env,
    debug::*,
    IResult,
    Eval,
};


/**
 * Evaluates a source file and executes the main method.
 */
impl Eval for File {
    fn eval(&self, env: &mut Env) -> IResult<Val> {
        for item in &self.items {
            env.store_item(item.clone());
        }
        let func = env.push_main()?;
        func.block.eval(env)?;
        env.pop_func()?;
        Ok(Val::None)
    }
}


/**
 * Implementation of item.
 */
impl Item {
    /**
     * Evaluates a function item.
     */
    pub fn eval_func(&self, values: Vec<Val>, env: &mut Env) -> IResult<Val> {
        match self {
            Item::Fn(func) => func.eval(values, env),
            Item::ForeignFn(func) => func.eval(values, env),
            _ => Err(RuntimeError::item_not_found(&self.get_ident(), &["function"]))
        }
    }
}


/**
 * Evaluates a function item.
 */

impl FnItem {
    fn eval(&self, values: Vec<Val>, env: &mut Env) -> IResult<Val> {
        env.push_func(&self, values);
        let result = self.block.eval(env)?;
        env.pop_func();
        Ok(result)
    }
}


/**
 * Evaluates a foreign function item.
 */
impl ForeignFnItem {
    fn eval(&self, values: Vec<Val>, env: &mut Env) -> IResult<Val> {
        match self.ident.to_string.as_str() {
            "trace" => {
                trace(env);
            },
            "print_int" => {
                match values[0].get_i32() {
                    Some(arg) => print_int(arg),
                    None => return Err(RuntimeError::context(self.span.clone(), "expected i32")),
                };
            },
            "print_bool" => {
                match values[0].get_bool() {
                    Some(arg) => print_bool(arg),
                    None => return Err(RuntimeError::context(self.span.clone(), "expected bool")),
                };
            }
            _ => {
                return Err(RuntimeError::context(self.span.clone(), "not a debug function"));
            }
        };
        Ok(Val::None)
    }
}
