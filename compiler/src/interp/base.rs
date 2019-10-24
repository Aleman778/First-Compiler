
/***************************************************************************
 * Base evaluations is used to evaluate the main parts of the AST
 * e.g. the a file, function declarations etc.
 ***************************************************************************/


use crate::ast::{
    base::*,
};
use crate::interp::{
    value::Val,
    env::Env,
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
        func.block.eval(env)
    }
}
