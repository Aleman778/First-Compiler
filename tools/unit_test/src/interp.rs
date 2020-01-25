
/***************************************************************************
 * Helper functions for testing the interpreter.
 ***************************************************************************/


use std::path::Path;
use sqrrlc::sqrrlc::session::Session;
use sqrrlc::sqrrlc_ast::{
    span::Span,
    base::{File, Item, FnItem},
    expr::Expr,
};
use sqrrlc::sqrrlc_parser::{
    Parser,
    ParseSpan,
};
use sqrrlc::sqrrlc_interp::{
    Eval,
    IResult,
    env::RuntimeEnv,
    value::*,
};


/**
 * Interprets a mathematical expression.
 */
pub fn interp_math(input: &str) -> IResult<Val> {
    let expr = Expr::parse_math(ParseSpan::new_extra(input, 0)).unwrap().1;
    let mut sess = Session::new();
    let mut env = RuntimeEnv::new(&mut sess);
    expr.eval(&mut env)
}


/**
 * Interprets an expression.
 */
pub fn interp_expr(input: &str) -> IResult<Val> {
    let mut sess = Session::new();
    let mut env = RuntimeEnv::new(&mut sess);
    setup_env(&mut env);
    let expr = Expr::parse(ParseSpan::new_extra(input, 0)).unwrap().1;
    expr.eval(&mut env)
}


/**
 * Interprets a file source and after that interprets teh input string.
 */
pub fn interp_file(file: &str, input: &str) -> IResult<Val> {
    let mut sess = Session::new();
    let file = sess.source_map().load_file(&Path::new(&format!("tests/{}", file))).unwrap();
    let span = ParseSpan::new_extra(&file.source, 0);
    let file_ast = File::parse(span).unwrap().1;
    let expr_ast = Expr::parse(ParseSpan::new_extra(input, 0)).unwrap().1;
    let mut env = RuntimeEnv::new(&mut sess);
    setup_env(&mut env);
    file_ast.eval(&mut env);
    expr_ast.eval(&mut env)
}



/**
 * Setup the environment.
 */
pub fn setup_env(env: &mut RuntimeEnv) {
    let main = FnItem::parse(ParseSpan::new_extra("fn main() {}", 0)).unwrap().1;
    env.store_item(Item::Fn(main));
    match env.load_main() {
        Ok(func) => { env.push_func(&func, Vec::new()).unwrap(); },
        Err(_) => panic!("Failed to load main function"),
    }
}

    
/**
 * Returns an i32 value
 */
pub fn val_i32(val: i32) -> Val {
    Val::from_i32(val, Span::new_empty())
}

    
/**
 * Returns a boolean value.
 */
pub fn val_bool(val: bool) -> Val {
    Val::from_bool(val, Span::new_empty())
}


/**
 * Returns a break value
 */
pub fn val_break() -> Val {
    Val::from_data(ValData::Break, None, Span::new_empty())
}


/**
 * Returns a break value
 */
pub fn val_continue() -> Val {
    Val::from_data(ValData::Continue, None, Span::new_empty())
}


/**
 * Returns a break value
 */
pub fn val_none() -> Val {
    Val::from_data(ValData::None, None, Span::new_empty())
}
