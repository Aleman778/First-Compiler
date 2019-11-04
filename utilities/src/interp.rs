
/***************************************************************************
 * Helper functions for testing the interpreter.
 ***************************************************************************/


use sqrrl::sqrrlc_ast::{
    span::Span,
    base::{File, Item, FnItem},
    expr::Expr,
};
use sqrrl::sqrrlc_parser::{
    Parser,
    ParseSpan,
};
use sqrrl::sqrrlc_interp::{
    Eval,
    IResult,
    env::RuntimeEnv,
    value::Val,
};


/**
 * Interprets a mathematical expression.
 */
pub fn interp_math(input: &str) -> IResult<Val> {
    let expr = Expr::parse_math(ParseSpan::new_extra(input, "")).unwrap().1;
    let mut env = RuntimeEnv::new(input.to_string());
    expr.eval(&mut env)
}


/**
 * Interprets an expression.
 */
pub fn interp_expr(input: &str) -> IResult<Val> {
    let mut env = RuntimeEnv::new("".to_string());
    setup_env(&mut env);
    let expr = Expr::parse(ParseSpan::new_extra(input, "")).unwrap().1;
    expr.eval(&mut env)
}


/**
 * Interprets a file source and after that interprets teh input string.
 */
pub fn interp_file(file: &str, input: &str) -> IResult<Val> {
    let span = ParseSpan::new_extra(file, "");
    let file = File::parse(span).unwrap().1;
    let expr = Expr::parse(ParseSpan::new_extra(input, "")).unwrap().1;
    let mut env = RuntimeEnv::new("".to_string());
    setup_env(&mut env);
    file.eval(&mut env)?;
    expr.eval(&mut env)
}



/**
 * Setup the environment.
 */
pub fn setup_env(env: &mut RuntimeEnv) {
    let main = FnItem::parse(ParseSpan::new_extra("fn main() {}", "")).unwrap().1;
    env.store_item(Item::Fn(main));
    env.push_main().unwrap();
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
    Val::Break(Span::new_empty())
}


/**
 * Returns a break value
 */
pub fn val_continue() -> Val {
    Val::Continue(Span::new_empty())
}

    
