
/***************************************************************************
 * Helper functions for testing the interpreter.
 ***************************************************************************/


use compiler::{
    ast::expr::Expr,
    parser::ParseSpan,
    interp::{
        Eval,
        IResult,
        value::Val,
    },
};


/**
 * Interprets a mathematical expression.
 */
pub fn interp_math(input: &str) -> IResult<Val> {
    let expr = Expr::parse_math(ParseSpan::new_extra(input, "")).unwrap().1;
    expr.eval()
}
