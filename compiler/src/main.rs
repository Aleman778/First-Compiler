
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/

mod error;
mod ast;
mod parser;


use crate::ast::expr::Expr;
use crate::parser::ParseSpan;
use crate::interpreter::Eval;

fn main() {
    let input = ParseSpan::new("2+3");
    let expr = Expr::parse_math(input.clone());
    match expr {
        Ok(ast) => println!("Parse Ok:{:#?}", ast),
        Err(e) => {
            match e {
                nom::Err::Error(err) => println!("{}", parser::error::convert_error(&input, err)),
                _ => println!("{:#?}", e),
            }
        }
    }
    match expr.eval() {
        Ok(val) => println!("Eval Ok:{:#?}", val),
        Err(e) => println!("Eval Err:{:#?}", e),
    }
}
