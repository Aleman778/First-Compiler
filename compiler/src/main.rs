
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/

mod error;
mod ast;
mod parser;


use crate::ast::expr::Expr;
use crate::parser::ParseSpan;

fn main() {
    let input = ParseSpan::new("2+3**(2*3)+-4");
    match Expr::parse_math(input.clone()) {
        Ok(ast) => println!("Ok:{:#?}", ast),
        Err(e) => {
            match e {
                nom::Err::Error(err) => println!("{}", parser::error::convert_error(&input, err)),
                _ => println!("{:#?}", e),
            }
        }
    }
    // println!("{:#?}", ast::atom::Atom::parse(ParseSpan::new("aaa")));
}
