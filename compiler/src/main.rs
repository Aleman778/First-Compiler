
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


mod error;
mod ast;
mod parser;
mod interpreter;


use crate::ast::expr::Expr;
use crate::parser::{Parser, ParseSpan};
use crate::interpreter::Eval;


fn main() {
    let input = ParseSpan::new("(5+3**2-3)>=11");
    let expr = Expr::parse_math(input.clone());
    match expr {
        Ok(ast) => {
            println!("Parse Ok:{:#?}", ast);
            
            match ast.1.eval() {
                Ok(val) => println!("Eval Ok:{:#?}", val),
                Err(e) => println!("Eval Err:{:#?}", e),
            };
        }
        Err(e) => {
            match e {
                nom::Err::Error(err) => println!("{}", parser::error::convert_error(&input, err)),
                _ => println!("{:#?}", e),
            };
        }
    }
}
