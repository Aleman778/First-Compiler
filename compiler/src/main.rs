
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/

mod error;
mod ast;
mod parser;


use parser::ParseSpan;
use parser::Parser;


fn main() {
    let input = ParseSpan::new("2+3**2*3+4");
    match ast::expr::Expr::parse(input.clone()) {
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
