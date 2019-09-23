
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


mod ast;
mod parser;


use ast::{Span, env::Env};
use parser::Parser;


fn main() {
    println!("{:#?}", ast::atom::Val::parse(Span::new_extra("42", &Env::new())));
}
