
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


mod ast;
mod parser;


use ast::Span;
use parser::Parser;


fn main() {
    println!("{:#?}", ast::atom::Atom::parse(Span::new("42")));
}
