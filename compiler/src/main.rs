
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


mod ast;
mod parser;


use parser::ParseSpan;
use parser::Parser;


fn main() {
    println!("{:#?}", ast::atom::Atom::parse(ParseSpan::new("42")));
}
