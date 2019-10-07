
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/

mod error;
mod ast;
mod parser;


use parser::ParseSpan;
use parser::Parser;


fn main() {
    println!("{:#?}", ast::atom::Atom::parse(ParseSpan::new("1111111111111111")));
    println!("{:#?}", ast::atom::Atom::parse(ParseSpan::new("aaa")));
}
