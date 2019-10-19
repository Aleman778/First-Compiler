
/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


mod error;
mod ast;
mod parser;
mod interp;


use std::fs;
use std::path::Path;
use crate::ast::base::File;
use crate::parser::{Parser, ParseSpan};
// use crate::interpreter::Eval;


fn main() {
    let filename = "c:/dev/sqrrl-lang/compiler/examples/primes.sq";
    let contents = fs::read_to_string(filename).expect("file was not found");
    let span = ParseSpan::new_extra(contents.as_str(), Path::new(filename));
    let file = File::parse(span).unwrap();
    println!("File AST:{:#?}", file);
}
