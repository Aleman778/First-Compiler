#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


mod error;
mod ast;
mod parser;
mod interp;


use std::fs;
use crate::error::convert_error;
use crate::ast::{
    base::File,
    expr::Expr,
};
use crate::parser::{Parser, ParseSpan};
use crate::interp::{
    debug::debug_functions,
    env::RuntimeEnv,
    Eval,
};


fn main() {
    // Parse from file
    let filename = "c:/dev/sqrrl-lang/compiler/examples/sandbox.sq";
    let contents = fs::read_to_string(filename).expect("file was not found");
    let span = ParseSpan::new_extra(contents.as_str(), filename);
    let mut expr = File::parse(span).unwrap().1;
    expr.extend(debug_functions());
    // println!("File AST:{:#?}", file);

    // Parse from mathematical expressions from string
    // let input = "true == 3";//"2+3**2*3+4";
    // let span = ParseSpan::new_extra(input, "");
    // let expr = Expr::parse_math(span).unwrap().1;
    let mut env = RuntimeEnv::new(contents.clone());
    let val = expr.eval(&mut env);
    // println!("Expr AST:\n{:#?}\n\n", expr);
    match val {
        Ok(_) => println!("Ok"),
        Err(e) => println!("{}", convert_error(e.kind.description().as_str(), &e.span, contents.as_str(), "")),
    };
    // println!("Env: {:#?}", env);
}
