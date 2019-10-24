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
use crate::ast::{
    base::File,
    expr::Expr,
};
use crate::parser::{Parser, ParseSpan};
use crate::interp::{
    env::Env,
    Eval,
};


fn main() {
    // Parse from file
    let filename = "c:/dev/sqrrl-lang/compiler/examples/primes.sq";
    let contents = fs::read_to_string(filename).expect("file was not found");
    let span = ParseSpan::new_extra(contents.as_str(), filename);
    let expr = File::parse(span).unwrap().1;
    // println!("File AST:{:#?}", file);

    // Parse from mathematical expressions from string
    // let input = "true == 3";//"2+3**2*3+4";
    // let span = ParseSpan::new_extra(input, "");
    // let expr = Expr::parse_math(span).unwrap().1;
    let mut env = Env::new();
    let val = expr.eval(&mut env);
    println!("Expr AST:\n{:#?}\n\n", expr);
    match val {
        Ok(v) => println!("Evaluates to:\n{:#?}", v),
        Err(e) => println!("error: {}", e.kind.description()),
    };
    println!("Env: {:#?}", env);
}
