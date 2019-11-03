#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


use std::fs;
use compiler::error::convert_error;
use compiler::ast::{
    base::File,
    expr::Expr,
};
use compiler::parser::{Parser, ParseSpan};
use compiler::interp::{
    debug::debug_functions,
    env::RuntimeEnv,
    Eval,
};
use compiler::type_checker::{
    env::TypeEnv,
    TypeChecker,
};


fn main() {
    // let a: i32 = 5 + false;
    
    // Parse from file
    // let filename = "c:/dev/sqrrl-lang/compiler/examples/sandbox.sq";
    // let contents = fs::read_to_string(filename).expect("file was not found");
    // let span = ParseSpan::new_extra(contents.as_str(), filename);
    // let mut expr = File::parse(span).unwrap().1;
    // expr.extend(debug_functions());

    //Parse expression
    let source = "5 + false";
    let span = ParseSpan::new_extra(source, "src\\main.rs");
    let expr = Expr::parse_math(span).unwrap().1;
    let mut env = TypeEnv::new();
    // println!("AST: {:#?}", expr);
    expr.check_type(&mut env);
    env.done(source);
    
    // let mut env = RuntimeEnv::new(contents.clone());
    // let val = expr.eval(&mut env);
    // match val {
        // Ok(_) => println!("Ok"),
        // Err(e) => println!("{}", convert_error(e.kind.description().as_str(), &e.span, contents.as_str(), "")),
    // };
}
