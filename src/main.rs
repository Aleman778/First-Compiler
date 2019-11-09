#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


use std::fs;
use sqrrl::sqrrlc::{
    error::diagnostic::*,
    error::emitter::Emitter,
};
use sqrrl::sqrrlc_ast::{
    base::*,
    expr::*,
    ty::*,
};
use sqrrl::sqrrlc_parser::{Parser, ParseSpan};
use sqrrl::sqrrlc_interp::{
    debug::debug_functions,
    env::RuntimeEnv,
    Eval,
};
use sqrrl::sqrrlc_typeck::{
    env::TypeEnv,
    TypeChecker,
};
use sqrrl::sqrrlc::symbol::{
    generator::*,
    table::SymbolTable,
};


fn main() {
    // let a: i32 = 5 + false;

    let emitter = Emitter::new();
    emitter.emit_diagnostic(&Diagnostic::new(Level::Note, "unused warnings are turned on by default"));
    emitter.emit_diagnostic(&Diagnostic::new(Level::Warning, "function `do_nothing` is unused"));
    emitter.emit_diagnostic(&Diagnostic::new(Level::Error, "cannot add `i32` to `bool`"));
    emitter.emit_diagnostic(&Diagnostic::new(Level::Fatal, "out of memory"));
    
    // Parse from file
    let filename = "c:/dev/sqrrl-lang/examples/sandbox.sq";
    let contents = fs::read_to_string(filename).expect("file was not found");
    let span = ParseSpan::new_extra(contents.as_str(), filename);
    let mut expr = File::parse(span).unwrap().1;
    expr.extend(debug_functions());

    let symbols = gen_sym_table(&expr);
    // println!("{:#?}", symbols);
    
    let mut env = TypeEnv::new(symbols);
    expr.check_type(&mut env);
    
    //Parse expression
    // let source = "5 + false";
    // let span = ParseSpan::new_extra(source, "src\\main.rs");
    // let expr = Expr::parse_math(span).unwrap().1;
    // let mut env = TypeEnv::new();
    // println!("SQRRLC-AST: {:#?}", expr);
    // expr.check_type(&mut env);
    // env.done(source);
    
    // let mut env = RuntimeEnv::new(contents.clone());
    // let val = expr.eval(&mut env);
    // match val {
        // Ok(_) => println!("Ok"),
        // Err(e) => println!("{}", convert_error(e.kind.description().as_str(), &e.span, contents.as_str(), "")),
    // };
}
