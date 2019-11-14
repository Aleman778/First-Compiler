#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


use std::fs;
use std::env;
use std::path::{Path, PathBuf};
use sqrrl::sqrrlc::{
    session::Session,
    error::diagnostic::*,
    error::emitter::Emitter,
};
use sqrrl::sqrrlc_ast::{
    base::*,
    expr::*,
    ty::*,
    span::*,
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
    let sess = Session::new(PathBuf::from(r"C:\dev\sqrrl-lang\examples\"));
    let file = sess.source_map().load_file(Path::new("sandbox.sq")).unwrap();
    // println!("{:?}", file);
    
    // Test diagnostics
    
    let span = Span::from_bounds(LineColumn::new(7, 1), LineColumn::new(13, 2));
    let span_sub = Span::from_bounds(LineColumn::new(10, 24), LineColumn::new(10, 25));

    let err = &mut sess.struct_span_warn(span, "This function is not used!");
    err.span_label(span_sub, "no implementation of sub");
    sess.emit(err);
    let err = &mut sess.struct_span_err(span, "This function is not used!");
    err.span_label(span_sub, "no implementation of sub");
    sess.emit(err);
    let err = &mut sess.struct_span_fatal(span, "This function is not used!");
    err.span_label(span_sub, "no implementation of sub");
    sess.emit(err);
    
    // Parse the loaded file
    // let span = ParseSpan::new(&file.source);
    // let mut expr = File::parse(span).unwrap().1;
    // expr.extend(debug_functions());

    // let symbols = gen_sym_table(&expr);
    // println!("{:#?}", symbols);
    
    // let mut env = TypeEnv::new(symbols);
    // expr.check_type(&mut env);
    
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
