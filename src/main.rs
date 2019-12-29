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
    TypeChecker,
    TyCtxt,
};
use sqrrl::sqrrlc::symbol::{
    generator::*,
    table::SymbolTable,
};


fn main() {
    let sess = Session::with_dir(PathBuf::from(r"C:\dev\sqrrl-lang\"));
    let file = sess.source_map().load_file(Path::new("examples/sandbox.sq")).unwrap();
    // Parse the loaded file
    let span = ParseSpan::new_extra(&file.source, 0);
    let mut expr = File::parse(span).unwrap().1;
    expr.extend(debug_functions());
    // println!("AST Expr: {:#?}", expr);

    // Type checking routine
    let mut sym = gen_sym_table(&expr);
    // println!("{:#?}", sym);
    let mut tcx = TyCtxt::new(&sess, &mut sym);
    // expr.check_type(&mut tcx);

    // Start the interpreter routine
    let mut env = RuntimeEnv::new(&sess);
    expr.eval(&mut env);
}
