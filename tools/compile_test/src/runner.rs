
/***************************************************************************
 * The runner module executes the test code and compares output.
 ***************************************************************************/


use crate::test::Test;
use sqrrl::sqrrlc::session::Session;
use sqrrl::sqrrlc_parser::{
    Parser,
    ParseSpan
};
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


/**
 * Runs the test code and evaluates the output.
 */
pub fn run_test(test: Test, sess: Session) {
    // Load the file
    let file = sess.source_map().load_file(test.file).unwrap();
    let span = ParseSpan::new_extra(&file.source, 0);

    // Parse the file
    let mut ast = File::parse(span).unwrap().1;

    // Append debug functions e.g. print_int()
    ast.extend(debug_functions());

    // Generate symbol table
    let mut sym = gen_sym_table(&ast);

    // Type checking
    let mut tcx = TyCtxt::new(&sess, &mut sym);
    ast.check_type(&mut tcx);

    // Interpreting
    let mut env = RuntimeEnv::new(&sess);
    ast.eval(&mut env);
    
}
