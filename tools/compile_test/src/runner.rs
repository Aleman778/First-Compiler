
/***************************************************************************
 * The runner module executes the test code and compares output.
 ***************************************************************************/


use log::debug;
use std::rc::Rc;
use std::path::Path;
use crate::test::Test;
use sqrrlc::sqrrlc::session::Session;
use sqrrlc::sqrrlc_ast::base::File;
use sqrrlc::sqrrlc_parser::{
    Parser,
    ParseSpan
};
use sqrrlc::sqrrlc_interp::{
    debug::debug_functions,
    env::RuntimeEnv,
};
use sqrrlc::sqrrlc_typeck::{
    TypeChecker,
    TyCtxt,
};
use sqrrlc::sqrrlc::{
    symbol::generator::*,
    error::{Handler, emitter::Emitter},
    utils::Destination,
    source_map::SourceMap,
};


/**
 * Runs the test code and evaluates the output.
 */
pub fn run_test(test: &Test, dir: &Path) -> bool {
    // Setup the compiler session
    let mut sess = setup_session(dir);
    
    // Load the file
    let file = sess.source_map().load_file(&test.file).unwrap();
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
    let mut env = RuntimeEnv::new(&mut sess);
    ast.eval(&mut env);

    // match sess.dest_out {
        // Destination::Raw(data, _) => println!("{}", data),
        // _ => debug!("invalid destination"),
    // };
    

    return true;
}



/**
 * Setup a new session for this specific suite.
 */
fn setup_session(dir: &Path) -> Session {
    let emitter = Vec::new();
    let out = Vec::new();
    let err = Vec::new();
    let out2 = &out;
    let source_map = Rc::new(SourceMap::new(dir.to_path_buf()));
    println!("{:?}", out2);
    Session {
        handler: Handler::new(Emitter::new(
            Box::new(emitter),
            Rc::clone(&source_map),
            None,
            false)),
        working_dir: dir.to_path_buf(),
        source_map,
        dest_out: Destination::Raw(Box::new(out), false),
        dest_err: Destination::Raw(Box::new(err), false),
    }
}
