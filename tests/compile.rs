
/***************************************************************************
 * Starts a compile test to test compiling many different files
 * in the `src/test` folder.
 ***************************************************************************/


use std::path::Path;
use compile_test::{
    suite::TestSuite,
};



/**
 * Runs the compile test suite.
 */
#[test]
fn compile_test() {
    let suite = TestSuite::from(&Path::new("src/test"));
    println!("running {} tests"), suite.tests.len();
    suite.run();
    println!("test result: ok. {} passed; {} failed; 0 ignored; 0 messured; 0 filtered out",
             suite.passed, suite.failed);
    
}
