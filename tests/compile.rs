
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
    
}
