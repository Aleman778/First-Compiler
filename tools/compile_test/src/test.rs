
/***************************************************************************
 * The tests module defines the properties of a given test
 ***************************************************************************/


use std::path::PathBuf;



/**
 * Test case struct defines the source file
 * and other options to consider when running.
 */
pub struct Test {
    pass: TestPass,
    file: PathBuf,
    output: PathBuf,
}


/**
 * Test pass defines how a given file should be tested.
 * Some test relies on stderr output while others only
 * need to check if it compiles/ builds without errors
 * or runs with a specfic stdout output.
 */
pub enum TestPass {
    /// Builds and runs the given file and compare output.
    Run,
    /// Only builds the given file.
    Build,
    /// Check only compiles the given file without codegen (faster than build)
    Check,
}
