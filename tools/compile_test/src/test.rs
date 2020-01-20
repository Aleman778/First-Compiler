
/***************************************************************************
 * The tests module defines the properties of a given test
 ***************************************************************************/


use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::{prelude::*, BufReader};


/**
 * Test case struct defines the source file
 * and other options to consider when running.
 * If no output provided file then assume it fails,
 * if no other test pass is provided.
 */
#[derive(Debug)]
pub struct Test {
    pub pass: TestPass,
    pub file: PathBuf,
    pub output: Option<PathBuf>,
}


impl Test {

    /**
     * Create a new test from a given source file path.
     */
    pub fn from(path: &Path) -> Self {
        Test {
            pass: TestPass::parse(path),
            file: path.to_path_buf(),
            output: find_output(path),
        }
    }
}


/**
 * Tries to find output file either file with the name
 * with extension .stdout or .stderr with same filename.
 */
fn find_output(path: &Path) -> Option<PathBuf> {
    let stderr = path.with_extension("stderr");
    if stderr.exists() {
        return Some(stderr);
    }
    let stdout = path.with_extension("stdout");
    println!("{:#?}", stdout.exists());
    if stdout.exists() {
        return Some(stdout);
    }
    None
}


/**
 * Test pass defines how a given file should be tested.
 * Some test relies on stderr output while others only
 * need to check if it compiles/ builds without errors
 * or runs with a specfic stdout output.
 */
#[derive(Debug)]
pub enum TestPass {
    /// Builds and runs the given file and compare output.
    Run,
    /// Only builds the given file.
    Build,
    /// Check only compiles the given file without codegen (faster than build)
    Check,
    /// The code should fail, optionally match the stderr output.
    Fail,
}



impl TestPass {
    /**
     * Parse the first line of the source code to 
     * to figure out the test pass enum.
     */
    fn parse(path: &Path) -> TestPass {
        match File::open(path) {
            Ok(file) => {
                let mut reader = BufReader::new(file);
                let mut line = String::new();
                reader.read_line(&mut line);
                if line.contains("// run-pass") { TestPass::Run }
                else if line.contains("// build-pass") { TestPass::Build }
                else if line.contains("// check-pass") { TestPass::Check }
                else { TestPass::Fail }
            }
            Err(_) => TestPass::Fail
        }
    }
}
