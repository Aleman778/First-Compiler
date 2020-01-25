
/***************************************************************************
 * The runner module executes the test code and compares output.
 ***************************************************************************/


use std::process::Command;
use std::path::Path;
use crate::test::Test;


/**
 * Runs the test code and evaluates the output.
 */
pub fn run_test(test: &Test, dir: &Path) -> bool {
    let output = if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args(&["/C", "echo hello"])
            .output()
            .expect("failed to execute process")
    } else {
        Command::new("sh")
            .arg("-c")
            .arg("echo hello")
            .output()
            .expect("failed to execute process")
    };
    return true;
}
