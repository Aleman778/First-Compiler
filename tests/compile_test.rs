
/***************************************************************************
 * Starts a compile test to test compiling many different files
 * in the `src/test` folder.
 ***************************************************************************/


use compile_test::generate_tests;
use std::process::{Command, Stdio};
use std::path::Path;
use std::ffi::OsStr;
use std::fs;


generate_tests!("src/test"; run_test);


/**
 * Runs the test code and evaluates the output.
 * Returns true if the test passed, false is failure.
 */
pub fn run_test(pass: &str, file: &str, output: &str) {
    let mut out_res = Command::new("sqrrlc")
        .args(&[OsStr::new(file)])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();

    assert!(out_res.is_ok());
    let compilation = out_res.unwrap();
    match pass {
        // Check that the test passed with correct stdout output.
        "run-pass" => {
            let actual = String::from_utf8_lossy(&compilation.stdout);
            if !output.is_empty() {
                let expected = fs::read_to_string(Path::new(output)).unwrap().replace('\r', "");
                assert_eq!(actual, expected);
            } else {
                assert_eq!(actual, "");
            }
        }
        // Check that the test program was built without errors.
        "build-pass" | "check-pass" => {
            let actual = String::from_utf8_lossy(&compilation.stderr);
            assert_eq!(actual, "");
        }
        // Check that the test program failed to build with specific errors.
        _ => {
            let actual = String::from_utf8_lossy(&compilation.stderr);
            if !output.is_empty() {
                let expected = fs::read_to_string(Path::new(output)).unwrap().replace('\r', "");
                assert_eq!(actual, expected);
            } else {
                assert_ne!(actual, "");
            }
        }
    }
}
