
/***************************************************************************
 * Starts a compile test to test compiling many different files
 * in the `src/test` folder.
 ***************************************************************************/


use compile_test::generate_tests;
use std::process::Command;
use std::path::Path;
use std::{fs, env};


generate_tests!("src/test"; run_test);


/**
 * Runs the test code and evaluates the output.
 * Returns true if the test passed, false is failure.
 */
pub fn run_test(pass: &str, file: &str, output: &str) {
    let command = env::var("SQRRLC_BIN").unwrap_or("sqrrlc".to_string());
    let compilation = Command::new(command)
        .arg(file)
        .output()
        .expect("failed to run the compiler");
    match pass {
        // Check that the test passed with correct stdout output.
        "run-pass" => {
            let actual = String::from_utf8_lossy(&compilation.stdout);
            let error = String::from_utf8_lossy(&compilation.stderr);
            if !output.is_empty() {
                let expected = fs::read_to_string(Path::new(output)).unwrap().replace('\r', "");
                assert_eq!(actual, expected);
            } else {
                assert_eq!(actual, "");
            }
            assert_eq!(error, "");
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
    assert!(compilation.status.success());
}
