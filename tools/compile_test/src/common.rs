
/***************************************************************************
 * The common module defines the properties of a given test
 ***************************************************************************/


use std::{io, fs};
use std::io::{prelude::*, BufReader};
use std::path::Path;
use std::fs::File;
use std::ffi::OsStr;


/**
 * Test case struct defines the source file
 * and other options to consider when running.
 * If no output provided file then assume it fails,
 * if no other test pass is provided.
 */
#[derive(Debug)]
pub struct Test {
    pub pass: String,
    pub name: String,
    pub file: String,
    pub output: String,
}


impl Test {
    /**
     * Create a new test from a given source file path.
     */
    pub fn from(path: &Path) -> Self {
        let pass = parse_pass(path);
        let output = find_output(pass, path);
        Test {
            file: path.to_str().unwrap().to_string(),
            name: path.file_stem().map(|s| s.to_owned().into_string().unwrap()).unwrap(),
            pass: pass.to_string(),
            output: output.to_string(),
        }
    }
}


/**
 * Tries to find output file either file with the name
 * with extension .stdout or .stderr with same filename.
 */
fn find_output(pass: &str, path: &Path) -> String {
    match pass {
        "run-pass" => {
            let stdout = path.with_extension("stdout");
            if stdout.exists() {
                stdout.to_str().unwrap().to_string()
            } else {
                String::new()
            }
        }
        "fail" => {
            let stderr = path.with_extension("stderr");
            if stderr.exists() {
                stderr.to_str().unwrap().to_string()
            } else {
                String::new()
            }
        }
        _ => String::new()
    }
}


/**
 * Parse the first line of the source code to 
 * to figure out the test pass enum.
 */
fn parse_pass(path: &Path) -> &str {
    match File::open(path) {
        Ok(file) => {
            let mut reader = BufReader::new(file);
            let mut line = String::new();
            reader.read_line(&mut line).unwrap();
            if line.contains("// run-pass") { "run-pass" }
            else if line.contains("// build-pass") { "build-pass" }
            else if line.contains("// check-pass") { "check-pass" }
            else if line.contains("// ignore") { "ignore" }
            else { "fail" }
        }
        Err(_) => "fail"
    }
}


/**
 * Looks through the directory of files finding and creating tests.
 * The tests are appended to the given tests vector.
 */
pub fn find_source_files(dir: &Path, tests: &mut Vec<Test>) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                find_source_files(&path, tests)?;
            } else {
                if path.extension() == Some(OsStr::new("sq")) {
                    tests.push(Test::from(&path));
                }
            }
        }
    }
    Ok(())
}
