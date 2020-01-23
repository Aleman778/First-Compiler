

/***************************************************************************
 * Test suite module defines a list of test cases.
 ***************************************************************************/


use std::io;
use std::fs;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use crate::test::Test;
use crate::runner::run_test;


/**
 * Test suite is a list of test cases that are
 * executed in order. This class also keeps track
 * of progress during execution.
 */
pub struct TestSuite {
    /// The test suite source directory.
    pub dir: PathBuf,
    /// The list of tests in this suite.
    pub tests: Vec<Test>,
    /// The current test index.
    pub current: u32,
    /// Number of tests passed.
    pub passed: u32,
    /// Number of tests failed.
    pub failed: u32,
}


impl TestSuite {
    /**
     * Creaes a new test suite from the provided path to
     * directory where the test files are located.
     */
    pub fn from(dir: &Path) -> Self {
        let mut tests = Vec::new();
        find_source_files(dir, &mut tests);
        TestSuite {
            dir: dir.to_path_buf(),
            tests: tests,
            current: 0,
            passed: 0,
            failed: 0,
        }
    }


    /**
     * Runs the entire test suite.
     */
    pub fn run(&mut self) {
        self.current = 0;
        self.passed = 0;
        self.failed = 0;
        for test in &self.tests {
            if run_test(test, &self.dir) {
                self.passed += 1;
            } else {
                self.failed += 1;
            }
            self.current += 1;
        }
    }
}


fn find_source_files(dir: &Path, tests: &mut Vec<Test>) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                find_source_files(&path, tests)?;
            } else {
                if path.extension() == Some(OsStr::new("rs")) {
                    tests.push(Test::from(&path));
                }
            }
        }
    }
    Ok(())
}
