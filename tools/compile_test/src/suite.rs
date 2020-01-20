

/***************************************************************************
 * Test suite module defines a list of test cases.
 ***************************************************************************/


use std::io;
use std::fs;
use std::ffi::OsStr;
use std::path::{Path};
use sqrrl::sqrrlc::session::Session;
use crate::test::Test;


/**
 * Test suite is a list of test cases that are
 * executed in order. This class also keeps track
 * of progress during execution.
 */
pub struct TestSuite {
    /// The compiler session used throughout the tests.
    session: Session,
    /// The list of tests in this suite.
    tests: Vec<Test>,
    /// The current test index.
    current: u32,
    /// Number of tests passed.
    passed: u32,
    /// Number of tests failed.
    failed: u32,
}


impl TestSuite {
    /**
     * Creaes a new test suite from the provided path to
     * directory where the test files are located.
     */
    pub fn from(dir: &Path) -> Self {
        let mut tests = Vec::new();
        find_source_files(dir, &mut tests);
        println!("{:#?}", tests);
        TestSuite {
            session: Session::with_dir(dir.to_path_buf()),
            tests: tests,//lookup_files(dir),
            current: 0,
            passed: 0,
            failed: 0,
        }
    }


    pub fn run() {
        
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



// fn visit_dirs(dir: &Path, cv) {
//         if dir.is_dir() {
//             for entry in fs::read_dir(dir)? {
//                 let entry = entry?;
//                 let path = entry.path();
//                 if path.is_dir() {
                    
//                 } else {
                    
//                 }
//             }
//         }
// }
