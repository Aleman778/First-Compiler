

/***************************************************************************
 * Test suite module defines a list of test cases.
 ***************************************************************************/


use crate::test::Test;


/**
 * Test suite is a list of test cases that are
 * executed in order. This class also keeps track
 * of progress during execution.
 */
pub struct TestSuite {
    tests: Vec<Test>,
    current: u32,
    passed: u32,
    failed: u32,
}


impl TestSuite {
    
}
