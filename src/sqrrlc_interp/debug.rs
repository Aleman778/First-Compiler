
/***************************************************************************
 * Defines rust (foreign) functions for debugging interpreted programs.
 ***************************************************************************/

use crate::sqrrlc_interp::{
    env::RuntimeEnv,
};


/**
 * Prints the environment
 */
pub fn trace(env: &mut RuntimeEnv) {
    println!("{:#?}", env);
}


/**
 * Prints the given integer.
 */
pub fn print_int(val: i32) {
    println!("{}", val);
}


/**
 * Prints the given boolean.
 */
pub fn print_bool(val: bool) {
    println!("{}", val);
}


/**
 * Simple equals assertion.
 */
pub fn assert(val: bool) {
    assert!(val);
}

/**
 * Simple equals assertion.
 */
pub fn assert_eq_int(left: i32, right: i32) {
    assert_eq!(left, right);
}


/**
 * Simple equals assertion.
 */
pub fn assert_eq_bool(left: bool, right: bool) {
    assert_eq!(left, right);
}
