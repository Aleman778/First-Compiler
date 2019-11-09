
/***************************************************************************
 * Sqrrl compiler main module contains common data structures and
 * is the where the driver that runs the compiler is located.
 ***************************************************************************/


use crate::sqrrlc::error::Error;


pub type Result<T> = std::result::Result<T, Error>;


pub mod error;
pub mod symbol;
