#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/

use sqrrlc::sqrrlc::driver;


#[macro_use]
extern crate clap;


fn main() {
    driver::main();
}
