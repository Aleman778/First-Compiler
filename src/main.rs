//! The main entry point of the sqrrl compiler.


#![allow(dead_code)]
#![allow(unused_imports)]

use sqrrlc::sqrrlc::driver;
use sqrrlc::sqrrlc::source_map::Filename;

#[macro_use]
extern crate clap;

fn main() {
    let config = driver::Config {
        input: driver::Input::Code {
            name: Filename::Custom("test".to_string()),
            input: "1".to_string(),
        },
        ..Default::default()
    }; 
}
