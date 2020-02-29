//! The main entry point of the sqrrl compiler.


#![allow(dead_code)]
#![allow(unused_imports)]

use sqrrlc::core::driver;
use sqrrlc::core::source_map::Filename;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate log;
extern crate simple_logger;


fn main() {
    let config = driver::Config {
        input: driver::Input::Code {
            name: Filename::Custom("test".to_string()),
            input: r###"    b'\u{534}'    "###.to_string(),
        },
        ..Default::default()
    };

    driver::run_compiler(config);
}
