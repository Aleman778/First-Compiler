//! The main entry point of the compiler.


extern crate log;
extern crate clap;
extern crate simple_logger;


#[macro_use]
pub mod error;
pub mod span;
pub mod core;
pub mod ast;
pub mod lexer;
pub mod parser;


use crate::core::driver;
use crate::core::source_map::Filename;


/**
 * Compiler entry point.
 */
fn main() {
    simple_logger::init_with_level(log::Level::Debug).unwrap();
    
    let config = driver::Config {
        input: driver::Input::Code {
            name: Filename::Custom("test".to_string()),
            input: r###"     for x in y x * 2    "###.to_string(),
        },
        ..Default::default()
    };

    driver::run_compiler(config);
    // driver::main();
}
