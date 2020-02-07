#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/

// use sqrrlc::sqrrlc::driver;

use sqrrlc::sqrrlc_ast;
use sqrrlc::sqrrlc_lexer;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate nom;


fn main() {
    // driver::main();
    let input = "1 + 2";
    let tokenizer = sqrrlc_lexer::tokenize(input);
    while if let Some(token) = tokenizer.next() {
        println!("{:?}", token);
    }
}
