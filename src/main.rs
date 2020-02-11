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
    let input = "fn main() {return 1   \n     \t \t +    a;}";
    
    let mut ptr: usize = 0;
    let mut stream = sqrrlc_lexer::tokenize(input);
    while let Some(token) = stream.next() {
        println!("token_kind: {:?},\nlenth: {}", token.kind, token.len);
        println!("str: {}\n", &input[ptr..(ptr + token.len)]);
        ptr += token.len;
    }
}
