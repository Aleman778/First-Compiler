//! Macro helper library for sqrrl compiler defines useful
//! macros and procedural macros for parts of the compiler.


mod symbols;
mod compile_test;


extern crate proc_macro;
use proc_macro::TokenStream;


/**
 * Defines predefined symbols for both keywords
 * and general symbols used by the compiler.
 * 
 * Usage:
 * ```Rust
 * use macros::define_symbols;
 * 
 * define_symbols! {
 *     Keywords {
 *         // ... put keywords here ... e.g: `key: "value",`
 *     }
 *     Symbols {
 *         // ,,, put symbols here ... e.g: `key,`
 *     }
 * }
 * ```
 */
#[proc_macro]
pub fn define_symbols(input: TokenStream) -> TokenStream {
    symbols::define_symbols(input)
}


/**
 * Generates tests based on files in given directory.
 * Only accepts .sq files at the moment. Used for generating
 * test functions for compiling tests code and checking output.
 * 
 * Usage:
 * ```Rust
 * use macros::generate_tests;
 *
 * generate_tests!("src/tests"; run_test);
 *
 * pub fn run_test(pass: &str, file: &str, output: &str)  {
 *    //... put test code here ...
 * }
 * ```
 */
#[proc_macro]
pub fn generate_tests(input: TokenStream) -> TokenStream {
    crate::compile_test::generate_tests(input)
}
