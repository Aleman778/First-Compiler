
/***************************************************************************
 * The unit test library
 ***************************************************************************/


extern crate proc_macro;
use crate::common::find_source_files;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Lit, Ident, Token};
use std::path::Path;


/**
 * Concatinates two token streams.
 */
fn concat_ts(
    accu: proc_macro2::TokenStream,
    other: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    quote! { #accu #other }
}


/**
 * Parameter items from generate_tests macro.
 */
struct Params {
    dir: Lit,
    lambda: Ident,
}



/**
 * Parses the items from input token stream.
 */
impl Parse for Params {
    fn parse(input: ParseStream) -> Result<Self> {
        let dir: Lit = input.parse()?;
        input.parse::<Token![;]>()?;
        let lambda: Ident = input.parse()?;

        Ok(Params {
            dir,
            lambda,
        })
    }
}


/**
 * Generates test functions for each test file in
 * the given input directory, usage:
 * ```
 * use compile_test::generate_tests;
 *
 * generate_tests!("src/tests");
 * ```
 */
#[proc_macro]
pub fn generate_tests(item: TokenStream) -> TokenStream {
    let Params{dir, lambda} = parse_macro_input!(item as Params);
    let path = if let Lit::Str(s) = dir {
        s.value()
    } else {
        panic!("the directory argument has to be a string literal");
    };
    let mut tests = Vec::new();
    find_source_files(&Path::new(&path), &mut tests).unwrap();
    let mut expanded: proc_macro2::TokenStream = "".parse().unwrap();
    for test in tests {
        let test_pass = &test.pass;
        let test_name = &test.name;
        let test_file = &test.file;
        let test_output = &test.output;
        let func_ident = proc_macro2::Ident::new(&test_name, proc_macro2::Span::call_site());
        expanded = concat_ts(expanded, quote! {
            #[test]
            fn #func_ident() {
                #lambda(#test_pass, #test_file, #test_output);
            }
        });
    }
    expanded.into()
}


mod common;
