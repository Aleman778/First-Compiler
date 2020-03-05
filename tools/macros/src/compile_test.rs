//! Compile tests procedural macro code.


use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Lit, Ident, Token};
use std::path::Path;
use std::{io, fs};
use std::io::{prelude::*, BufReader};
use std::fs::File;
use std::ffi::OsStr;


/**
 * Test case struct defines the source file
 * and other options to consider when running.
 * If no output provided file then assume it fails,
 * if no other test pass is provided.
 */
#[derive(Debug)]
pub struct Test {
    pub pass: String,
    pub name: String,
    pub file: String,
    pub output: String,
}


impl Test {
    /**
     * Create a new test from a given source file path.
     */
    pub fn from(path: &Path) -> Self {
        let pass = parse_pass(path);
        let output = find_output(pass, path);
        Test {
            file: path.to_str().unwrap().to_string(),
            name: path.file_stem().map(|s| s.to_owned().into_string().unwrap()).unwrap(),
            pass: pass.to_string(),
            output: output.to_string(),
        }
    }
}


/**
 * Tries to find output file either file with the name
 * with extension .stdout or .stderr with same filename.
 */
fn find_output(pass: &str, path: &Path) -> String {
    match pass {
        "run-pass" => {
            let stdout = path.with_extension("stdout");
            if stdout.exists() {
                stdout.to_str().unwrap().to_string()
            } else {
                String::new()
            }
        }
        "fail" => {
            let stderr = path.with_extension("stderr");
            if stderr.exists() {
                stderr.to_str().unwrap().to_string()
            } else {
                String::new()
            }
        }
        _ => String::new()
    }
}


/**
 * Parse the first line of the source code to 
 * to figure out the test pass enum.
 */
fn parse_pass(path: &Path) -> &str {
    match File::open(path) {
        Ok(file) => {
            let mut reader = BufReader::new(file);
            let mut line = String::new();
            reader.read_line(&mut line).unwrap();
            if line.contains("// run-pass") { "run-pass" }
            else if line.contains("// build-pass") { "build-pass" }
            else if line.contains("// check-pass") { "check-pass" }
            else if line.contains("// ignore") { "ignore" }
            else { "fail" }
        }
        Err(_) => "fail"
    }
}


/**
 * Looks through the directory of files finding and creating tests.
 * The tests are appended to the given tests vector.
 */
pub fn find_source_files(dir: &Path, tests: &mut Vec<Test>) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                find_source_files(&path, tests)?;
            } else {
                if path.extension() == Some(OsStr::new("sq")) {
                    tests.push(Test::from(&path));
                }
            }
        }
    }
    Ok(())
}


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
 * Generate tests based on given input.
 */
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
        });
        if test.pass == "ignore" {
            expanded = concat_ts(expanded, quote! {
                #[ignore]
            });
        }
        expanded = concat_ts(expanded, quote! {
            fn #func_ident() {
                #lambda(#test_pass, #test_file, #test_output);
            }
        });
    }
    expanded.into()
}
