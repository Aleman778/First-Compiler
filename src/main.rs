#![allow(dead_code)]

extern crate clap;

#[cfg(any(target_os="linux", target_os="macos"))]
extern crate libc;

mod error;
mod ast;
mod parser;
mod interp;
mod typeck;
mod borrowck;
mod ir;
mod jit;
mod x86;
mod intrinsics;
// mod llvm;

use log::{info, error};
use std::path::{Path, PathBuf};
use std::{env, fs};
use clap::{App, Arg, AppSettings};
use crate::ast::File;
use crate::parser::parse_file;
use crate::intrinsics::get_intrinsic_ast_items;
use crate::interp::{create_interp_context, interp_file, interp_entry_point};
use crate::typeck::{create_type_context, type_check_file};
use crate::borrowck::borrow_check_file;
use crate::ir::{create_ir_builder, build_ir_from_ast};
use crate::x86::{compile_ir_to_x86_machine_code};
use crate::jit::{allocate_jit_code, finalize_jit_code, free_jit_code, execute_jit_code};
// use crate::llvm::codegen_test;

struct Config {
    input: Option<String>,
    run: Option<String>,
    interpret: bool,
    nocolor: bool,
    compiletest: bool,
}

pub fn main() {
    if cfg!(debug_assertions) {
        // NOTE(alexander): used for debugging without arguments
        let config = Config {
            input: Some(String::from("c:/dev/compiler/examples/borrowing.sq")),
            run: None,
            interpret: false,
            nocolor: false,
            compiletest: false,
        };
        run_compiler(config);
        return;
    }

    let matches = App::new("sqrrl")
        .setting(AppSettings::ArgRequiredElseHelp)
        .arg(Arg::with_name("INPUT")
             .help("The input source file to compile")
             .value_name("FILE")
             .index(1))
        .arg(Arg::with_name("run")
             .short("r")
             .value_name("CODE")
             .help("Code that executes the code before running main"))
        .arg(Arg::with_name("interpret")
             .short("i")
             .help("Runs the interpreter on the given input"))
        // .arg(Arg::with_name("output")
             // .short("o")
             // .value_name("FILE")
             // .help("Write output to <filename>"))
        .arg(Arg::with_name("version")
             .short("V")
             .long("version")
             .help("Print version output and exit"))
        .arg(Arg::with_name("verbose")
             .short("v")
             .long("verbose")
             .help("Enable verbose output"))
        .arg(Arg::with_name("nocolor")
             .long("nocolor")
             .help("Disables colored text in errors"))
        .arg(Arg::with_name("Zcompiletest")
             .long("Zcompiletest")
             .help("Runs the compiler in testing mode")
             .hidden(true))
        .get_matches();

    if matches.is_present("version") {
        const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
        println!("sqrrl {}", VERSION.unwrap_or("unknown version"));
    }

    let config = Config {
        input:       matches.value_of("INPUT").map(|s| s.to_string()),
        run:         matches.value_of("run").map(|s| s.to_string()),
        interpret:   matches.is_present("interpret"),
        nocolor:     matches.is_present("nocolor"),
        compiletest: matches.is_present("Zcompiletest"),
    };

    run_compiler(config);
}

fn run_compiler(config: Config) {
    info!("setting up the compiler");
    let mut _working_dir = env::current_dir().unwrap_or(PathBuf::new());
    let mut ast_file: Option<File> = None;

    // Parse input file provided by config
    if let Some(input) = config.input {
        let path = Path::new(&input);
        let source;
        match fs::read_to_string(&input) {
            Ok(string) => source = string,
            Err(err) => {
                eprintln!("error: {}", err);
                return;
            }
        };
        
        let mut filename = String::clone(&input);
        if path.is_absolute() {
            _working_dir = path.parent().map(|p| p.to_path_buf()).unwrap_or(_working_dir);
            filename = String::from(path.file_name().map(|s| s.to_str().unwrap()).unwrap());
        }

        // Parse input file
        ast_file = Some(parse_file(source, filename));
    }

    // Parse optional code directly from the config
    if let Some(source) = config.run {
        let filename = "<run>";
        let ast_run = parse_file(source, String::from(filename));
        if let Some(file) = &mut ast_file {
            file.imported_files.insert(String::from(filename), Box::new(ast_run));
        } else {
            ast_file = Some(ast_run);
        }
    }

    if ast_file.is_none() {
        eprintln!("\nerror: failed to parse anything");
        return;
    }

    // Include compiler intrinsics in the parsed ast file
    let mut ast = ast_file.unwrap();
    let intrinsic_mod = get_intrinsic_ast_items();
    ast.items.push(intrinsic_mod);

    if ast.error_count > 0 {
        error!("parse errors reported {} errors, stopping compilation", ast.error_count);
        eprintln!("\nerror: aborting due to previous error");
        return;
    }
    print!("\n\nAbstract Syntax Tree:\n-----------------------------------------------\n{:#?}", ast.items);

    // Type check the current file
    let mut tc = create_type_context();
    type_check_file(&mut tc, &ast);

    if tc.error_count > 0 {
        error!("type checker reported {} errors, stopping compilation", tc.error_count);
        eprintln!("\nerror: aborting due to previous error");
        return;
    }

    // let mut bc = create_borrow_context();
    let borrow_error_count = borrow_check_file(&ast);

    if borrow_error_count > 0 {
        error!("borrow checker reported {} errors, stopping compilation", borrow_error_count);
        eprintln!("\nerror: aborting due to previous error");
        return;
    }

    // Interpret the current file
    if config.interpret {
        let mut ic = create_interp_context();
        interp_file(&mut ic, &ast);
        interp_entry_point(&mut ic);
        return;
    }

    // LLVM pass
    // codegen_test();

    // Build low-level intermediate representation
    let mut ir_builder = create_ir_builder();

    // build lir
    build_ir_from_ast(&mut ir_builder, &ast);
    print!("\n\nIntermediate Representation:\n-----------------------------------------------\n{}", ir_builder);

    // The resulting intermediate representation
    let ir_instructions = ir_builder.instructions;
    let ir_functions = ir_builder.functions;

    // Generate code to jit
    let (machine_code, assembly) = compile_ir_to_x86_machine_code(ir_instructions, ir_functions);

    println!("\n\nX86 Assembler:\n-----------------------------------------------");
    println!("{}", assembly);

    println!("\n\nX86 Machine Code:\n-----------------------------------------------");
    for (i, byte) in machine_code.iter().enumerate() {
        print!("{:02x} ", byte);
        if i % 16 == 15 {
            println!("");
        }
    }
    println!("\nSize of code is {} bytes", machine_code.len());

    let jit_code = allocate_jit_code(machine_code.len());
    
    unsafe {
        let src_len = machine_code.len();
        let src_ptr = machine_code.as_ptr();
        std::ptr::copy_nonoverlapping(src_ptr, jit_code.addr, src_len);
    }
    
    finalize_jit_code(&jit_code);

    println!("\nOutput from executing jitted code:\n-----------------------------------------------");
    let ret = execute_jit_code(&jit_code);
    println!("\nProgram exited with code {}", ret);
    free_jit_code(&jit_code);
}
