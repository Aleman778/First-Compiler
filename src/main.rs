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

use atty;
use log::{info, error};
use std::path::{Path, PathBuf};
use std::{env, fs};
use clap::{App, Arg, AppSettings};
use termcolor::ColorChoice;
use crate::ast::File;
use crate::parser::parse_file;
use crate::intrinsics::get_intrinsic_ast_items;
use crate::interp::{create_interp_context, interp_file, interp_entry_point};
use crate::typeck::{create_type_context, type_check_file};
use crate::borrowck::borrow_check_file;
use crate::ir::{create_ir_builder, build_ir_from_ast};
use crate::x86::{compile_ir_to_x86_machine_code};
use crate::jit::{allocate_jit_code, finalize_jit_code, execute_jit_code};
// use crate::llvm::codegen_test;

struct Config {
    input: Option<String>,
    run: Option<String>,
    backend: Backend,
    print: Print,
    color_choice: ColorChoice,
    type_checking: bool,
    borrow_checking: bool,
    compiletest: bool,
}

enum Backend {
    Interpreter,
    X86,
    LLVM,
}

enum Print {
    Ast,
    Ir,
    Assembly,
    MachineCode,
    None,
}

pub fn main() {
    // if cfg!(debug_assertions) {
    //     // NOTE(alexander): used for debugging without arguments
    //     let config = Config {
    //         input: Some(String::from("c:/dev/compiler/examples/sandbox.sq")),
    //         run: None,
    //         backend: Backend::X86,
    //         print: Print::Assembly,
    //         color_choice: ColorChoice::Auto,
    //         type_checking: true,
    //         borrow_checking: true,
    //         compiletest: false,
    //     };
    //     run_compiler(config);
    //     return;
    // }
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
        .arg(Arg::with_name("backend")
             .long("backend")
             .help(r#"Compiler backend "interp", "x86", "llvm" (default is "interpreter")"#)
             .value_name("BACKEND")
             .takes_value(true)
             .default_value("interp"))
        .arg(Arg::with_name("print")
             .long("print")
             .help(r#"Print info "ast", "ir", "asm", "machinecode", "none" (default is "none")"#)
             .value_name("BACKEND")
             .takes_value(true)
             .default_value("none"))
        .arg(Arg::with_name("version")
             .short("V")
             .long("version")
             .help("Print version output and exit"))
        .arg(Arg::with_name("color")
             .long("color")
             .help(r#"Color preference "always", "ansi", "auto", "off" (default is "auto")"#)
             .value_name("PREFERENCE")
             .takes_value(true)
             .default_value("auto"))
        .arg(Arg::with_name("Znotypecheck")
             .long("Znotypecheck")
             .help("Runs the compiler without type checking")
             .hidden(true))
        .arg(Arg::with_name("Znoborrowcheck")
             .long("Znoborrowcheck")
             .help("Runs the compiler without borrow checking")
             .hidden(true))
        .arg(Arg::with_name("Zcompiletest")
             .long("Zcompiletest")
             .help("Runs the compiler in testing mode")
             .hidden(true))
        .get_matches();

    let mut skip_compilation = false;
    if matches.is_present("version") {
        const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
        println!("sqrrl {}", VERSION.unwrap_or("unknown version"));
        skip_compilation = true;
    }

    let color_choice = match matches.value_of("color").unwrap().to_lowercase().as_str() {
        "always" => ColorChoice::Always,
        "ansi" => ColorChoice::AlwaysAnsi,
        "auto" => {
            if atty::is(atty::Stream::Stdout) {
                ColorChoice::Auto
            } else {
                ColorChoice::Never
            }
        }
        "off" => ColorChoice::Never,
        _ => {
            println!("\n--color expectes one of these values \"always\", \"ansi\", \"auto\", \"off\".\n");
            skip_compilation = true;
            ColorChoice::Never
        }
    };

    let backend = match matches.value_of("backend").unwrap().to_lowercase().as_str() {
        "interp" => Backend::Interpreter,
        "x86" => Backend::X86,
        "llvm" => Backend::LLVM,
        _ => {
            println!("\n--backend expectes one of these values \"interp\", \"x86\", \"llvm\"\n");
            skip_compilation = true;
            Backend::Interpreter
        }
    };

    let print = match matches.value_of("print").unwrap().to_lowercase().as_str() {
        "ast" => Print::Ast,
        "ir" => Print::Ir,
        "asm" => Print::Assembly,
        "machinecode" => Print::MachineCode,
        "none" => Print::None,
        _ => {
            println!("\n--print expectes one of these values \"ast\", \"ir\", \"asm\", \"machinecode\", \"none\"\n");
            skip_compilation = true;
            Print::None
        }
    };
    
    if !skip_compilation {
        let config = Config {
            input: matches.value_of("INPUT").map(|s| s.to_string()),
            run: matches.value_of("run").map(|s| s.to_string()),
            type_checking: !matches.is_present("Znotypecheck"),
            borrow_checking: !matches.is_present("Znoborrowcheck"),
            compiletest: matches.is_present("Zcompiletest"),
            backend,
            print,
            color_choice,
        };

    
        run_compiler(config);
    }
}

fn run_compiler(config: Config) {
    info!("setting up the compiler");

    error::COLOR_CHOICE.with(|color_choice| {
        *color_choice.borrow_mut() = config.color_choice;
    });
    
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
        eprintln!("\nerror: no input file or code");
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

    if let Print::Ast = config.print {
        print!("\n\n{:#?}", ast.items);
    }

    // Type check the current file
    if config.type_checking {
        let mut tc = create_type_context();
        type_check_file(&mut tc, &ast);
        if tc.error_count > 0 {
            error!("type checker reported {} errors, stopping compilation", tc.error_count);
            eprintln!("\nerror: aborting due to previous error");
            return;
        }
    }

    // Borrow check the current file
    if config.borrow_checking {
        let borrow_error_count = borrow_check_file(&ast);
        if borrow_error_count > 0 {
            error!("borrow checker reported {} errors, stopping compilation", borrow_error_count);
            eprintln!("\nerror: aborting due to previous error");
            return;
        }
    }

    match config.backend {
        Backend::Interpreter => {
            // Interpret the current file
            let mut ic = create_interp_context();
            interp_file(&mut ic, &ast);
            let code = interp_entry_point(&mut ic);
            println!("\nInterpreter exited with code {}", code);
            return;
        }

        Backend::X86 => {
            // Build low-level intermediate representation
            let mut ir_builder = create_ir_builder();

            // build lir
            build_ir_from_ast(&mut ir_builder, &ast);
            if let Print::Ir = config.print {
                print!("\n\n{}", ir_builder);
            }

            // The resulting intermediate representation
            let ir_instructions = ir_builder.instructions;
            let ir_functions = ir_builder.functions;

            // Generate code to jit
            let (machine_code, assembly) = compile_ir_to_x86_machine_code(ir_instructions, ir_functions);

            if let Print::Assembly = config.print {
                println!("\n\n{}", assembly);
            }

            if let Print::MachineCode = config.print {
                println!("\n");
                for (i, byte) in machine_code.iter().enumerate() {
                    print!("{:02x} ", byte);
                    if i % 16 == 15 {
                        println!("");
                    }
                }
                println!("\n\nSize of code is {} bytes", machine_code.len());
            }

            let jit_code = allocate_jit_code(machine_code.len());
            
            unsafe {
                let src_len = machine_code.len();
                let src_ptr = machine_code.as_ptr();
                std::ptr::copy_nonoverlapping(src_ptr, jit_code.addr, src_len);
            }
            
            finalize_jit_code(&jit_code);

            let ret = execute_jit_code(&jit_code);
            println!("\nProgram exited with code {}", ret);
        }

        Backend::LLVM => {
            unimplemented!()
        }
    }
}
