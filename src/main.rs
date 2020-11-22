extern crate clap;

mod error;
mod ast;
mod parser;
mod value;
mod interp;
mod intrinsics;

use log::info;
use std::path::{Path, PathBuf};
use std::{env, fs};
use clap::{App, Arg, AppSettings};
use crate::ast::File;
use crate::parser::parse_file;
use crate::intrinsics::get_intrinsic_ast_items;
use crate::interp::{InterpContext, interp_file, interp_entry_point};
// use crate::typeck::{TypeChecker, TyCtxt};

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
            input: Some(String::from("examples/sandbox.sq")),
            run: None,
            interpret: true,
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
    let mut working_dir = env::current_dir().unwrap_or(PathBuf::new());

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
            working_dir = path.parent().map(|p| p.to_path_buf()).unwrap_or(working_dir);
            filename = String::from(path.file_name().map(|s| s.to_str().unwrap()).unwrap());
        }

        // Parse input file
        ast_file = Some(parse_file(source, filename));
        println!("ast: {:#?}", ast_file);
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
        return;
    }

    let ast = ast_file.unwrap();
    let intrinsic_mod = get_intrinsic_ast_items();
    ast.items.push(intrinsic_mod);

    if config.interpret {
        let mut ic = InterpContext::new();
        interp_file(&mut ic, &ast);
        interp_entry_point();
    }

    // let mut sym_table = gen_sym_table(&ast);
    // let mut ty_ctxt = TyCtxt::new(&sess, &mut sym_table);
    // ast.check_type(&mut ty_ctxt);
    // if config.interpret {
        // let mut env = RuntimeEnv::new(&mut sess);
        // ast.eval(&mut env);
    // }
}
