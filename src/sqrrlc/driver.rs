
/***************************************************************************
 * The compilation driver is what runs the actual passes in the compiler.
 * The main.rs file immediately calls this driver::main()
 ***************************************************************************/


use std::path::{Path, PathBuf};
use std::{rc::Rc, env};
use crate::sqrrlc::{
    session::Session,
    source_map::{SourceMap, Filename},
};
use crate::sqrrlc_ast::base::File;
use crate::sqrrlc_parser::{
    Parser, ParseSpan
};
use crate::sqrrlc_interp::{
    debug::debug_functions,
    env::RuntimeEnv
};
use crate::sqrrlc_typeck::{
    TypeChecker, TyCtxt,
};
use crate::sqrrlc::symbol::{
    generator::gen_sym_table
};


/**
 * The input defines the compilation source code target.
 * This can either contain the actual code or reference
 * the code via file.
 */
pub enum Input {
    /// Load input source code from file.
    File(PathBuf),
    /// Run input source code from string.
    Code {
        /// The name can be either a real file or custom name.
        name: Filename,
        /// The string containign the source code to compile.
        input: String
    }
}


/**
 * Compiler configuations defines variables that
 * the compiler uses to build the program.
 */
pub struct Config {
    /// The input is the target that will be compiled.
    input: Input,
    /// Input directory is the location of the.
    input_dir: Option<PathBuf>,
    /// The output file.
    output_file: Option<PathBuf>,
    /// The output directory.
    output_dir: Option<PathBuf>,
}


/**
 * The main function for running the actual compiler.
 * The compiler needs to be configured using the provided config struct.
 */
pub fn run_compiler(config: Config) {
    let input_dir = match config.input_dir {
        Some(dir) => dir,
        None => env::current_dir().unwrap()
    };
    let source_map = Rc::new(SourceMap::from_dir(&input_dir));
    let mut sess = Session::from_dir(input_dir);
    let file = match config.input {
        Input::File(file) => {
            match sess.source_map().load_file(&file) {
                Ok(file) => file,
                Err(err) => {
                    let err = struct_err!(
                        sess,
                        "could not read the file `{}`: {}",
                        file.display(), err
                    );
                    sess.emit(&err);
                    return;
                }
            }
        }
        Input::Code{name, input} => sess.source_map().add_from_source(name, input, 0, 0),
    };
    let span = ParseSpan::new_extra(&file.source, 0);
    let mut ast = File::parse(span).unwrap().1;
    ast.extend(debug_functions());
    let mut sym_table = gen_sym_table(&ast);
    let mut ty_ctxt = TyCtxt::new(&sess, &mut sym_table);
    ast.check_type(&mut ty_ctxt);
    let mut env = RuntimeEnv::new(&mut sess);
    ast.eval(&mut env);
}


/**
 * The main function of the compilation driver,
 * handles the command line interface.
 */
pub fn main() {
    use clap::{App, Arg, AppSettings};
    
    let matches = App::new("sqrrlc")
        .setting(AppSettings::ArgRequiredElseHelp)
        .arg(Arg::with_name("INPUT")
             .help("The input source file to compile")
             .value_name("FILE")
             .index(1))
        .arg(Arg::with_name("run")
             .short("r")
             .value_name("CODE")
             .help("Code that executes the code before running main"))
        .arg(Arg::with_name("output")
             .short("o")
             .value_name("FILE")
             .help("Write output to <filename>"))
        .arg(Arg::with_name("version")
             .short("V")
             .help("Print version output and exit"))
        .arg(Arg::with_name("verbose")
             .short("v")
             .help("Enable verbose output"))
        .get_matches();

    if let Some(path_str) = matches.value_of("INPUT") {
        let input_path = Path::new(path_str);
        let input = Input::File(input_path.to_path_buf());
        let input_dir = if input_path.is_absolute() {
            input_path.parent().map(|p| p.to_path_buf())
        } else {
            None
        };
        let output_path = matches.value_of("output").map(|s| Path::new(s));
        let output_file = output_path.map(|p| PathBuf::from(p.file_name().unwrap()));
        let output_dir = output_path.map(|p| p.parent().unwrap().to_path_buf());
        let config = Config {
            input,
            input_dir,
            output_file,
            output_dir,
        };
        run_compiler(config);
    } else if let Some(code) = matches.value_of("run") {
        let input = Input::Code {
            name: Filename::Custom(String::from("run")),
            input: String::from(code),
        };
        let config = Config {
            input,
            input_dir: None,
            output_file: None,
            output_dir: None,
        };
        run_compiler(config);
    }
}
