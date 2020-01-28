
/***************************************************************************
 * The compilation driver is what runs the actual passes in the compiler.
 * The main.rs file immediately calls this driver::main()
 ***************************************************************************/


use log::info;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::env;
use crate::sqrrlc::{
    session::Session,
    error::{emitter::Emitter, Handler},
    utils::ColorConfig,
    source_map::{SourceMap, Filename},
    symbol::generator::gen_sym_table,
};
use crate::sqrrlc_ast::base::{File, Item};
use crate::sqrrlc_parser::{Parser, ParseSpan};
use crate::sqrrlc_interp::env::RuntimeEnv;
use crate::sqrrlc_typeck::{TypeChecker, TyCtxt};


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
    /// Run the interpreter on the input.
    interpret: bool,
    /// Disable colored text in errors.
    nocolor: bool,
}


/**
 * The main function for running the actual compiler.
 * The compiler needs to be configured using the provided config struct.
 */
pub fn run_compiler(config: Config) {
    info!("setting up the compiler");
    let input_dir = match config.input_dir {
        Some(dir) => dir,
        None => env::current_dir().expect("no working directory found"),
    };
    let color_config = if config.nocolor {
        ColorConfig::Never
    } else {
        ColorConfig::Always
    };
    let source_map = Rc::new(SourceMap::from_dir(&input_dir.as_path()));
    let mut sess = Session {
        handler: Handler::new(Emitter::stderr(Rc::clone(&source_map), None, color_config)),
        working_dir: input_dir,
        source_map,
    };
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
    let span = ParseSpan::new_extra(&file.source, file.id);
    let mut ast = File::parse(span).unwrap().1;
    ast.extend(parse_stdlib_basic(sess.source_map()));
    let mut sym_table = gen_sym_table(&ast);
    let mut ty_ctxt = TyCtxt::new(&sess, &mut sym_table);
    ast.check_type(&mut ty_ctxt);
    if config.interpret {
        let mut env = RuntimeEnv::new(&mut sess);
        ast.eval(&mut env);
    }
}


/**
 * Parses the FFI items in the stdlib basic.sq file.
 */
pub fn parse_stdlib_basic(source_map: &SourceMap) -> Vec<Item> {
    let directory = env::var("SQRRLC_LIBSTD_DIR").unwrap_or("src/libstd/".to_string());
    let file = source_map.load_file(Path::new(&(directory + "basic.sq")))
        .expect("could not find basic.sq in the std library directory");
    let span = ParseSpan::new_extra(&file.source, file.id);
    let ast = File::parse(span).unwrap().1;
    ast.items
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
        .arg(Arg::with_name("interpret")
             .short("i")
             .help("Runs the interpreter on the given input"))
        .arg(Arg::with_name("output")
             .short("o")
             .value_name("FILE")
             .help("Write output to <filename>"))
        .arg(Arg::with_name("version")
             .short("V")
             .long("version")
             .help("Print version output and exit"))
        .arg(Arg::with_name("verbose")
             .short("v")
             .help("Enable verbose output"))
        .arg(Arg::with_name("nocolor")
             .long("nocolor")
             .help("Disables colored text in errors"))
        .get_matches();

    if matches.is_present("version") {
        const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
        println!("sqrrlc {}", VERSION.unwrap_or("unknown version"));
    }
    
    let interpret = matches.is_present("interpret");
    let nocolor = matches.is_present("nocolor");
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
            interpret,
            nocolor,
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
            interpret,
            nocolor,
        };
        run_compiler(config);
    }
}
