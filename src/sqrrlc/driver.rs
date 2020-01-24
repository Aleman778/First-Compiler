
/***************************************************************************
 * The compilation driver is what runs the actual passes in the compiler.
 * The main.rs file immediately calls this driver::main()
 ***************************************************************************/


use std::path::{Path, PathBuf};
use std::env;


/**
 * Filename defines different kinds of 
 * refering to a file, e.g. path and filename.
 */
#[derive(Debug)]
pub enum Filename {
    /// The filename is based on the real file path.
    Real(PathBuf),
    /// The filename is a custom string.
    Custom(String),
}


/**
 * The input defines the compilation source code target.
 * This can either contain the actual code or reference
 * the code via file.
 */
#[derive(Debug)]
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
#[derive(Debug)]
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



pub fn run_compiler(config: Config) {
    println!("{:#?}", config);
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
             .required(true)
             .value_name("FILENAME")
             .index(1))
        .arg(Arg::with_name("run")
             .short("r")
             .value_name("CODE")
             .help("Code that executes the code before running main"))
        .arg(Arg::with_name("output")
             .short("o")
             .value_name("FILENAME")
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
        let input_dir = if input_path.is_relative() {
            env::current_dir().ok()
        } else {
            if let Some(p) = input_path.parent() {
                Some(p.to_path_buf())
            } else {
                None
            }
        };
        let output_path = matches.value_of("o").map(|s| Path::new(s));;
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
