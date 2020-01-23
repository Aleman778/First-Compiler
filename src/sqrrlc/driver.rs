
/***************************************************************************
 * The compilation driver is what runs the actual passes in the compiler.
 * The main.rs file immediately calls this driver::main()
 ***************************************************************************/


use clap::{App, Arg, ArgMatches, AppSettings};
use std::path::{Path, PathBuf};
use std::env;
use crate::sqrrlc::source_map::FileLoader;


/**
 * Filename defines different kinds of 
 * refering to a file, e.g. path and filename.
 */
enum Filename {
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
enum Input {
    /// Load input source code from file.
    File(PathBuf),
    /// Run input source code from string.
    Str {
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
struct Config {
    /// The input is the target that will be compiled.
    input: Input,
    /// Input directory is the location of the.
    input_dir: Option<PathBuf>,
    /// The output file.
    output_file: Option<PathBuf>,
    /// The output directory.
    output_dir: Option<PathBuf>,
    /// The file loader to use.
    file_loader: Option<Box<dyn FileLoader + Sync + Send>>,
}



fn run_compiler(config: Config) {
    
    
    // println!("{}", filepath.display());
    
}



/**
 * The main function of the compilation driver,
 * handles the command line interface.
 */
pub fn main() {
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

    // let input = read_input();
    
}


fn read_input(matches: ArgMatches) -> Option<Input> {
    // if let Some(file) = matches.get_value("INPUT") {
        // let input = 
    // } else {
        
    // }

    None
}
