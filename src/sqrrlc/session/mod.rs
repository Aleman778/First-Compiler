
/***************************************************************************
 * Compiler session module defines a general session struct that is used
 * throughout the compilation to hold the current state of the compiler.
 ***************************************************************************/


use crate::sqrrlc::error::diagnostic::*;
use crate::sqrrlc_ast::span::Span;


/**
 * The session struct defines the current compilation state,
 * e.g. error handler, compilation target, current directory.
 */
#[derive(Debug)]
pub struct Session {
    // The handler is used to deal with error reporting.
    // pub handler: Handler,

    // The working directory of the compiler.
    // pub working_dir: PathBuf,

    // The mapping of source files in use.
    // pub source_map: SourceMap,

}


impl Session {
    /**
     * Creates a new warning diagnostic with a given message.
     */
    pub fn struct_warn<'a>(&self, message: &'a str) -> Diagnostic {
        Diagnostic::new(Level::Warning, message)
    }


    /**
     * Creates a new warning diagnostic with a given message and span information.
     */
    pub fn struct_span_warn(&self, message: &str, span: Span) -> Diagnostic {
        let mut diagnostic = self.struct_warn(message);
        diagnostic.span = span;
        diagnostic
    }
    
}
