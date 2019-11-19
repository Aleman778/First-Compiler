
/***************************************************************************
 * Compiler session module defines a general session struct that is used
 * throughout the compilation to hold the current state of the compiler.
 ***************************************************************************/


use std::rc::Rc;
use std::path::PathBuf;
use crate::sqrrlc::{
    error::{diagnostic::*, emitter::Emitter, Handler},
    source_map::SourceMap,
};
use crate::sqrrlc_ast::span::Span;


/**
 * The session struct defines the current compilation state,
 * e.g. error handler, compilation target, current directory.
 */
pub struct Session {
    // The handler is used to deal with error reporting.
    pub handler: Handler,

    // The working directory of the compiler.
    pub working_dir: PathBuf,

    // The mapping of source files in use.
    source_map: Rc<SourceMap>,
}


impl Session {
    /**
     * Creats a new empty session without specified working directory.
     */
    pub fn new() -> Self {
        Session::with_dir(std::env::current_dir().unwrap())
    }
    
    
    /**
     * Create a new empty session with a specific working directory.
     */
    pub fn with_dir(working_dir: PathBuf) -> Self {
        let src_map = Rc::new(SourceMap::new(working_dir.clone()));
        Session {
            handler: Handler::new(Emitter::new(Rc::clone(&src_map))),
            working_dir: working_dir,
            source_map: src_map,
        }
    }


    /**
     * Returns reference to the source map.
     */
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }
    
    
    /**
     * Emits a warning diagnostic with a given message.
     */
    pub fn warn<'a>(&self, message: &'a str) {
        let diagnostic = self.handler.struct_warn(message);
        self.emit(&diagnostic);
    }


    /**
     * Emits a warning diagnostic with a given message and span information.
     */
    pub fn span_warn(&self, span: Span, message: &str) {
        let diagnostic = self.handler.struct_span_warn(span, message);
        self.emit(&diagnostic);
    }

    
    /**
     * Creates a new warning diagnostic with a given message.
     */
    pub fn struct_warn<'a>(&self, message: &'a str) -> Diagnostic {
        self.handler.struct_warn(message)
    }


    /**
     * Creates a new warning diagnostic with a given message and span information.
     */
    pub fn struct_span_warn(&self, span: Span, message: &str) -> Diagnostic {
        self.handler.struct_span_warn(span, message)
    }

    
    /**
     * Emits a error diagnostic with a given message.
     */
    pub fn err<'a>(&self, message: &'a str) {
        let diagnostic = self.handler.struct_err(message);
        self.emit(&diagnostic);
    }


    /**
     * Emits a warning diagnostic with a given message and span information.
     */
    pub fn span_err(&self, span: Span, message: &str) {
        let diagnostic = self.handler.struct_span_err(span, message);
        self.emit(&diagnostic);
    }
    
    
    /**
     * Creates a new error diagnostic with a given message.
     */
    pub fn struct_err<'a>(&self, message: &'a str) -> Diagnostic {
        self.handler.struct_err(message)
    }


    /**
     * Creates a new error diagnostic with a given message and span information.
     */
    pub fn struct_span_err(&self, span: Span, message: &str) -> Diagnostic {
        self.handler.struct_span_err(span, message)
    } 

    
    /**
     * Emits a fatal diagnostic with a given message.
     */
    pub fn fatal<'a>(&self, message: &'a str) {
        let diagnostic = self.handler.struct_fatal(message);
        self.emit(&diagnostic);
    }


    /**
     * Emits a fatal diagnostic with a given message and span information.
     */
    pub fn span_fatal(&self, span: Span, message: &str) {
        let diagnostic = self.handler.struct_span_fatal(span, message);
        self.emit(&diagnostic);
    }
    
    
    /**
     * Creates a new warning diagnostic with a given message.
     */
    pub fn struct_fatal<'a>(&self, message: &'a str) -> Diagnostic {
        self.handler.struct_fatal(message)
    }
    
    
    /**
     * Creates a new warning diagnostic with a given message and span information.
     */
    pub fn struct_span_fatal<'a>(&self, span: Span, message: &str) -> Diagnostic {
        self.handler.struct_span_fatal(span, message)
    }


    /**
     * Emits the diagnostic to be displayed.
     */
    pub fn emit(&self, diagnostic: &Diagnostic) {
        self.handler.emit_diagnostic(diagnostic);
    }
}
