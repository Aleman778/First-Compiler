//! Compiler session module defines a general session struct that is used
//! throughout the compilation to hold the current state of the compiler.


use std::rc::Rc;
use std::path::PathBuf;
use crate::core::{
    utils::ColorConfig,
    error::{diagnostic::*, emitter::Emitter, Handler},
    span::symbol::SymbolMap,
    source_map::SourceMap,
};
use crate::core::span::Span;


/**
 * The session struct holds compilation data,
 */
pub struct Session<'a> {
    /// Handler is used to deal with error reporting.
    pub handler: Handler,
    /// Working directory of the compiler.
    pub working_dir: PathBuf,
    /// Mapping of source files in use.
    pub source_map: Rc<SourceMap>,
    /// Mapping strings to symbols and vice versa.
    pub symbol_map: SymbolMap<'a>,
}


impl<'a> Session<'a> {
    /**
     * Creats a new empty session without specified working directory.
     */
    pub fn new() -> Self {
        Session::from_dir(std::env::current_dir().unwrap())
    }
    
    
    /**
     * Create a new empty session with a specific working directory.
     */
    pub fn from_dir(working_dir: PathBuf) -> Self {
        let source_map = Rc::new(SourceMap::from_dir(&working_dir.as_path()));
        Session {
            handler: Handler::new(Emitter::stderr(
                Rc::clone(&source_map),
                None,
                ColorConfig::Always)),
            working_dir,
            source_map,
            symbol_map: SymbolMap::new(), 
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
    pub fn warn(&self, message: &'a str) {
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
    pub fn struct_warn(&self, message: &'a str) -> Diagnostic {
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
    pub fn err(&self, message: &'a str) {
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
    pub fn struct_err(&self, message: &'a str) -> Diagnostic {
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
    pub fn fatal(&self, message: &'a str) {
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
    pub fn struct_fatal(&self, message: &'a str) -> Diagnostic {
        self.handler.struct_fatal(message)
    }
    
    
    /**
     * Creates a new warning diagnostic with a given message and span information.
     */
    pub fn struct_span_fatal(&self, span: Span, message: &str) -> Diagnostic {
        self.handler.struct_span_fatal(span, message)
    }


    /**
     * Emits the diagnostic to be displayed.
     */
    pub fn emit(&self, diagnostic: &Diagnostic) {
        self.handler.emit_diagnostic(diagnostic);
    }
}
