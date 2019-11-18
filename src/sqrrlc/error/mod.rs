
/***************************************************************************
 * The `error` module contains diagnostic tools that is used for
 * error handling and creating rust-like error messages.
 ***************************************************************************/


use std::sync::Mutex;
use crate::sqrrlc::error::{emitter::Emitter, diagnostic::*};
use crate::sqrrlc_ast::span::Span;


pub struct Handler(Mutex<HandlerInner>);


/**
 * The error handler is used to handle errors from the compiler.
 */
pub struct HandlerInner {
    /// The number of errors encountered so far.
    pub error_count: usize,

    /// The error emitter to use.
    pub emitter: Emitter,
}


/**
 * Handler implementation.
 */
impl Handler {
    /**
     * Create a new empty handler.
     */
    pub fn new(emitter: Emitter) -> Self {
        Handler(Mutex::new(
            HandlerInner {
                error_count: 0,
                emitter: emitter,
            }
        ))
    }

    
    /**
     * Creates a new warning diagnostic with a given message.
     */
    pub fn struct_warn<'a>(&self, message: &'a str) -> Diagnostic {
        Diagnostic::new(Level::Warning, message)
    }


    /**
     * Creates a new warning diagnostic with a given message and span information.
     */
    pub fn struct_span_warn(&self, span: Span, message: &str) -> Diagnostic {
        let mut diagnostic = self.struct_warn(message);
        diagnostic.primary_span(span);
        diagnostic
    }

    
    /**
     * Creates a new error diagnostic with a given message.
     */
    pub fn struct_err<'a>(&self, message: &'a str) -> Diagnostic {
        Diagnostic::new(Level::Error, message)
    }


    /**
     * Creates a new error diagnostic with a given message and span information.
     */
    pub fn struct_span_err(&self, span: Span, message: &str) -> Diagnostic {
        let mut diagnostic = self.struct_err(message);
        diagnostic.primary_span(span);
        diagnostic
    }
   
    
    /**
     * Creates a new fatal diagnostic with a given message.
     */
    pub fn struct_fatal<'a>(&self, message: &'a str) -> Diagnostic {
        Diagnostic::new(Level::Fatal, message)
    }


    /**
     * Creates a new fatal diagnostic with a given message and span information.
     */
    pub fn struct_span_fatal(&self, span: Span, message: &str) -> Diagnostic {
        let mut diagnostic = self.struct_fatal(message);
        diagnostic.primary_span(span);
        diagnostic
    }

    
    /**
     * Emits the diagnostic, cancelled diagnostics are not emitted however.
     */
    pub fn emit_diagnostic(&self, diagnostic: &Diagnostic) {
        let mut inner = self.0.lock().unwrap();
        inner.emit_diagnostic(diagnostic);
    }
}


impl HandlerInner {
    pub fn emit_diagnostic(&mut self, diagnostic: &Diagnostic)  {
        if diagnostic.is_err() {
            self.error_count += 1;
        }
        self.emitter.emit_diagnostic(diagnostic);
    }
}    


pub mod styled_buffer;
pub mod diagnostic;
pub mod emitter;
pub mod snippet;

#[macro_use]
pub mod macros;
