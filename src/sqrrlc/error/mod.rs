
/***************************************************************************
 * The `error` module contains diagnostic tools that is used for
 * error handling and creating rust-like error messages.
 ***************************************************************************/


use crate::sqrrlc::error::{emitter::Emitter, diagnostic::*};
use crate::sqrrlc_ast::span::Span;


/**
 * The error handler is used to handle errors from the compiler.
 */
pub struct Handler {
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
        Handler {
            error_count: 0,
            emitter: emitter,
        }
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
    pub fn struct_span_warn(&self, message: &str, span: Span) -> Diagnostic {
        let mut diagnostic = self.struct_warn(message);
        diagnostic.set_span(span);
        diagnostic
    }
    
}



pub mod diagnostic;
pub mod emitter;
