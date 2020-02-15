
/***************************************************************************
 * The diagnostic module defines attricutes of a compiler message.
 * Note: this code is based on the rust compiler (but simplified).
 ***************************************************************************/


use std::fmt;
use crate::sqrrlc::error::{
    Level,
    snippet::{Style, StyledString},
};
use crate::sqrrlc::span::*;


/**
 * Diagnostic struct defines a message from the compiler.
 * This can be an error message with span info and explanations.
 */
#[derive(Debug)]
pub struct Diagnostic {
    /// The diagnostic level e.g. `Level::Warning` etc.
    pub level: Level,

    /// Optional error code identifier string.
    pub code: Option<String>,

    /// The vector of error messages.
    pub message: Vec<StyledString>,

    /// The multi span is used to annotate errors in code.
    pub span: MultiSpan,

    /// Vector of sub diagnostics.
    pub children: Vec<SubDiagnostic>,
}


/**
 * Sub diagnostic e.g. note attached to an error.
 */
#[derive(Debug)]
pub struct SubDiagnostic {
    /// The diagnostic level e.g. `Level::Warning` etc.
    pub level: Level,

    /// The vector of styled error messages.
    pub message: Vec<StyledString>,

    /// The multi span is used to annotate errors in code.
    pub span: MultiSpan,
}


/**
 * Implementation of diagnostic.
 */
impl Diagnostic {
    /**
     * Create a new diagnostic.
     */
    pub fn new(level: Level, message: &str) -> Self {
        Diagnostic::new_with_code(level, None, message)
    }


    /**
     * Create a new diagnostic with a speicifc error identifier.
     */
    pub fn new_with_code(level: Level, code: Option<String>, message: &str) -> Self {
        Diagnostic {
            level: level,
            code: code,
            message: vec![StyledString::new(message.to_owned(), Style::MainHeaderMsg)],
            span: MultiSpan::new(),
            children: vec![],
        }        
    }


    /**
     * Set the primary span of this diagnostic.
     */
    pub fn primary_span(&mut self, span: Span) {
        if !span.is_empty() {
            self.span.primary_spans.push(span);
        }
    }
    

    /**
     * Create a new span label for the given 
     */
    pub fn span_label(&mut self, span: Span, label: &str) -> &mut Self {
        if !span.is_empty() {
            self.span.span_labels.push((span, label.to_owned()));
        }
        self
    }


    /**
     * Create a note sub diagnostic with a given message.
     */
    pub fn note(&mut self, message: &str) -> &mut Self {
        self.sub(Level::Note, vec![StyledString::new(message.to_owned(), Style::NoStyle)], MultiSpan::new());
        self
    }


    /**
     * Cancel this diagnostic, should not be emitted to the user.
     */
    pub fn cancel(&mut self) {
        self.level = Level::Cancelled;
    }


    /**
     * Check if this diagnostic is cancelled.
     */
    pub fn cancelled(&self) -> bool {
        self.level == Level::Cancelled
    }


    /**
     * Check if this diagnostic is an error.
     */
    pub fn is_err(&self) -> bool {
        match self.level {
            Level::Error |
            Level::Fatal => true,
            _ => false,
        }
    }
    

    /**
     * Push a new sub diagnostic to this diagnostic as child.
     */
    pub fn sub(
        &mut self,
        level: Level,
        message: Vec<StyledString>,
        span: MultiSpan,
    ) {
        let sub = SubDiagnostic {
            level,
            message,
            span,
        };
        self.children.push(sub);
    }


    /**
     * Returns true if there is a primary span being used.
     */
    pub fn has_primary_spans(&self) -> bool {
        !self.span.primary_spans.is_empty()
    }
}


/**
 * Display formatting for diagnostic levels.
 */
impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Level::Note => f.write_str("note"),
            Level::Warning => f.write_str("warning"),
            Level::Error => f.write_str("error"),
            Level::Fatal => f.write_str("fatal"),
            Level::Cancelled => f.write_str("cancelled"),
        }
    }
}
