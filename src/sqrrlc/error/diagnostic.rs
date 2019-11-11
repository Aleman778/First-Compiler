
/***************************************************************************
 * The diagnostic module defines attricutes of a compiler message.
 ***************************************************************************/


use std::fmt;
use crate::sqrrlc_ast::span::Span;


/**
 * The level of importance for this diagnostic.
 */
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum Level {
    /// Note diagnostic, provides extra info to user.
    Note,
    
    /// Warning diagnostic, can still compile fine.
    Warning,
    
    /// Error diagnostic, won't compile but can still continue.
    Error,

    /// Fatal diagnstic, won't compile anymore and should abort immediately.
    Fatal,
}


/**
 * Diagnostic struct defines a message from the compiler.
 * This can be an error message with span info and explanations.
 */
pub struct Diagnostic {
    /// The diagnostic level e.g. `Level::Warning` etc.
    pub level: Level,

    /// Optional error code identifier string.
    pub code: Option<String>,

    /// The vector of error messages.
    pub message: Vec<String>,

    /// The primary span to use.
    pub primary_span: Span,

    /// Vector of labels with span information.
    pub labels: Vec<(Span, String)>,

    /// Vector of sub diagnostics.
    pub children: Vec<SubDiagnostic>,
}


/**
 * Sub diagnostic e.g. note attached to an error.
 */
pub struct SubDiagnostic {
        /// The diagnostic level e.g. `Level::Warning` etc.
    pub level: Level,

    /// The vector of error messages.
    pub message: Vec<String>,

    /// The primary span to use.
    pub primary_span: Span,

    /// Vector of labels with span information.
    pub labels: Vec<(Span, String)>,
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
            message: vec![message.to_owned()],
            primary_span: Span::new_empty(),
            labels: vec![],
            children: vec![],
        }        
    }


    /**
     * Create a new span label for the given 
     */
    pub fn span_label(&mut self, span: Span, label: &str) -> &mut Self {
        self.labels.push((span, label.to_owned()));
        self
    }


    /**
     * Create a note sub diagnostic with a given message.
     */
    pub fn note(&mut self, message: &str) -> &mut Self {
        self.sub(Level::Note, vec![message.to_owned()], Span::new_empty(), vec![]);
        self
    }


    /**
     * Push a new sub diagnostic to this diagnostic as child.
     */
    pub fn sub(&mut self, level: Level, message: Vec<String>, primary_span: Span, labels: Vec<(Span, String)>) {
        let sub = SubDiagnostic {
            level,
            message,
            primary_span,
            labels,
        };
        self.children.push(sub);
    }


    /**
     * Returns the message string from all the separate messages.
     */
    pub fn message(&self) -> String {
        self.message.iter().map(|i| i.as_str()).collect::<String>()
    }


    /**
     * Set the primary span of this diagnostic.
     */
    pub fn set_span(&mut self, span: Span) {
        self.primary_span = span;
    }


    /**
     * Returns true if there is a primary span being used.
     */
    pub fn has_primary_span(&self) -> bool {
        self.primary_span.is_empty()
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
        }
    }
}
