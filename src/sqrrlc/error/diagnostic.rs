
/***************************************************************************
 * The diagnostic module defines attricutes of a compiler message.
 * Note: this code is based on the rust compiler (but simplified).
 ***************************************************************************/


use std::fmt;
use crate::sqrrlc::error::snippet::{Style, StyledString};
use crate::sqrrlc_ast::span::Span;


/**
 * The level of importance for this diagnostic.
 */
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Level {
    /// Note diagnostic, provides extra info to user.
    Note,
    
    /// Warning diagnostic, can still compile fine.
    Warning,
    
    /// Error diagnostic, won't compile but can still continue.
    Error,

    /// Fatal diagnstic, won't compile anymore and should abort immediately.
    Fatal,

    /// Cancelled diagnostic, diagnostic should be ignored and not emitted.
    Cancelled,
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
    pub message: Vec<StyledString>,

    /// The multi span is used to annotate errors in code.
    pub span: MultiSpan,

    /// Vector of sub diagnostics.
    pub children: Vec<SubDiagnostic>,
}


/**
 * Sub diagnostic e.g. note attached to an error.
 */
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


/**
 * Multi span struct is a data structure that holds mutliple spans
 * used for error dignostics.
 */
#[derive(Debug)]
pub struct MultiSpan {
    /// The primary span defines where the error is located, highlighted with `^^^`.
    pub primary_spans: Vec<Span>,

    /// Span lables are secondary but some can provide labels to primary spans.
    pub span_labels: Vec<(Span, String)>,
}


/**
 * Implementation of multispan.
 */
impl MultiSpan {
    /**
     * Creates a new empty multispan object.
     */
    pub fn new() -> Self {
        MultiSpan {
            primary_spans: vec![],
            span_labels: vec![],
        }
    }


    /**
     * Returns the primary span.
     */
    pub fn primary_span(&self) -> Option<Span> {
        self.primary_spans.first().cloned()
    }
    
    
    /**
     * Returns list of span labels.
     */
    pub fn span_labels(&self) -> Vec<SpanLabel> {
        let is_primary = |span| self.primary_spans.contains(&span);
        let mut span_labels = self.span_labels.iter().map(|&(span, ref label)|
            SpanLabel {
                span: span,
                is_primary: is_primary(span),
                label: Some(label.clone()),
            }
        ).collect::<Vec<_>>();

        for &span in &self.primary_spans {
            if !span_labels.iter().any(|sl| sl.span == span) {
                span_labels.push(SpanLabel {
                    span,
                    is_primary: true,
                    label: None,
                });
            }
        }
        span_labels
    }
}


/**
 * Span label struct defines a span with an attacked label.
 */
#[derive(Debug)]
pub struct SpanLabel {
    /// The span to include in the snippet.
    pub span: Span,

    /// Is this a primary span? Underline those with ^^^ versus ---.
    pub is_primary: bool,

    /// What label should be attached to this span (if any)?
    pub label: Option<String>,
}
