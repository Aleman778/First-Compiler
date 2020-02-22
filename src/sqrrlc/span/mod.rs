//! Span is a data structure that defines a range of characters
//! within the entire program. It is represented using global
//! byte position which means that one position is unique to
//! a particular file. Spans are used mostly for error diagnostics.


#![allow(dead_code)]


use std::rc::Rc;
use crate::sqrrlc::source_map::SourceFile;


pub mod symbol;


/**
 * Dummy span, points to position 0 and has length of 0.
 */
pub const DUMMY_SPAN: Span = Span { base: 0, len: 0 };


/**
 * The byte position in a source file.
 */
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct BytePos(pub u32);


impl BytePos {
    /**
     * Creates a new byte position from usize number.
     */
    #[inline]
    pub fn new(n: usize) -> Self {
        BytePos(n as u32)
    }
    

    /**
     * Get the index number of this byte position.
     */
    #[inline]
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}


/**
 * Span defines the base byte position and byte length, in 6 bytes.
 * This data structure should be used over SpanData when possible.
 * 
 */
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub base: u32,
    pub len: u16,
}


impl Span {
    /**
     * Creates a new span from base position and length.
     */
    pub fn new(base: usize, len: usize) -> Self {
        Span {
            base: base as u32,
            len: len as u16,
        }
    }
    
    /**
     * Creates a new span from low and high byte positions.
     */
    pub fn from_range(lo: BytePos, hi: BytePos) -> Self {
        Span {
            base: lo.0,
            len: (hi.0 - lo.0) as u16,
        }
    }


    /**
     * Checks if the span length is zero.
     */
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }


    /**
     * Get the low byte position of this span.
     */
    pub fn lo(self) -> BytePos {
        BytePos(self.base)
    }


    /**
     * Get the high byte position of this span.
     */
    pub fn hi(self) -> BytePos {
        BytePos(self.base + (self.len as u32))
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


/**
 * Local location (line and column indices) in a source file.
 * This should not be stored anywhere only used for emitting
 * and annotating error diagnostics.
 * Note: the line and column number starts at 0 instead of 1.
 */
#[derive(Debug)]
pub struct Location {
    /// The local source file.
    pub file: Rc<SourceFile>,
    /// The line index.
    pub line: usize,
    /// The column index.
    pub col: usize,
}
