
/***************************************************************************
 * Snippet module contains helper functions to simplify rendering
 * code snippets with labled annotations.
 ***************************************************************************/


use std::rc::Rc;
use crate::sqrrlc::source_map::SourceFile;
use crate::sqrrlc::error::diagnostic::{Diagnostic, Level};
use crate::sqrrlc_ast::span::Span;


/**
 * File containing annotated lines used for
 * rendering code snippits for a file.
 */
pub struct FileWithAnnotatedLines {
    /// The file being annotated.
    pub file: Rc<SourceFile>,

    /// List of annotated lines.
    pub lines: Vec<AnnotatedLine>,
}


/**
 * Implementation of file with annotated lines.
 */
impl FileWithAnnotatedLines {

    
}



/**
 * Annotations for a specific line.
 */
pub struct AnnotatedLine {
    /// The line index starting from 0.
    pub line_index: usize,

    /// List of annotations for this line.
    pub annotations: Vec<Annotation>,
}


impl AnnotatedLine {
    /**
     * Calculate the annotations based on the current line number and diganostic reference.
     * Reutrns a list of annotations for this line.
     */
    fn calc(d: &Diagnostic, line_number: u32) -> Self {
        let mut annotations = Vec::new();
        for span in &d.span.primary_spans {
            match Annotation::calc(d, *span, "", false, line_number) {
                Some(annotation) => annotations.push(annotation),
                None => { },
            };
        }
        for (span, text) in &d.span.span_labels {
            match Annotation::calc(d, *span, text, false, line_number) {
                Some(annotation) => annotations.push(annotation),
                None => { },
            };
        }
        annotations.sort();
        // annotations.iter().rev().cloned().collect();
        AnnotatedLine {
            line_index: line_number as usize,
            annotations
        }
    }
}


/**
 * Defines an annotation with location, label and type.
 */
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Annotation {
    /// The starting column for this annotation
    pub start_col: usize,

    /// The ending column for this annotation.
    pub end_col: usize,

    /// The annotation label string, None if it has no label.
    pub label: Option<String>,

    /// Is this annotation from the primary span.
    pub is_primary: bool,

    /// The type of annotation i.e. single line, multiline etc.
    pub annotation_type: AnnotationType,
}


impl Annotation {

    /**
     * Calculate the annotation
     */
    fn calc(
        d: &Diagnostic,
        span: Span,
        text: &str,
        is_primary: bool,
        line_number: u32
    ) -> Option<Annotation> {
        if span.start.line < line_number || span.end.line > line_number {
            return None;
        }


        let start_col = span.start.column;
        let end_col = span.end.column;
        let label;
        if !text.is_empty() {
            label = Some(text.to_owned());
        } else {
            label = None;
        }
            
        let annotation_type;
        if span.is_multiline() {
            annotation_type = AnnotationType::Multiline;
        } else {
            annotation_type = AnnotationType::SingleLine;
        }

        return Some(
            Annotation {
                start_col,
                end_col,
                label,
                is_primary,
                annotation_type,
            }
        );
    }

}


/**
 * Defines an annotation type e.g. single line, multiline etc.
 */
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum AnnotationType {
    /// Single line annotations.
    SingleLine,

    /// The start of a multiline annotaiton.
    MultilineStart,

    /// In between the start and end of a multiline annotation.
    Multiline,

    /// The end of a multiline annotation.
    MultilineEnd,
}


/**
 * Styled string is a string with an attached style.
 */
#[derive(Debug)]
pub struct StyledString {
    pub text: String,
    pub style: Style,
}


/**
 * Implementation of styled string.
 */
impl StyledString {
    pub fn new(text: String, style: Style) -> Self {
        StyledString{text, style}
    }
}


/**
 * Definse different types of styles used in the printing.
 */
#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub enum Style {
    /// The header message is the message of the parent diagnostic.
    HeaderMessage,

    /// The line number display in code snippets.
    LineNumber,

    /// The underline of code defined by one span label.
    Underline,

    /// The underline of code defined by the primary span label.
    UnderlinePrimary,

    /// Style based on the diagnostic level.
    Level(Level),

    /// Use no style, resets the styling.
    NoStyle,
}
