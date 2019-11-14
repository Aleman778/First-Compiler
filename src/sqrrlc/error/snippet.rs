
/***************************************************************************
 * Snippet module contains helper functions to simplify rendering
 * code snippets with labled annotations.
 * Note: this code is based on the rust compiler (but somewhat simplified).
 ***************************************************************************/


use std::rc::Rc;
use crate::sqrrlc::source_map::*;
use crate::sqrrlc::error::diagnostic::*;
use crate::sqrrlc_ast::span::Span;


/**
 * File containing annotated lines used for
 * rendering code snippits for a file.
 */
#[derive(Debug)]
pub struct FileWithAnnotatedLines {
    /// The file being annotated.
    pub file: Rc<SourceFile>,

    /// List of annotated lines.
    pub lines: Vec<AnnotatedLine>,

    /// The depth of mutliline spans.
    multiline_depth: usize,
}


/**
 * Implementation of file with annotated lines.
 */
impl FileWithAnnotatedLines {
    /**
     * Collects all the annotations for every file referenced in the multispan.
     */
    fn collect_annotations(
        msp: &MultiSpan,
        source_map: &SourceMap
    ) -> Vec<FileWithAnnotatedLines> {
        
        let mut output = vec![];
        // let mut multiline_annotations = vec![];

        for (span, label) in &msp.span_labels {
            let start = span.start.column;
            let mut end = span.end.column;
        }
        
        return output;    
    }
}


/**
 * Helper funtion for add annotations to vector of file annotations.
 */
fn add_annotation_to_file(
    file_vec: &mut Vec<FileWithAnnotatedLines>,
    file: Rc<SourceFile>,
    line_index: usize,
    ann: Annotation
) {
    for slot in file_vec.iter_mut() {
        if slot.file.filename == file.filename {
            for line_slot in &mut slot.lines {
                if line_slot.line_index == line_index {
                    line_slot.annotations.push(ann);
                    return;
                }
            }
            slot.lines.push(AnnotatedLine::new(line_index, vec![ann]));
            slot.lines.sort();
            return;
        }
    }
    file_vec.push(FileWithAnnotatedLines {
        file,
        lines: vec![AnnotatedLine::new(line_index, vec![ann])],
        multiline_depth: 0,
    });
}


/**
 * Annotations for a specific line.
 */
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct AnnotatedLine {
    /// The line index starting from 0.
    pub line_index: usize,

    /// List of annotations for this line.
    pub annotations: Vec<Annotation>,
}


impl AnnotatedLine {
    /**
     * Creates a new empty annotated line object.
     */
    pub fn new(line_index: usize, annotations: Vec<Annotation>) -> Self {
        AnnotatedLine {
            line_index,
            annotations,
        }
    }

    
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


/**
 * Implementation of annotations
 */
impl Annotation {
    /**
     * Calculate the annotation
     */
    fn calc(
        _d: &Diagnostic,
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
