
/***************************************************************************
 * Snippet module contains helper functions to simplify rendering
 * code snippets with labled annotations.
 * Note: this code is based on the rust compiler (but somewhat simplified).
 ***************************************************************************/


use std::rc::Rc;
use std::cmp::{min, max};
use crate::core::source_map::*;
use crate::error::Level;
use crate::span::MultiSpan;


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
    pub multiline_depth: usize,
}


/**
 * Implementation of file with annotated lines.
 */
impl FileWithAnnotatedLines {
    /**
     * Collects all the annotations for every file referenced in the multispan.
     */
    pub fn collect_annotations(
        msp: &MultiSpan,
        sm: &SourceMap
    ) -> Vec<FileWithAnnotatedLines> {
        
        let mut output = vec![];
        let mut multiline_annotations = vec![];

        for span_label in &msp.span_labels() {
            let lo = sm.lookup_char_pos(span_label.span.lo());
            let mut hi = sm.lookup_char_pos(span_label.span.hi());
            if lo.line == hi.line && lo.col == hi.col {
                hi.col += 1;
            }

            if lo.line == hi.line {
                let ann = Annotation {
                    start_col: lo.col,
                    end_col: hi.col,
                    is_primary: span_label.is_primary,
                    label: span_label.label.clone(),
                    annotation_type: AnnotationType::SingleLine,
                };
                add_annotation_to_file(&mut output, lo.file, lo.line as usize, ann);
            } else {
                let ann = MultilineAnnotation {
                    depth: 1,
                    line_start: lo.line as usize,
                    line_end: hi.line as usize,
                    start_col: lo.col,
                    end_col: hi.col,
                    is_primary: span_label.is_primary,
                    label: span_label.label.clone(),
                    overlaps_exactly: false,
                };
                multiline_annotations.push((lo.file, ann));
            }
        }

        // Find overlapping multiline annotations and put them at different depths.
        multiline_annotations.sort_by_key(|&(_, ref ml)| (ml.line_start, ml.line_end));
        for (_, ann) in multiline_annotations.clone() {
            for (_, other_ann) in multiline_annotations.iter_mut() {
                if !(ann.same_span(other_ann)) &&
                    num_overlap(ann.line_start, ann.line_end, other_ann.line_start, other_ann.line_end, true)
                {
                    other_ann.depth += 1;
                } else if ann.same_span(other_ann) && &ann != other_ann {
                    other_ann.overlaps_exactly = true;
                } else {
                    break;
                }
            }
        }

        // Convert MultilineAnnotations into several Annotations,
        // also make sure that two overlapping annotations are shared as one.
        let mut max_depth = 0;
        for (file, ann) in multiline_annotations {
            max_depth = max(max_depth, ann.depth);
            let mut end_ann = ann.as_end();
            if !ann.overlaps_exactly {
                add_annotation_to_file(&mut output, Rc::clone(&file), ann.line_start, ann.as_start());
                let middle = min(ann.line_start + 4, ann.line_end);
                for line in ann.line_start + 1..middle {
                    add_annotation_to_file(&mut output, Rc::clone(&file), line, ann.as_line());
                }
                let line_end = ann.line_end.saturating_sub(1);
                if middle < line_end {
                    add_annotation_to_file(&mut output, Rc::clone(&file), line_end, ann.as_line());
                }
            } else {
                end_ann.annotation_type = AnnotationType::SingleLine;
            }
            add_annotation_to_file(&mut output, file, ann.line_end, end_ann);
        }
        for file_vec in output.iter_mut() {
            file_vec.multiline_depth = max_depth;
        }
        output
    }
}


/**
 * Check if two number a and b are overlapping, inclusive means that it should include
 * overlaps that are exactly on the same value.
 */
pub fn num_overlap(a_start: usize, a_end: usize, b_start: usize, b_end: usize, inclusive: bool) -> bool {
    let extra = if inclusive {
        1
    } else {
        0
    };
    (b_start..b_end + extra).contains(&a_start) ||
    (a_start..a_end + extra).contains(&b_start)
}


/**
 * Checks if two annotations are overlapping. Padding for annotation a can be added
 * e.g. to check if the label for annotation A also overlaps with annotation B.
 */
pub fn overlaps(a: &Annotation, b: &Annotation, padding: usize) -> bool {
    num_overlap(a.start_col, a.end_col + padding, b.start_col, b.end_col, false)
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
        if slot.file.name == file.name {
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
     * Returns true if this is a line multiline annotation.
     */
    pub fn is_line(&self) -> bool {
        match self.annotation_type {
            AnnotationType::MultilineLine(_) => true,
            _ => false,
        }
    }


    /**
     * Returns true if this annotation type is of multiline type.
     */
    pub fn is_multiline(&self) -> bool {
        match self.annotation_type {
            AnnotationType::Multiline(_) |
            AnnotationType::MultilineStart(_) |
            AnnotationType::MultilineLine(_) |
            AnnotationType::MultilineEnd(_) => true,
            _ => false,
        }
    }

    
    /**
     * Returns the length of this annotation.
     */
    pub fn len(&self) -> usize{
        if self.end_col > self.start_col {
            self.end_col - self.start_col
        } else {
            self.start_col - self.end_col
        }
    }


    /**
     * Check if this annotation has the same span as the provided other annotation.
     */
    pub fn same_span(&self, other: &Annotation) -> bool {
        return self.start_col == other.start_col &&
            self.end_col == other.end_col;
    }
    

    /**
     * Check if this annotation has a label and that it is a non empty label.
     */
    pub fn has_label(&self) -> bool {
        if let Some(ref label) = self.label {
            label.len() > 0
        } else {
            false
        }
    }


    /**
     * Check if this annotation takes space.
     * Mutliline start and end always have to keep vertical space.
     */
    pub fn takes_space(&self) -> bool {
        match self.annotation_type {
            AnnotationType::MultilineStart(_) |
            AnnotationType::MultilineEnd(_) => true,
            _ => false,
        }
    }
}


/**
 * Multiline annotation struct used for rendering multiline annotations
 * in code snippets.
 */
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct MultilineAnnotation {
    /// The depth of the multiline annotation.
    pub depth: usize,

    /// The starting line for this annotation.
    pub line_start: usize,

    /// The ending line for this annotation.
    pub line_end: usize,

    /// The starting column for this annotation.
    pub start_col: usize,

    /// The ending column for this annotation.
    pub end_col: usize,

    /// The annotation label string, None if it has no label.
    pub label: Option<String>,
    
    /// Is this annotation from the primary span.
    pub is_primary: bool,

    /// Does this annotation overlap another exactly.
    pub overlaps_exactly: bool,
}


impl MultilineAnnotation {
    /**
     * Returns true if both annotations have the same exact span.
     */
    pub fn same_span(&self, other: &Self) -> bool {
        return self.line_start == other.line_start && self.line_end == other.line_end &&
            self.start_col == other.start_col && self.end_col == other.end_col;
    }
    
    
    /**
     * Returns the single line annotation of the MultilineStart.
     */
    pub fn as_start(&self) -> Annotation {
        Annotation {
            start_col: self.start_col,
            end_col: self.start_col + 1,
            is_primary: self.is_primary,
            label: None,
            annotation_type: AnnotationType::MultilineStart(self.depth),
        }
    }


    /**
     * Returns the single line annotation of the MultilineEnd.
     */
    pub fn as_end(&self) -> Annotation {
        Annotation {
            start_col: self.end_col.saturating_sub(1),
            end_col: self.end_col,
            is_primary: self.is_primary,
            label: self.label.clone(),
            annotation_type: AnnotationType::MultilineEnd(self.depth),
        }
    }


    /**
     * Returns the single line annotation of the MultilineLine.
     */
    pub fn as_line(&self) -> Annotation {
        Annotation {
            start_col: self.start_col,
            end_col: self.end_col,
            is_primary: self.is_primary,
            label: None,
            annotation_type: AnnotationType::MultilineLine(self.depth),
        }
    }
}


/**
 * Defines an annotation type e.g. single line, multiline etc.
 */
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum AnnotationType {
    /// Single line annotations.
    SingleLine,

    /// The start of a multiline annotaiton, the parameter is the depth value.
    MultilineStart(usize),

    /// In between the start and end of a multiline annotation, the parameter is the depth value.
    MultilineLine(usize),

    /// The end of a multiline annotation, the parameter is the depth value.
    MultilineEnd(usize),

    /// Annotation enclosing the first and last character of the muliline span.
    Multiline(MultilineAnnotation),
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
    /// The main header message is the message of the parent diagnostic
    MainHeaderMsg,
    
    /// The header message is the message of the sub diagnostics.
    HeaderMessage,

    /// The line number display in code snippets.
    LineNumber,

    /// The line and column number of a file.
    LineAndColumn,

    /// The underline of code defined by a primary span label.
    UnderlinePrimary,

    /// The style of primary labels used for annotations.
    LabelPrimary,

    /// The underline of code defined by a secondary span label.
    UnderlineSecondary,

    /// The style of secondary labels used for annotations.
    LabelSecondary,

    /// The actual source code referenced in errors.
    Quotation,

    /// Style based on the diagnostic level.
    Level(Level),

    /// Use no style, resets the styling.
    NoStyle,
}


/**
 * Returns the correct type of underline style, either
 * the primary or secondary style.
 */
pub fn get_underline_style(is_primary: bool) -> Style {
    if is_primary {
        Style::UnderlinePrimary
    } else {
        Style::UnderlineSecondary
    }        
}


/**
 * Returns the correct type of label style, either
 * the primary or secondary style.
 */
pub fn get_label_style(is_primary: bool) -> Style {
    if is_primary {
        Style::LabelPrimary
    } else {
        Style::LabelSecondary
    }
}
