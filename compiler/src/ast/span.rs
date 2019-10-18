#![allow(dead_code)]

/***************************************************************************
 * Span information for storing location data used for debugging
 ***************************************************************************/


use crate::parser::ParseSpan;
use std::fmt;


/**
 * Contains line and column numbers.
 */
#[derive(Clone, Copy, PartialEq)]
pub struct LineColumn {
    pub line: u32,
    pub column: usize,
}


/**
 * Implementation of line column struct.
 */
impl LineColumn {
    /**
     * Constructor for convenience.
     */
    pub fn new(line: u32, col: usize) -> Self {
        LineColumn {
            line: line,
            column: col,
        }
    }
}
    

/**
 * To string for lines and columns
 */
impl std::string::ToString for LineColumn {
    fn to_string(&self) -> String {
        format!("{}:{}", self.line, self.column)
    }
}


/**
 * Debug print for line column.
 */
impl fmt::Debug for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ line: {}, col: {} }}", self.line, self.column)
    }
}


/**
 * Custom span struct only includes lines and columns from the start to
 * the end of the span location.
 */
#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}


/**
 * Implementation of span.
 */
impl Span {
    /**
     * Constructs a new span from a parse span.
     */
    pub fn new(s: ParseSpan) -> Self {
        Span {
            start: LineColumn {
                line: s.line,
                column: s.get_column(),
            },
            end: LineColumn {
                line: get_end_line(&s),
                column: get_end_column(&s),
            },
        }
    }


    /**
     * Construcst a new span from a starting and end position.
     */
    pub fn from_bounds(start: LineColumn, end: LineColumn) -> Self {
        Span {
            start: start,
            end: end,
        }
    }
    

    /**
     * Constructs a new combined span from the given spans.
     */
    pub fn combined(spans: &[&ParseSpan]) -> Self {
        Span {
            start: LineColumn {
                line: spans[0].line,
                column: spans[0].get_column(),
            },
            end: LineColumn {
                line: get_end_line(&spans[spans.len() - 1]),
                column: get_end_column(&spans[spans.len() - 1]),
            },
        }        
    }
    
        
    /**
     * Get the fragment of this span.
     * @NotFullyImplemented
     */
    pub fn fragment<'a>(&self, src: &ParseSpan<'a>) -> String {
        let split = src.fragment.split("\n");
        let lines: Vec<&str> = split.collect();
        let mut result = String::new();
        if self.multiline() {
            for line in self.start.line..self.end.line {
                if line == self.start.line {
                    let fragment = lines[(line - 1) as usize];
                    result.push_str(&fragment[(self.start.column-1)..]);
                } else if line == self.end.line - 1 {
                    let fragment = lines[(line - 1) as usize];
                    result.push_str(&fragment[..(self.end.column-1)]);
                } else {
                    result.push_str(&lines[(line - 1) as usize]);    
                }
            }
        } else {
            let fragment = lines[(self.start.line - 1) as usize];
            result.push_str(&fragment[(self.start.column - 1)..(self.end.column - 1)]);
        }
        result
    }
    

    /**
     * Get the offset to the start of the span.
     */
    pub fn offset(&self) -> usize {
        self.start.column
    }


    /**
     * Check if the span includes more than one line.
     */
    pub fn multiline(&self) -> bool {
        self.end.line > self.start.line
    }
}


impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span {{ line: {}-{}, col: {}-{} }}", self.start.line,
               self.end.line, self.start.column, self.end.column)
    }
}



/***************************************************************************
 * Helper methods for calculating the ending lines and columns
 ***************************************************************************/


/**
 * Get the end line.
 */
fn get_end_line(s: &ParseSpan) -> u32 {
    s.line + (s.fragment.matches("\n").count() as u32)
}


/**
 * Get the end column.
 */
fn get_end_column(s: &ParseSpan) -> usize {
    match s.fragment.rfind("\n") {
        Some(pos) => s.fragment.len() - pos + 2,
        None => s.get_column() + s.fragment.len(),
    }
}
