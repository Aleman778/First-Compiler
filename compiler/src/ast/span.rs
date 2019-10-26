#![allow(dead_code)]

/***************************************************************************
 * Span information for storing location data used for debugging
 ***************************************************************************/


use std::fmt;
use crate::parser::ParseSpan;


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
 * Display for lines and columns
 */
impl fmt::Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
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
#[derive(Clone, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
    pub file: String,
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
            file: s.extra.to_string(),
        }
    }


    /**
     * Constructs an empty span.
     */
    pub fn new_empty() -> Self {
        Span {
            start: LineColumn{line: 0, column: 0},
            end: LineColumn{line: 0, column: 0},
            file: String::new(),
        }
    }


    /**
     * Construcst a new span from a starting and end position.
     */
    pub fn from_bounds(start: LineColumn, end: LineColumn, file: &str) -> Self {
        Span {
            start: start,
            end: end,
            file: file.to_string(),
        }
    }
    
        
    /**
     * Get the fragment of this span.
     */
    pub fn fragment(&self, source: &str) -> String {
        let split = source.split("\n");
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
            println!("{}", source);
            let fragment = lines[(self.start.line - 1) as usize];
            result.push_str(&fragment[(self.start.column - 1)..(self.end.column - 1)]);
        }
        result
    }


    /**
     * Returns a string containing the location i.e. file:line:col.
     * e.g. src/main.rs:10:4
     */
    pub fn location(&self) -> String {
        format!("{}:{}", self.file, self.start.to_string())
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


    /**
     * Check if the given span is an empty span,
     * i.e. points at line 0 to 0 and column 0 to 0.
     */
    pub fn is_empty(&self) -> bool {
        return self.start.line == 0 && self.end.line == 0 &&
            self.start.column == 0 && self.end.column == 0;
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
