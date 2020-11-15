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
 * the end of the span location. The location is a pointer to a source file in the source mapper.
 */
#[derive(Copy, Clone, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
    pub loc: usize,
}

/**
 * Implementation of span.
 */
impl Span {
    /**
     * Constructs a new span from a parse span.
     */
    pub fn new(s: ParseSpan, file_id: usize) -> Self {
        Span {
            start: LineColumn {
                line: s.line,
                column: s.get_column(),
            },
            end: LineColumn {
                line: get_end_line(&s),
                column: get_end_column(&s),
            },
            loc: file_id,
        }
    }

    /**
     * Constructs an empty span.
     */
    pub fn new_empty() -> Self {
        Span {
            start: LineColumn{line: 0, column: 0},
            end: LineColumn{line: 0, column: 0},
            loc: 0,
        }
    }

    /**
     * Construcst a new span from a starting and end position.
     */
    pub fn from_bounds(start: LineColumn, end: LineColumn, file_id: usize) -> Self {
        Span {
            start: start,
            end: end,
            loc: file_id,
        }
    }

    /**
     * Get the fragment of this span.
     */
    pub fn fragment(&self, source: &str) -> String {
        let split = source.split("\n");
        let lines: Vec<&str> = split.collect();
        let mut result = String::new();
        if self.is_multiline() {
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
    pub fn is_multiline(&self) -> bool {
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

    /**
     * Check if two spans are located in the exact same location.
     */
    pub fn same_span(self, other: Span) -> bool {
        return self.start.line == other.start.line && self.end.line == other.end.line &&
            self.start.column == other.start.column && self.end.column == other.end.column;
    }

    /**
     * Check if other span is fully enclosed in this span.
     */
    pub fn contains(self, other: Span) -> bool {
        if self.start.line < other.start.line && self.end.line > other.end.line {
            true
        } else if self.start.line == other.start.line && self.end.line == other.end.line {
            self.start.column <= other.start.column && self.end.column >= other.end.column
        } else {
            false
        }
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
