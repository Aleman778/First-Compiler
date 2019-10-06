
/***************************************************************************
 * Span information for storing location data used for debugging
 ***************************************************************************/


use crate::parser::ParseSpan;


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LineColumn {
    pub line: u32,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}


impl Span {
    pub fn new(s: ParseSpan) -> Self {
        Span{
            start: LineColumn{
                line: s.line,
                column: s.get_column(),
            },
            end: LineColumn{
                line: get_end_line(&s),
                column: get_end_column(&s),
            },
        }
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
