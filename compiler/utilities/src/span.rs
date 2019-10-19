
/***************************************************************************
 * Helper functions for testing the span information
 ***************************************************************************/

extern crate compiler;

use compiler::{
    ast::span::Span,
    parser::ParseSpan,
};
use nom_locate::LocatedSpanEx;
use std::path::Path;


/**
 * Returns a ParseSpan used as input to parser.
 */
pub fn input(input: &str) -> ParseSpan {
    ParseSpan::new_extra(input, Path::new(""))
}


/**
 * Returns a ParseSpan from the offset and fragment.
 */
pub fn output(offset: usize, frag: &str) -> ParseSpan {
    LocatedSpanEx{
        offset: offset,
        line: 1,
        fragment: frag,
        extra: Path::new(""),
    }
}


/**
 * Easily define span information to test whats left after parsing.
 */
pub fn span(offset: usize, frag: &str) -> Span {
    Span::new(output(offset, frag))
}
