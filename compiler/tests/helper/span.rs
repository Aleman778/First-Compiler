
/***************************************************************************
 * Helper functions for testing the span information
 ***************************************************************************/


extern crate compiler;


use crate::compiler::{
    ast::span::Span,
    parser::ParseSpan,
};
use nom_locate::LocatedSpanEx;


fn input(input: &str) -> ParseSpan {
    ParseSpan::new(input)
}


/**
 * Returns a ParseSpan from the offset and fragment.
 */
pub fn output(offset: usize, frag: &str) -> ParseSpan {
    LocatedSpanEx{
        offset: offset,
        line: 1,
        fragment: frag,
        extra: (),
    }
}


/**
 * Easily define span information to test whats left after parsing.
 */
pub fn span(offset: usize, frag: &str) -> Span {
    Span::new(output(offset, frag))
}
