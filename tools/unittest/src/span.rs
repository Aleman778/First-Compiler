
/***************************************************************************
 * Helper functions for testing the span information
 ***************************************************************************/


use sqrrl::sqrrlc_ast::span::Span;
use sqrrl::sqrrlc_parser::ParseSpan;
use nom_locate::LocatedSpanEx;


/**
 * Returns a ParseSpan used as input to parser.
 */
pub fn input(input: &str) -> ParseSpan {
    ParseSpan::new_extra(input, 0)
}


/**
 * Returns a ParseSpan from the offset and fragment.
 */
pub fn output(offset: usize, frag: &str) -> ParseSpan {
    LocatedSpanEx{
        offset: offset,
        line: 1,
        fragment: frag,
        extra: 0,
    }
}


/**
 * Easily define span information to test whats left after parsing.
 */
pub fn span(offset: usize, frag: &str) -> Span {
    Span::new(output(offset, frag), 0)
}
