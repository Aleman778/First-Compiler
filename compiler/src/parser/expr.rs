
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    combinator::map,
    error::context,
};


use crate::ast::{
    span::Span,
    atom::ExprIdent,
    // expr::*,
};


use crate::parser::{
    token::parse_ident,
    ParseSpan,
    IResult,
    Parser,
};


/* TEMPLATE USED FOR COPYING

impl Parser for  {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "",
        )(input)
    }
}

*/


/**
 * Parse an identifier e.g. test_id.
 */
impl Parser for ExprIdent {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "identifier",
            map(parse_ident, |(span, id)| ExprIdent{to_string: id.to_string(), span: Span::new(span)})
        )(input)
    }
}
