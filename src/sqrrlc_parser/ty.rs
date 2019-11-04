
/***************************************************************************
 * Type Parser implementation defines parsers for all the supported types.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, tuple},
    branch::alt,
    error::context,
};
use crate::sqrrlc_ast::{
    span::{Span, LineColumn},
    ty::*,
};
use crate::sqrrlc_parser::{
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse a type can be i32, bool or a reference.
 */
impl Parser for Type {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "type",
            alt((
                map(preceded(multispace0, tag("i32")), |s| Type::Int32{span: Span::new(s)}),
                map(preceded(multispace0, tag("bool")), |s| Type::Bool{span: Span::new(s)}),
                map(preceded(multispace0, TypeReference::parse), |reference| Type::Reference(reference)),
            ))
        )(input)    
    }
}


/**
 * Parse a type reference or mutable reference.
 */
impl Parser for TypeReference {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "type reference",
            map(tuple((
                preceded(multispace0, tag("&")),
                opt(preceded(multispace0, tag("mut"))),
                preceded(multispace1, Type::parse),
            )),
                |(and, mutability, elem)| {
                    let elem_span = elem.get_span();
                    TypeReference {
                        mutability: mutability.is_some(),
                        elem: Box::new(elem),
                        span: Span::from_bounds(
                            LineColumn::new(and.line, and.get_column()),
                            elem_span.end, input.extra
                        )
                    }
                }
            )
        )(input)
    }
}
