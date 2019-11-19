
/***************************************************************************
 * Literal parser implementation defines parsers for each type of
 * literal e.g. integers, booleans etc.
 ***************************************************************************/


use nom::{
    character::complete::{digit1, multispace0},
    bytes::complete::tag,
    sequence::preceded,
    combinator::map,
    branch::alt,
    Err,
};


use crate::sqrrlc_ast::{
    span::Span,
    lit::*,
};


use crate::sqrrlc_parser::{
    error::{ParseError, ErrorKind},
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parser implementation for 32 bit unsigned integer literals.
 */
impl Parser for LitInt {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        let (input, digits) = preceded(multispace0, digit1)(input)?;
        match digits.fragment.parse::<i32>() {
            Ok(n) => Ok((input, LitInt{
                value: n,
                span: Span::new(digits, input.extra),
            })),
            Err(e) => Err(Err::Error(ParseError::new(digits, ErrorKind::ParseIntError(e)))),
        }
    }
}


/**
 * Parser implementation for boolean literals.
 */
impl Parser for LitBool {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        preceded(multispace0, alt((
            map(tag("true"),  |s| LitBool{value:true,  span: Span::new(s, input.extra)}),
            map(tag("false"), |s| LitBool{value:false, span: Span::new(s, input.extra)}),
        )))(input)
    }
}
