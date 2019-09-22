
/***************************************************************************
 * Atom parser implementation defines parsers for atoms and values.
 ***************************************************************************/


use nom::{
    character::complete::digit1,
    bytes::complete::tag,
    combinator::map,
    branch::alt,
    Err,
};

use crate::ast::Span;
use crate::ast::atom::*;


use crate::parser::{
    IResult,
    Error,
    ErrorKind,
    Parser,
};



impl<'a> Parser<'a, P>


// impl<'a> Parser<'a, Paren<'a>> for Paren<'a> {
//     fn parse(input: Span<'a>) -> IResult<Span, Paren<'a>> {
//         map(tuple((
//             tag("("),
//             preceded(multispace0, 
//     }
// }



/**
 * Parser implementation for 32 bit unsigned integer literals.
 */
pub parse_
impl<'a> Parser<'a, LitInt<'a>> for LitInt<'a> {
    fn parse(input: Span<'a>) -> IResult<Span, LitInt<'a>> {
        let (input, digits) = digit1(input)?;
        match digits.fragment.parse::<i32>() {
            Ok(n) => Ok((input, LitInt{
                value: n,
                span: digits
            })),
            Err(e) => Err(Err::Error(Error(
                input,
                Some(digits),
                ErrorKind::ParseIntError(e),
            ))),
        }
    }
}


/**
 * Parser implementation for boolean literals.
 */
impl<'a> Parser<'a, LitBool<'a>> for LitBool<'a> {
    fn parse(input: Span<'a>) -> IResult<Span, LitBool<'a>> {
        alt((
            map(tag("true"),  |s| LitBool{value:true,  span: s}),
            map(tag("false"), |s| LitBool{value:false, span: s}),
        ))(input)
    }
}
