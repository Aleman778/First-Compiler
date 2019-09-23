
/***************************************************************************
 * Atom parser implementation defines parsers for atoms and values.
 ***************************************************************************/


use nom::{
    character::complete::{alpha1, digit1},
    character::{is_alphanumeric},
    bytes::complete::{tag, take_while1},
    combinator::{map, peek},
    branch::alt,
    Err,
};

use crate::ast::{
    Span,
    atom::*,
};


use crate::parser::{
    IResult,
    Error,
    ErrorKind,
    Parser,
};


impl<'a> Parser<'a, Ident<'a>> for Ident<'a> {
    fn parse(input: Span<'a>) -> IResult<Span, Ident<'a>> {
        peek(alt((alpha1, tag("_"))))(input)?;
        map(take_while1(|c: char| is_alphanumeric(c as u8) || c == '_'),
            |s: Span<'a>| Ident{to_string: s.fragment, span: s}
        )(input)
    }
}


/**
 * Parse either an integer or boolean literal.
 */
impl<'a> Parser<'a, Atom<'a>> for Atom<'a> {
    fn parse(input: Span<'a>) -> IResult<Span, Atom<'a>> {
        alt((
            map(LitInt::parse, |literal| Atom::Num(literal)),
            map(LitBool::parse, |literal| Atom::Bool(literal)),
        ))(input)
    }
}


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
