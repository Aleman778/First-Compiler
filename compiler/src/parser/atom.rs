#![allow(dead_code)]


/***************************************************************************
 * Atom parser implementation defines parsers for atoms and values.
 ***************************************************************************/


use nom::{
    character::complete::{alpha1, digit1},
    character::{is_alphanumeric},
    bytes::complete::{tag, take_while1},
    combinator::{map, peek},
    sequence::{pair, delimited},
    branch::alt,
    error::context,
    Err,
};


use crate::ast::{
    span::Span,
    atom::*,
};


use crate::parser::{
    error::{ParseError, ErrorKind},
    token::parse_math_expr,
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse a parenthesized expression e.g. (2 + 6) 
 */
impl Parser for ExprParen {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(delimited(tag("("), parse_math_expr, tag(")")),
            |expr| ExprParen{expr: Box::new(expr), span: Span::new(input)}
        )(input)
    }
}


/**
 * Parse an identifier e.g. test_id.
 */
impl Parser for ExprIdent {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "identifier",
            map(pair(
                peek(alt((alpha1, tag("_")))),
                take_while1(|c: char| is_alphanumeric(c as u8) || c == '_')
            ), |(_, s): (ParseSpan, ParseSpan)| ExprIdent{to_string: s.fragment.to_string(), span: Span::new(s)})
        )(input)
    }    
}


/**
 * Parser implementation for 32 bit unsigned integer literals.
 */
impl Parser for ExprLitInt {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        let (input, digits) = digit1(input)?;
        match digits.fragment.parse::<i32>() {
            Ok(n) => Ok((input, ExprLitInt{
                value: n,
                span: Span::new(digits),
            })),
            Err(e) => Err(Err::Error(ParseError::new(digits, ErrorKind::ParseIntError(e)))),
        }
    }
}


/**
 * Parser implementation for boolean literals.
 */
impl Parser for ExprLitBool {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        alt((
            map(tag("true"),  |s| ExprLitBool{value:true,  span: Span::new(s)}),
            map(tag("false"), |s| ExprLitBool{value:false, span: Span::new(s)}),
        ))(input)
    }
}
