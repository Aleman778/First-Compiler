#![allow(dead_code)]


/***************************************************************************
 * Atom parser implementation defines parsers for atoms and values.
 ***************************************************************************/


use nom::{
    character::complete::{alpha1, digit1, multispace0},
    character::{is_alphanumeric},
    bytes::complete::{tag, take_while1},
    combinator::{map, peek},
    sequence::{preceded, pair, tuple},
    branch::alt,
    error::context,
    Err,
};


use crate::ast::{
    span::Span,
    expr::Expr,
    atom::*,
};


use crate::parser::{
    error::{ParseError, ErrorKind},
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse either an integer or boolean literal.
 */
impl Parser for Atom {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "atom",
            alt((
                map(LitBool::parse, |literal| Atom::Bool(literal)),
                map(Ident::parse, |ident| Atom::Ident(ident)),
                map(LitInt::parse, |literal| Atom::Num(literal)),
            ))
        )(input)
    }
}


// impl Parser for Paren {
//     fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
//         map(tuple((
//             tag("("),
//             preceded(multispace0, Expr::parse),
//             tag(")")
//         )), |(_, expr, _)| Paren{expr: Box::new(expr), span: Span::new(input)}
//         )(input)
//     }
// }


/**
 * Parse an identifier e.g. test_id.
 */
impl Parser for Ident {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Ident> {
        context(
            "identifier",
            map(pair(
                peek(alt((alpha1, tag("_")))),
                take_while1(|c: char| is_alphanumeric(c as u8) || c == '_')
            ), |(_, s): (ParseSpan, ParseSpan)| Ident{to_string: s.fragment.to_string(), span: Span::new(s)})
        )(input)
    }    
}


/**
 * Parser implementation for 32 bit unsigned integer literals.
 */
impl Parser for LitInt {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        let (input, digits) = digit1(input)?;
        match digits.fragment.parse::<i32>() {
            Ok(n) => Ok((input, LitInt{
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
impl Parser for LitBool {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        alt((
            map(tag("true"),  |s| LitBool{value:true,  span: Span::new(s)}),
            map(tag("false"), |s| LitBool{value:false, span: Span::new(s)}),
        ))(input)
    }
}
