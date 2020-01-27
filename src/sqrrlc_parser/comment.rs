
/***************************************************************************
 * Parsing comments, documentation comments nom combinatior.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::{take_until, take, tag},
    sequence::pair,
    combinator::map,
    multi::many0,
    branch::alt,
    error::context,
    Err,
};
use crate::sqrrlc_parser::{
    error::{ParseError, ErrorKind},
    ParseSpan,
    IResult,
};



/**
 * Parse both multispace1 and comments.
 */
pub fn multispace_comment0(input: ParseSpan) -> IResult<ParseSpan, ()> {
    map(
        many0(
            alt((
                map(multispace1, |_| ()),
                any_comment,
            ))
        ), |_| ()
    )(input)
}


/**
 * Parses any type of comment and removes it from the span.
 */
fn any_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    alt((
        doc_comment,
        block_doc_comment,
        comment,
        block_comment,
    ))(input)
}


/**
 * Parse a singleline comment.
 */
fn comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    context(
        "comment",
        map(pair(tag("//"), take_until("\n")), |_| ())
    )(input)
}


/**
 * Parse a block comment.
 */
fn block_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    let mut input = tag("/*")(input)?.0;
    loop {
        let next: IResult<ParseSpan, ParseSpan> = take(1usize)(input);
        match next {
            Ok((inpt, _)) => input = inpt,
            Err(_) => return Err(Err::Error(ParseError::new(
                input, ErrorKind::Context("unterminated block comment")))),
        };
        let new: IResult<ParseSpan, ParseSpan> = tag("/*")(input);
        let end: IResult<ParseSpan, ParseSpan> = tag("*/")(input);
        if new.is_ok() {
            input = block_comment(input)?.0;
        } else if end.is_ok() {
            return map(tag("*/"), |_| ())(input);
        }
    }
}


/**
 * Parse a singleline documentation comment.
 */
fn doc_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    context(
        "doc-comment",
        map(pair(tag("///"), take_until("\n")), |_| ())
    )(input)
}


/**
 * Parse a 
 */
fn block_doc_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    let mut input = tag("/**")(input)?.0;
    loop {
        let next: IResult<ParseSpan, ParseSpan> = take_until("\n")(input);
        match next {
            Ok((inpt, _)) => input = inpt,
            Err(_) => return Err(Err::Error(ParseError::new(
                input, ErrorKind::Context("unterminated block doc-comment")))),
        };
        input = multispace0(input)?.0;
        let middle: IResult<ParseSpan, ParseSpan> = tag("*")(input);
        let new: IResult<ParseSpan, ParseSpan> = tag("/**")(input);
        let end: IResult<ParseSpan, ParseSpan> = tag("*/")(input);
        if middle.is_ok() {
            input = middle?.0;
        } else if new.is_ok() {
            input = block_doc_comment(input)?.0;
        } else if end.is_ok() {
            return Ok((input, ()));
        }
    }
}

