
/***************************************************************************
 * Parser module is responsible for parsing source code and transforming
 * it into a data structure known as an Abstract Syntax Tree (AST).
 * The AST data structure is implemented in the AST module.
 ***************************************************************************/


use crate::sqrrlc_parser::error::ParseError;
use nom_locate::LocatedSpanEx;
use nom::Err;


/**
 * Type alias of LocatedSpanEx for convenience.
 * First string is the input string, other string is
 * the filename that is being parsed.
 */
pub type ParseSpan<'a> = LocatedSpanEx<&'a str, usize>;


/**
 * Type aliased IResult from std::Result.
 */
type IResult<I, O, E = ParseError> = Result<(I, O), Err<E>>;


/**
 * Parser trait defines a generic parser that should
 * be implemented by each structure of the AST.
 */
pub trait Parser: Sized {
    fn parse<'a>(input: ParseSpan) -> IResult<ParseSpan, Self>;
}


pub mod error;
pub mod base;
pub mod op;
pub mod lit;
pub mod expr;
pub mod ty;
