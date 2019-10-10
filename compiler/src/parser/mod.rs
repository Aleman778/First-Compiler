
/***************************************************************************
 * Parser module is responsible for parsing source code and transforming
 * it into a data structure known as an Abstract Syntax Tree (AST).
 * The AST data structure is implemented in the AST module.
 ***************************************************************************/


use crate::parser::error::ParseError;
use nom_locate::LocatedSpan;
use nom::Err;

/**
 * Type alias of LocatedSpan for convenience.
 */
pub type ParseSpan<'a> = LocatedSpan<&'a str>;


/**
 * Type aliased IResult from std::Result.
 */
type IResult<'a, I, O, E = ParseError<'a>> = Result<(I, O), Err<E>>;


/**
 * Parser trait defines a generic parser that should
 * be implemented by each structure of the AST.
 */
pub trait Parser: Sized {
    fn parse<'a>(input: ParseSpan) -> IResult<ParseSpan, Self>;
}


pub mod error;
pub mod token;
pub mod expr;
