
/***************************************************************************
 * Parser module is responsible for parsing source code and transforming
 * it into a data structure known as an Abstract Syntax Tree (AST).
 * The AST data structure is implemented in the AST module.
 ***************************************************************************/


/**
 * Require AST data structure
 */
use nom_locate::LocatedSpan;


/**
 * Type alias of LocatedSpan for convenience.
 */
pub type ParseSpan<'a> = LocatedSpan<&'a str>;


/**
 * Require error handling from nom library
 */
use nom::{
    error,
    Err,
};


/**
 * Error struct, defines the original span and optional local
 * span that is being parsed, with the error kind.
 */
#[derive(Debug)]
pub struct Error<'a>(ParseSpan<'a>, Option<ParseSpan<'a>>, ErrorKind);


/**
 * Defines the different kinds of errors to expect.
 */
#[derive(Debug)]
enum ErrorKind {
    ParseIntError(std::num::ParseIntError),
    Nom(error::ErrorKind),
    SyntaxError,
}


/**
 * Implement nom parse error functions for Error.
 */
impl<'a> error::ParseError<ParseSpan<'a>> for Error<'a> {
    fn from_error_kind(input: ParseSpan<'a>, kind: error::ErrorKind) -> Self {
        Error(input, None, ErrorKind::Nom(kind))
    }

    fn append(_: ParseSpan<'a>, _: error::ErrorKind, other: Self) -> Self {
        other
    }
}


/**
 * Type aliased IResult from std::Result.
 */
type IResult<'a, I, O, E = Error<'a>> = Result<(I, O), Err<E>>;


/**
 * Parser trait defines a generic parser that should
 * be implemented by each structure of the AST.
 */
pub trait Parser: Sized {
    fn parse<'a>(input: ParseSpan) -> IResult<ParseSpan, Self>;
}


pub mod atom;
