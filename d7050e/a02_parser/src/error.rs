
/***************************************************************************
 * Result definition and errors handling for parser
 ***************************************************************************/

use nom::{error, Err};
use crate::ast::Span;


/**
 * Error struct, defines the original span and optional local
 * span that is being parsed, with the error kind.
 */
#[derive(Debug)]
pub struct Error<'a>(pub Span<'a>, pub Option<Span<'a>>, pub ErrorKind);


/**
 * Type aliased IResult from std::Result.
 */
pub type IResult<'a, I, O, E = Error<'a>> = Result<(I, O), Err<E>>;


/**
 * Defines the different kinds of errors to expect.
 */
#[derive(Debug)]
pub enum ErrorKind {
    ParseIntError(std::num::ParseIntError),
    Nom(error::ErrorKind),
    SyntaxError,
}


/**
 * Implement nom parse error functions for Error.
 */
impl<'a> error::ParseError<Span<'a>> for Error<'a> {
    fn from_error_kind(input: Span<'a>, kind: error::ErrorKind) -> Self {
        Error(input, None, ErrorKind::Nom(kind))
    }

    fn append(_: Span<'a>, _: error::ErrorKind, other: Self) -> Self {
        other
    }
}
