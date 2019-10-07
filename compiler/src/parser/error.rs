
/***************************************************************************
 * Error sub module for parser is used to capture more information
 * from the parser combinators and allow custom errors.
 ***************************************************************************/


use crate::ast::span::Span;
use crate::parser::ParseSpan;


/**
 * Parse error struct holds information about the error and location
 */
#[derive(Debug)]
pub struct ParseError<'a> {
    errors: Vec<Context<'a>>,
}


/**
 * Gives context to an error e.g. location, kind, file etc.
 */
#[derive(Debug)]
pub struct Context<'a> {
    file: &'a str,
    span: Span,
    kind: ErrorKind,
}


impl<'a> ParseError<'a> {
    pub fn new(input: ParseSpan<'a>, kind: ErrorKind) -> Self {
        ParseError{
            errors: vec![Context{
                file: "",
                span: Span::new(input),
                kind: kind,
            }],
        }
    }


    pub fn append(input: ParseSpan<'a>, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push(Context{
            file: "",
            span: Span::new(input),
            kind: kind,
        });
        other
    }
}


/**
 * Implementation of nom ParseErrors for my custom ParseError
 */
impl<'a> nom::error::ParseError<ParseSpan<'a>> for ParseError<'a> {
    fn from_error_kind(input: ParseSpan<'a>, kind: nom::error::ErrorKind) -> Self {
        ParseError{
            errors: vec![Context{
                file: "",
                span: Span::new(input),
                kind: ErrorKind::Nom(kind),
            }],
        }
    }


    fn append(input: ParseSpan<'a>, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push(Context{
            file: "",
            span: Span::new(input),
            kind: ErrorKind::Nom(kind),
        });
        other
    }


    fn from_char(input: ParseSpan<'a>, c: char) -> Self {
        ParseError{
            errors: vec![Context{
                file: "",
                span: Span::new(input),
                kind: ErrorKind::Char(c),
            }],
        }
    }


    fn add_context(input: ParseSpan<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push(Context{
            file: "",
            span: Span::new(input),
            kind: ErrorKind::Context(ctx),
        });
        other
    }
}


/**
 * Error kind enum defines different types of parse errors.
 */
#[derive(Debug)]
pub enum ErrorKind {
    /// Failed to parse an int e.g. overflow.
    ParseIntError(std::num::ParseIntError),
    /// Error kind given by various nom parsers.
    Nom(nom::error::ErrorKind),
    /// Indicates which character was expected by the char function
    Char(char),
    /// Static string added by the `context` function
    Context(&'static str),
}
