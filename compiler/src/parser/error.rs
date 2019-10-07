
/***************************************************************************
 * Error sub module for parser is used to capture more information
 * from the parser combinators and allow custom errors.
 ***************************************************************************/


use crate::ast::span::Span;
use crate::parser::ParseSpan;


/**
 * Parse error struct holds information about the error and location
 */
pub struct ParseError<'a> {
    errors: Vec<Verbose<'a>>,
}


/**
 * Gives context to an error e.g. location, kind, file etc.
 */
pub struct Verbose<'a> {
    file: &'a str,
    span: Span,
    kind: ErrorKind,
}


pub fn convert_error<'a>(input: &ParseSpan<'a>, error: ParseError<'a>) -> String {
    let mut result = String::new();
    result.push_str("error: ");
    for err in &error.errors {
        match &err.kind {
            ErrorKind::ParseIntError(e) => result.push_str(&e.to_string()),
            ErrorKind::Nom(e) => result.push_str((*e).description()),
            ErrorKind::Char(e) => result.push(*e),
            ErrorKind::Context(e) => result.push_str(e),
        };
        result.push('\n');
        result.push_str("  --> ");
        result.push_str("src\\main.rs:");//err.file);
        result.push_str(err.span.start.to_string().as_str());

        result.push_str("\n   |");
        result.push_str(format!("\n{}", err.span.start.line).as_str());
        result.push_str("  |    ");
        result.push_str(input.fragment);
        result.push_str("\n   |    ");
        
        
        break;
    }
    result
}


impl<'a> std::fmt::Debug for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.errors {
            write!(f, "{:?}", err)?;
        }
        Ok(())
    }
}

impl<'a> std::fmt::Debug for Verbose<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::ParseIntError(e) => write!(f, "error: {}", e),
            ErrorKind::Nom(e) => write!(f, "error: {:?}", e),
            ErrorKind::Char(e) => write!(f, "error: {:?}", e),
            ErrorKind::Context(e) => write!(f, "error: {}", e),
        }
    }
}


impl<'a> ParseError<'a> {
    pub fn new(input: ParseSpan<'a>, kind: ErrorKind) -> Self {
        ParseError{
            errors: vec![Verbose{
                file: "",
                span: Span::new(input),
                kind: kind,
            }],
        }
    }


    pub fn append(input: ParseSpan<'a>, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push(Verbose{
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
            errors: vec![Verbose{
                file: "",
                span: Span::new(input),
                kind: ErrorKind::Nom(kind),
            }],
        }
    }


    fn append(input: ParseSpan<'a>, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push(Verbose{
            file: "",
            span: Span::new(input),
            kind: ErrorKind::Nom(kind),
        });
        other
    }


    fn from_char(input: ParseSpan<'a>, c: char) -> Self {
        ParseError{
            errors: vec![Verbose{
                file: "",
                span: Span::new(input),
                kind: ErrorKind::Char(c),
            }],
        }
    }


    fn or(self, other: Self) -> Self {
        if self.errors.len() > other.errors.len() {
            self
        } else {
            other
        }
    }


    fn add_context(input: ParseSpan<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push(Verbose{
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
