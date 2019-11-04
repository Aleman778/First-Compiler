#![allow(dead_code)]

/***************************************************************************
 * Error sub module for parser is used to capture more information
 * from the parser combinators and allow custom errors.
 ***************************************************************************/


use crate::sqrrlc_ast::span::Span;
use crate::sqrrlc_parser::ParseSpan;


/**
 * Parse error struct holds information about the error and location
 */
pub struct ParseError {
    errors: Vec<Verbose>,
}


/**
 * Gives context to an error e.g. location, kind, file etc.
 */
pub struct Verbose {
    span: Span,
    kind: ErrorKind,
}


pub fn convert_error<'a>(input: &ParseSpan<'a>, error: ParseError) -> String {
    let mut result = String::new();
    let split = input.fragment.split("\n");
    let fragment: Vec<&str> = split.collect();

    for err in &error.errors {
    result.push_str("error: ");
        match &err.kind {
            ErrorKind::ParseIntError(e) => result.push_str(&e.to_string()),
            ErrorKind::Nom(e) => result.push_str((*e).description()),
            ErrorKind::Char(e) => result.push(*e),
            ErrorKind::Context(e) => {
                result.push_str("expected ");
                result.push_str(e);
                result.push_str(" got ");
                result.push_str(err.span.fragment(input.fragment).as_str())
            },
        };
        result.push('\n');
        
        let file = err.span.file.as_str();
        if file.len() > 0 {
            result.push_str("  --> ");
            result.push_str(file);
            result.push_str(":");
            result.push_str(err.span.start.to_string().as_str());
        }

        let line_number = format!("{}", err.span.start.line);
        
        result.push_str("\n");
        for _ in 0..line_number.len() {
            result.push(' ');
        }
        result.push_str(" |\n");
        result.push_str(line_number.as_str());
        result.push_str(" |    ");
        result.push_str(fragment[(err.span.start.line - 1) as usize]);
        result.push('\n');
        for _ in 0..line_number.len() {
            result.push(' ');
        }
        result.push_str(" |    ");
        for i in 1..(input.fragment.len() + 1) {
            if i >= err.span.start.column && i < err.span.end.column {
                result.push('^');
            } else {
                result.push(' ');
            }
        }
        result.push('\n')
    }
    result
}


impl<'a> std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.errors {
            write!(f, "{:?}", err)?;
        }
        Ok(())
    }
}

impl<'a> std::fmt::Debug for Verbose {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::ParseIntError(e) => write!(f, "error: {}", e),
            ErrorKind::Nom(e) => write!(f, "error: {:?}", e),
            ErrorKind::Char(e) => write!(f, "error: {:?}", e),
            ErrorKind::Context(e) => write!(f, "error: {}", e),
        }
    }
}


impl<'a> ParseError {
    pub fn new(input: ParseSpan<'a>, kind: ErrorKind) -> Self {
        ParseError{
            errors: vec![Verbose{
                span: Span::new(input),
                kind: kind,
            }],
        }
    }


    pub fn append(input: ParseSpan<'a>, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push(Verbose {
            span: Span::new(input),
            kind: kind,
        });
        other
    }


    fn is_important(&self) -> bool {
        match self.errors.last() {
            Some(verbose) => {
                match verbose.kind {
                    ErrorKind::ParseIntError(_) => true,
                    _ => false,
                }
                
            },
            None => false,
        }
    }
}


/**
 * Implementation of nom ParseErrors for my custom ParseError
 */
impl<'a> nom::error::ParseError<ParseSpan<'a>> for ParseError {
    fn from_error_kind(input: ParseSpan<'a>, kind: nom::error::ErrorKind) -> Self {
        ParseError{
            errors: vec![Verbose{
                span: Span::new(input),
                kind: ErrorKind::Nom(kind),
            }],
        }
    }


    fn append(input: ParseSpan<'a>, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push(Verbose{
            span: Span::new(input),
            kind: ErrorKind::Nom(kind),
        });
        other
    }


    fn from_char(input: ParseSpan<'a>, c: char) -> Self {
        ParseError{
            errors: vec![Verbose{
                span: Span::new(input),
                kind: ErrorKind::Char(c),
            }],
        }
    }


    fn or(self, other: Self) -> Self {
        let sim = self.is_important();
        let oim = other.is_important();
        if sim && !oim {
            self
        } else if !sim && oim {
            other
        } else if self.errors.len() > other.errors.len() {
            self
        } else {
            other
        }
    }


    fn add_context(input: ParseSpan<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push(Verbose{
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
