
/***************************************************************************
 * Sqrrl compiler error sub module defines a general error trait 
 * that can be used by many parts of the compiler.
 ***************************************************************************/


use crate::sqrrlc_ast::span::Span;


/**
 * The error struct in sqrrlc has a span and the kind of error.
 */
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}


/**
 * The different kinds of errors that can occur.
 */
pub enum ErrorKind {
    OutOfScope(String, &'static [&'static str]),
    Context(&'static str),
}


/**
 * Implementation of ErrorKind.
 */
impl ErrorKind {
    /**
     * Converts ErrorKind into a text description.
     */
    pub fn description(&self) -> String {
        match self {
            ErrorKind::OutOfScope(ident, items)
                => format!("cannot find {:?} `{}` in this scope", items, ident),
            ErrorKind::Context(ctx)
                => format!("{}", ctx),
        }
    }
}


/**
 * Converts error information into 
 */
pub fn convert_error(description: &str, span: &Span, source: &str, explanation: &str) -> String {
    let mut result = String::new();
    result.push_str(format!("error: {}\n", description).as_str());
    if !span.is_empty() {
        if !span.file.len() > 0 {
            let spacing = format!("{}", span.end.line).len();
            result.push_str(format!("{}--> {}\n", " ".repeat(spacing).as_str(), span.location()).as_str());
            if source.len() > 0 {
                let split = source.split("\n");
                let fragments: Vec<&str> = split.collect();
                result.push_str(display_line(spacing, span.start.line, "").as_str());
                for line in span.start.line..(span.end.line + 1) {
                    result.push_str(display_line(spacing, line, fragments[(line - 1) as usize]).as_str());
                }
                result.push_str(mark_span(spacing, span, "^").as_str());
                if explanation.len() > 0 {
                    result.push_str(explanation)
                }
                result.push('\n');
            }
        }
    }
    result
}


/**
 * Display the code efter vertical line.
 */
fn display_line(spacing: usize, line: u32, code: &str) -> String {
    let mut result = String::new();
    if code.len() < 1 {
        result.push_str(" ".repeat(spacing).as_str());
        result.push_str(format!(" |\n").as_str())
    } else {
        result.push_str(format!("{} |    {}\n", line, code).as_str())
    }
    result
}


/**
 * Mark code using the span information after the vertical line.
 */
fn mark_span(spacing: usize, span: &Span, marker: &str) -> String {
    let mut result = " ".repeat(spacing);
    let before = " ".repeat(span.start.column - 1);
    let length = span.end.column - span.start.column;
    let underline = marker.repeat(length);
    result.push_str(format!(" |    {}{} ", before, underline).as_str());
    result
}
