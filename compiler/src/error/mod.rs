
/***************************************************************************
 * The error module of the compiler defines helper functions for
 * formatting errors into rust-like error messages.
 ***************************************************************************/


use crate::ast::span::Span;

/*
/**
 * ErrorFormatter is used for formatting any
 * errors into rust-like error messages.
 */
struct ErrorFormatter<'a> {
    severity: &'a str,
    description: &'a str,
    formatters: Vec<CodeFormatter<'a>>,    
}


struct CodeFormatter<'a> {
    span: &'a[&'a Span],
    marker: &'a str,
    
}


/**
 * Implementation of ErrorFormatter.
 */
impl ErrorFormatter {
    /**
     * Constructs am empty error formatter.
     */
    pub fn new() -> Self {
        ErrorFormatter {
            severity: "error",
            description: "",
            formatters: Vec::new(),
        }
    }

    
    /**
     * 
     */
    fn format(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("{}: {}", description));
        for f in formatters {
            result = f.format(result)
        }
    }
}


// impl CodeFormatter {
    // fn format(&self, 
// }
*/

/**
 * Converts error information into 
 */
pub fn convert_error(description: &str, span: &Span, source: &str, _explanation: &str) -> String {
    let mut result = String::new();
    result.push_str(format!("error: {}\n", description).as_str());
    if !span.is_empty() {
        if !span.file.len() > 0 {
            let spacing = format!("{}", span.end.line).len();
            result.push_str(format!("{}--> {}\n", " ".repeat(spacing).as_str(), span.location()).as_str());
            if source.len() > 0 {
                // let fragment = span.fragment(source);
                let split = source.split("\n");
                let lines: Vec<&str> = split.collect();
                result.push_str(display_line(spacing, span.start.line, "").as_str());
                for line in lines {
                    result.push_str(display_line(spacing, span.start.line, line).as_str());
                }
                result.push_str(display_line(spacing, span.start.line, "").as_str());
            }
        }
    }

    result
}


fn display_line(spacing: usize, line: u32, code: &str) -> String {
    let mut result = String::new();
    if code.len() < 1 {
        result.push_str(" ".repeat(spacing).as_str());
        result.push_str(format!(" |\n").as_str())
    } else {
        result.push_str(format!("{} |    {}\n", line, code).as_str())
        // for i in 1..(fragment.len() + 1) {
        // if i >= span.start.column && i < span.end.column {
            // result.push('^');
        // } else {
            // result.push(' ');
        // }
    }
    result
}



// fn code() {
    // result.push_str("\n");
    // for _ in 0..line_number.len() {
        // result.push(' ');
    // }
    // result.push_str(" |\n");
    // result.push_str(line_number.as_str());
    // result.push_str(" |    ");
    // result.push_str(fragment[(span.start.line - 1) as usize]);
    // result.push('\n');
    // for _ in 0..line_number.len() {
        // result.push(' ');
    // }
    // result.push_str(" |    ");
    // for i in 1..(fragment.len() + 1) {
        // if i >= span.start.column && i < span.end.column {
            // result.push('^');
        // } else {
            // result.push(' ');
        // }
    // }
    // result.push('\n');    
// }


fn error_code_multiline() {
    
}

