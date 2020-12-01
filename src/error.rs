use crate::ast::Span;
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub enum ErrorLevel {
    Fatal,
    Error,
    Warn,
    Info,
    Note,
    Help,
    Cancelled
}

pub struct ErrorMsg {
    pub level: ErrorLevel,
    pub line_number: u32,
    pub column_number: u32,
    pub path: String,
    pub msg: String,
    pub source: String,
    pub label: String,
    pub next: Option<Box<ErrorMsg>>
}

impl ErrorMsg {
    pub fn from_span(
        level: ErrorLevel,
        lines: &Vec<u32>,
        span: Span,
        filename: &str,
        source: &str,
        msg: &str,
        label: &str,
    ) -> Self {

        let line_number = match lines.binary_search(&span.base) {
            Ok(line) => line,
            Err(line) => line,
        };
        let line_start = lines[line_number];
        let line_end = lines[line_number];
        let column_number = span.base - line_start;

        ErrorMsg {
            level: level,
            line_number: line_number as u32,
            column_number: column_number as u32,
            path: filename.to_string(),
            msg: msg.to_string(),
            source: source[line_start as usize..line_end as usize].to_string(),
            label: label.to_string(),
            next: None,
        }
    }

    pub fn print(&self) -> std::io::Result<()> {
        let mut stderr = StandardStream::stderr(ColorChoice::Always);
        let mut color = ColorSpec::new();

        // Print Location
        writeln!(&mut stderr, "{}:{}:{}: ", self.path, self.line_number, self.column_number)?;

        // Print error level
        color.set_intense(true);
        match self.level {
            ErrorLevel::Fatal => {
                color.set_fg(Some(Color::White));
                color.set_bg(Some(Color::Red));
                stderr.set_color(&color)?;
                writeln!(&mut stderr, "fatal:")?;
            }
            ErrorLevel::Error => {
                color.set_bg(Some(Color::Red));
                stderr.set_color(&color)?;
                writeln!(&mut stderr, "error:")?;
            }
            ErrorLevel::Warn => {
                color.set_bg(Some(Color::Red));
                stderr.set_color(&color)?;
                writeln!(&mut stderr, "error:")?;
            }   
            ErrorLevel::Info => {
                color.set_bg(Some(Color::Blue));
                stderr.set_color(&color)?;
                writeln!(&mut stderr, "info:")?;
            }
            ErrorLevel::Note => {
                color.set_bg(Some(Color::Blue));
                stderr.set_color(&color)?;
                writeln!(&mut stderr, "note:")?;
            }
            ErrorLevel::Help => {
                color.set_bg(Some(Color::Blue));
                stderr.set_color(&color)?;
                writeln!(&mut stderr, "help:")?;
            }
            ErrorLevel::Cancelled => return Ok(())
        }
        color.clear();
        stderr.set_color(&color)?;
        writeln!(&mut stderr, " ")?;

        color.set_intense(true);
        stderr.set_color(&color)?;
        writeln!(&mut stderr, "{}", self.msg)?;
            
        color.clear();
        stderr.set_color(&color)?;

        let d = (((self.line_number as f32).log10()).floor() as u32) + 1;
        for _i in 0..=d {
            writeln!(&mut stderr, " ")?;
        }
        
        color.set_bg(Some(Color::Blue));
        stderr.set_color(&color)?;
        write!(&mut stderr, "|")?;
        
        color.clear();
        stderr.set_color(&color)?;
        write!(&mut stderr, "\n")?;

        write!(&mut stderr, "{}", self.line_number)?;
        write!(&mut stderr, " ")?;
        
        color.set_bg(Some(Color::Blue));
        stderr.set_color(&color)?;
        write!(&mut stderr, "|")?;
        
        color.clear();
        stderr.set_color(&color)?;
        write!(&mut stderr, " ")?;
        write!(&mut stderr, "{}", self.source)?;
        if self.source.len() == 0 || self.source.as_bytes()[self.source.len() - 1] != b'\n' {
            write!(&mut stderr, "\n")?;
        }

        for _i in 0..=d {
            write!(&mut stderr, " ")?;
        }
        
        color.set_bg(Some(Color::Blue));
        stderr.set_color(&color)?;
        write!(&mut stderr, "|")?;
        
        color.clear();
        stderr.set_color(&color)?;
        for _i in 0..=self.column_number {
            write!(&mut stderr, " ")?;
        }
        
        color.set_bg(Some(Color::Red));
        stderr.set_color(&color)?;
        write!(&mut stderr, "^")?;
        for _i in 0..=self.label.len() {
            writeln!(&mut stderr, "~")?;
        }
        write!(&mut stderr, " ")?;
        write!(&mut stderr, "{}", self.label)?;
        
        color.clear();
        stderr.set_color(&color)?;
        write!(&mut stderr, "\n")?;
        Ok(())
    }
}
