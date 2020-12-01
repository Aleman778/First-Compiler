#![allow(dead_code)]

use crate::ast::{Span, get_span_location_in_file};
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

pub fn create_error_msg_from_span(
    level: ErrorLevel,
    lines: &Vec<u32>,
    span: Span,
    filename: &str,
    source: &str,
    msg: &str,
    label: &str,
) -> ErrorMsg {
    let (line_number, column_number, line_start, line_end) = get_span_location_in_file(lines, span);
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

pub fn print_error_msg(msg: &ErrorMsg) {
    match print_error_mgs_fmt(msg) {
        _ => { }
    };
}

fn print_error_mgs_fmt(msg: &ErrorMsg) -> std::io::Result<()> {
    let mut stderr = StandardStream::stderr(ColorChoice::Always);
    let mut color = ColorSpec::new();

    // Print Location
    write!(&mut stderr, "{}:{}:{}: ", msg.path, msg.line_number, msg.column_number)?;

    // Print error level
    color.set_intense(true);
    match msg.level {
        ErrorLevel::Fatal => {
            color.set_fg(Some(Color::White));
            color.set_bg(Some(Color::Red));
            stderr.set_color(&color)?;
            write!(&mut stderr, "fatal:")?;
        }
        ErrorLevel::Error => {
            color.set_bg(Some(Color::Red));
            stderr.set_color(&color)?;
            write!(&mut stderr, "error:")?;
        }
        ErrorLevel::Warn => {
            color.set_bg(Some(Color::Red));
            stderr.set_color(&color)?;
            write!(&mut stderr, "error:")?;
        }
        ErrorLevel::Info => {
            color.set_bg(Some(Color::Blue));
            stderr.set_color(&color)?;
            write!(&mut stderr, "info:")?;
        }
        ErrorLevel::Note => {
            color.set_bg(Some(Color::Blue));
            stderr.set_color(&color)?;
            write!(&mut stderr, "note:")?;
        }
        ErrorLevel::Help => {
            color.set_bg(Some(Color::Blue));
            stderr.set_color(&color)?;
            write!(&mut stderr, "help:")?;
        }
        ErrorLevel::Cancelled => return Ok(())
    }
    color.clear();
    stderr.set_color(&color)?;
    write!(&mut stderr, " ")?;

    color.set_intense(true);
    stderr.set_color(&color)?;
    write!(&mut stderr, "{}\n", msg.msg)?;

    color.clear();
    stderr.set_color(&color)?;

    let d = (((msg.line_number as f32).log10()).floor() as u32) + 1;
    for _i in 0..=d {
        write!(&mut stderr, " ")?;
    }

    color.set_fg(Some(Color::Blue));
    stderr.set_color(&color)?;
    write!(&mut stderr, "|")?;

    color.clear();
    stderr.set_color(&color)?;
    write!(&mut stderr, "\n")?;

    write!(&mut stderr, "{}", msg.line_number)?;
    write!(&mut stderr, " ")?;

    color.set_fg(Some(Color::Blue));
    stderr.set_color(&color)?;
    write!(&mut stderr, "|")?;

    color.clear();
    stderr.set_color(&color)?;
    write!(&mut stderr, " ")?;
    write!(&mut stderr, "{}", msg.source)?;
    if msg.source.len() == 0 || msg.source.as_bytes()[msg.source.len() - 1] != b'\n' {
        write!(&mut stderr, "\n")?;
    }

    for _i in 0..=d {
        write!(&mut stderr, " ")?;
    }

    color.set_fg(Some(Color::Blue));
    stderr.set_color(&color)?;
    write!(&mut stderr, "|")?;

    color.clear();
    stderr.set_color(&color)?;
    for _i in 0..msg.column_number {
        write!(&mut stderr, " ")?;
    }

    color.set_fg(Some(Color::Red));
    stderr.set_color(&color)?;
    write!(&mut stderr, "^")?;
    for _i in 0..=msg.label.len() {
        write!(&mut stderr, "~")?;
    }
    write!(&mut stderr, " ")?;
    write!(&mut stderr, "{}", msg.label)?;

    color.clear();
    stderr.set_color(&color)?;
    write!(&mut stderr, "\n")?;
    Ok(())
}
