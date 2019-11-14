#![allow(dead_code)]
#![allow(unused_variables)]

/***************************************************************************
 * The `emitter` module is used to convert diagnostic to string
 * Note: this code is based on the rust compiler (but simplified).
 ***************************************************************************/


use std::rc::Rc;
use ansi_term::Color;
use crate::sqrrlc::error::{
    styled_buffer::*,
    diagnostic::*,
    snippet::*,
};
use crate::sqrrlc::source_map::SourceMap;


/**
 * Rename ansi_term::Style to ColorSpec to avoid confusion with
 * crate::sqrrlc::error::snippet::Style
 */
type ColorSpec = ansi_term::Style;


/**
 * The emitter struct prints the diagnostic information
 * to the console.
 */
pub struct Emitter {
    /// The source map containing all the source files.
    pub sm: Rc<SourceMap>,

    /// Flag for if ANSI mode is supported for windows.
    pub ansi_support: bool,
}


/**
 * Implementation for emitter.
 */
impl Emitter {
    /**
     * Creates a new emitter with default settings.
     */
    pub fn new(source_map: Rc<SourceMap>) -> Self {
        // ANSI support has to be enabled on windows.
        let mut enable_ansi = true;
        if cfg!(windows) {
            let response = ansi_term::enable_ansi_support();
            println!("{:?}", response);
            enable_ansi = response.is_ok();
        }
        Emitter {
            sm: source_map,
            ansi_support: enable_ansi,
        }
    }
    

    /**
     * Emit diagnostic information to console.
     */
    pub fn emit_diagnostic(&self, d: &Diagnostic) {
        let max_linum_len = self.get_max_linum_len(&d.span, &d.children);
        self.emit_message(&d.level, &d.code, &d.message, &d.span, max_linum_len, false);
        for sub in &d.children {
            self.emit_message(&sub.level, &None, &sub.message, &sub.span, max_linum_len, true);
        }
    }


    /**
     * Emits a message based on the given parameters.
     */
    fn emit_message(
        &self,
        level: &Level,
        code: &Option<String>,
        message: &Vec<StyledString>,
        span: &MultiSpan,
        max_linum_len: usize,
        is_secondary: bool,
    ) {
        let buf = &mut StyledBuffer::new();
        buf.append(&level.to_string(), Style::Level(*level), 0);
        buf.append(": ", Style::NoStyle, 0);
        for s in message {
            buf.append(&s.text, s.style, 0);
        }


        
        
        self.draw_code_snippet(buf, span);

        self.write_buffer(level, buf);
    }


    /**
     * Draw the code snippet if there is a primary span that refers to a source file.
     */
    fn draw_code_snippet(
        &self,
        buf: &mut StyledBuffer,
        span: &MultiSpan,
    ) {
        
    }


    /**
     * Renders the buffered diagnostic message into data containing styled texts.
     * The styled text data is then written in the console using eprint/ eprintln.
     * TODO: this should be moved out to enum for controlling the destination...
     */
    fn write_buffer(&self, level: &Level, buf: &StyledBuffer) {
        let data = buf.render();
        for line in &data {
            for string in line {
                if self.ansi_support {
                    let spec = self.get_color_spec(level, &string.style);
                    eprint!("{}", spec.paint(&string.text));
                } else {
                    eprint!("{}", string.text);
                }
            }
            eprintln!("");
        }
    }

    
    /**
     * Returns the color spec based on the diagnostic level and style type.
     */
    fn get_color_spec(&self, level: &Level, style: &Style) -> ColorSpec {
        match style {
            Style::LineNumber => Color::Cyan.bold(),
            Style::Underline |
            Style::UnderlinePrimary => self.get_level_spec(level),
            Style::Level(lvl) => self.get_level_spec(&lvl),
            Style::HeaderMessage |
            Style::NoStyle => ColorSpec::new(),
        }
    }


    /**
     * Returns the color spec based on the diagnostic level.
     */
    fn get_level_spec(&self, level: &Level) -> ColorSpec {
        match level {
            Level::Note => ColorSpec::new(),
            Level::Warning => Color::Yellow.normal(),
            Level::Error => Color::Red.normal(),
            Level::Fatal => Color::Black.on(Color::Red),
        }
    }

    
    /**
     * Draws a oolumn separator followed by a whitespace.
     */
    fn draw_col_separator(buf: &mut StyledBuffer, line: usize, col: usize) {
        buf.puts("| ", Style::LineNumber, line, col);
    }


    /**
     * Draws a column separator.
     */
    fn draw_col_separator_no_space(buf: &mut StyledBuffer, line: usize, col: usize) {
        buf.putc('|', Style::LineNumber, line, col);
    }

    /**
     * Draws a note separator followed by a whitespace.
     */
    fn draw_note_separator(buf: &mut StyledBuffer, line: usize, col: usize) {
        buf.puts("= ", Style::LineNumber, line, col);
    }


    /**
     * Returns the maximum line number length from the specified multispan.
     */
    fn get_multispan_max_len(&self, span: &MultiSpan) -> usize {
        let mut max_len = 0;
        for span in &span.primary_spans {
            max_len = max_len.max(self.sm.lookup_linum(&span).to_string().len());
        }
        
        for span_label in &span.span_labels {
            max_len = max_len.max(self.sm.lookup_linum(&span_label.0).to_string().len());
        }
        max_len
    }
    

    /**
     * Returns the maximum line number length from the span and children
     */
    fn get_max_linum_len(&self, span: &MultiSpan, children: &Vec<SubDiagnostic>) -> usize {
        let mut max_len = self.get_multispan_max_len(span);
        for child in children {
            max_len = max_len.max(self.get_multispan_max_len(&child.span));
        }
        max_len
    }
}


pub struct Margin {
    pub left_col: usize,

}
