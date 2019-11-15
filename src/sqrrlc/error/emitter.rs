#![allow(dead_code)]
#![allow(unused_variables)]

/***************************************************************************
 * The `emitter` module is used to convert diagnostic to string
 * Note: this code is based on the rust compiler (but simplified).
 ***************************************************************************/


use std::rc::Rc;
use std::cmp::{min, max};
use ansi_term::Color;
use crate::sqrrlc::error::{
    styled_buffer::*,
    diagnostic::*,
    snippet::*,
};
use crate::sqrrlc::source_map::*;


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

    pub terminal_width: Option<usize>,
}


/**
 * Implementation for emitter.
 */
impl Emitter {
    /**
     * Creates a new emitter with default settings.
     */
    pub fn new(source_map: Rc<SourceMap>) -> Self {
        let mut enable_ansi = true;
        if cfg!(windows) {
            let response = ansi_term::enable_ansi_support();
            enable_ansi = response.is_ok();
        }
        Emitter {
            sm: source_map,
            ansi_support: enable_ansi,
            terminal_width: None,
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


        
        
        self.draw_code_snippet(buf, span, max_linum_len);

        self.write_buffer(level, buf);
    }


    /**
     * Draw the code snippet if there is a primary span that refers to a source file.
     */
    fn draw_code_snippet(
        &self,
        buf: &mut StyledBuffer,
        msp: &MultiSpan,
        max_linum_len: usize
    ) {
        let annotated_files = FileWithAnnotatedLines::collect_annotations(msp, &self.sm);
        let (primary_file, primary_span) = if let Some(span) = msp.primary_span() {
            (
                if let Some(file) = self.sm.get_file(span.loc) {
                    file
                } else {
                    return;
                },
                span
            )
        } else {
            return;
        };
        
        for annotated_file in &annotated_files {
            let file = &annotated_file.file;
            let is_primary = file.filename == primary_file.filename;
            self.draw_file_info(buf, annotated_file, max_linum_len, is_primary);
            let margin = self.calculate_margin(&annotated_file, max_linum_len);
            draw_col_separator_no_space(buf, buf.num_lines(), max_linum_len + 1);
                
            for line in &annotated_file.lines {
                self.draw_source_line(buf, file, &line, max_linum_len, &margin);
            }
        }
    }


    /**
     * Draws the pointer to the file, starting line number and starting column number.
     */
    fn draw_file_info(
        &self,
        buf: &mut StyledBuffer,
        annotated_file: &FileWithAnnotatedLines,
        max_linum_len: usize,
        is_primary: bool
    ) {
        let buf_line_offset = buf.num_lines();
        if is_primary {
            buf.append("--> ", Style::LineNumber, buf_line_offset);
        } else {
            buf.append("::: ", Style::LineNumber, buf_line_offset);
        }
        let loc = if let Some(first_line) = annotated_file.lines.first() {
            let col = if let Some(first_annotation) = first_line.annotations.first() {
                format!(":{}", first_annotation.start_col)
            } else {
                String::new()
            };
            format!("{}:{}{}", annotated_file.file.filename.display(), first_line.line_index, col)
        } else {
            format!("{}", annotated_file.file.filename.display())
        };
        buf.append(&loc, Style::LineAndColumn, buf_line_offset);
        buf.prepend(&" ".repeat(max_linum_len), Style::NoStyle, buf_line_offset);
    }


    /**
     * Draws one source line with any provided annotations, and stores the 
     * result in the provided stored buffer.
     */
    fn draw_source_line(
        &self,
        buf: &mut StyledBuffer,
        file: &SourceFile,
        line: &AnnotatedLine,
        max_linum_len: usize,
        margin: &Margin,
    ) {
        let mut buf_line_offset = buf.num_lines();
        
        buf.append(&line.line_index.to_string().as_str(), Style::LineNumber, buf_line_offset);

        draw_col_separator(buf, buf_line_offset, max_linum_len + 1);
        buf.append(&file.get_line(line.line_index as u32).as_str(), Style::NoStyle, buf_line_offset);

        buf_line_offset += 1;
        draw_col_separator(buf, buf_line_offset, max_linum_len + 1);
    }


    fn calculate_margin(
        &self,
        annotated_file: &FileWithAnnotatedLines,
        max_linum_len: usize
    ) -> Margin {
        let file = &annotated_file.file;
        
        // Get the left-side margin to remove it
        let mut whitespace_margin = std::usize::MAX;
        for line_idx in 0..annotated_file.lines.len() {
            let line = &annotated_file.lines[line_idx];
            let source_string = file.get_line(line.line_index as u32 - 1);
            let leading_whitespace = source_string
                .chars()
                .take_while(|c| c.is_whitespace())
                .count();
            if source_string.chars().any(|c| !c.is_whitespace()) {
                whitespace_margin = min(
                    whitespace_margin,
                    leading_whitespace,
                );
            }
        }
        if whitespace_margin == std::usize::MAX {
            whitespace_margin = 0;
        }

        // Left-most column any visible span points at.
        let mut span_left_margin = std::usize::MAX;
        for line in &annotated_file.lines {
            for ann in &line.annotations {
                span_left_margin = min(span_left_margin, ann.start_col);
                span_left_margin = min(span_left_margin, ann.end_col);
            }
        }
        if span_left_margin == std::usize::MAX {
            span_left_margin = 0;
        }

        // Right-most column any visible span points at.
        let mut span_right_margin = 0;
        let mut label_right_margin = 0;
        let mut max_line_len = 0;
        for line in &annotated_file.lines {
            max_line_len = max(
                max_line_len, file.get_line(line.line_index as u32 - 1).len());
            for ann in &line.annotations {
                span_right_margin = max(span_right_margin, ann.start_col);
                span_right_margin = max(span_right_margin, ann.end_col);
                // TODO: does not account for labels not in the same line
                let label_right = ann.label.as_ref().map_or(0, |l| l.len() + 1);
                label_right_margin = max(label_right_margin, ann.end_col + label_right);
            }
        }

        let width_offset = 3 + max_linum_len;
        let code_offset = if annotated_file.multiline_depth == 0 {
            width_offset
        } else {
            width_offset + annotated_file.multiline_depth + 1
        };

        let column_width = if let Some(width) = self.terminal_width {
            width.saturating_sub(code_offset)
        } else {
            140
        };

        Margin::new(
            whitespace_margin,
            span_left_margin,
            span_right_margin,
            label_right_margin,
            column_width,
            width_offset,
            code_offset,
            max_line_len,
        )
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
            Style::LineAndColumn |
            Style::NoStyle => ColorSpec::new(),
        }
    }


    /**
     * Returns the color spec based on the diagnostic level.
     */
    fn get_level_spec(&self, level: &Level) -> ColorSpec {
        match level {
            Level::Note => ColorSpec::new(),
            Level::Warning => Color::Yellow.bold(),
            Level::Error => Color::Red.bold(),
            Level::Fatal => Color::Black.on(Color::Red),
        }
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


/***************************************************************************
 * Helper struct, margin used for handling source code snippet rendering.
 ***************************************************************************/


/**
 * The margin defines values for different margins used for 
 * helping the rendering of code snippets.
 */
pub struct Margin {
    /// The available whitespace in the left taht can be consumed when centering.
    pub whitespace_left: usize,
    /// The column of the beginning of left-most span
    pub span_left: usize,
    /// The column of the end of right-most span
    pub span_right: usize,
    /// The beginning of the line to be displayd.
    pub computed_left: usize,
    /// The end of the line to be displayd.
    pub computed_right: usize,
    /// The current width of the terminal, where 140 is default.
    pub column_width: usize,
    /// The width of the of the left column plus the separator and padding of 1.
    pub width_offset: usize,
    /// The code offset defines at which offset the code actually starts, width_offset plus multiline depth.
    pub code_offset: usize,
    /// The end column of a span label, including the span
    pub label_right: usize,
}


/**
 * Implementation of margin struct.
 */
impl Margin {
    /**
     * Creates a new margin with the specified paramters.
     */
    fn new(
        whitespace_left: usize,
        span_left: usize,
        span_right: usize,
        label_right: usize,
        column_width: usize,
        width_offset: usize,
        code_offset: usize,
        max_line_len: usize,
    ) -> Self {
         // The 6 is padding to make room for eventual cutting `...`
        let mut m = Margin {
            whitespace_left: whitespace_left.saturating_sub(6),
            span_left: span_left.saturating_sub(6),
            span_right: span_right + 6,
            computed_left: 0,
            computed_right: 0,
            column_width,
            width_offset,
            code_offset,
            label_right: label_right + 6,
        };
        m.compute(max_line_len);
        m
    }


    /**
     * Returns true if the code was cut to the left, false otherwise.
     */
    fn was_cut_left(&self) -> bool {
        self.computed_left > 0
    }


    /**
     * Returns true if the code was cut to the right, false otherwise.
     */
    fn was_cut_right(&self, line_len: usize) -> bool {
        let right = if self.computed_right == self.span_right ||
            self.computed_right == self.label_right
        {
            self.computed_right - 6
        } else {
            self.computed_right
        };
        right < line_len && self.computed_left + self.column_width < line_len
    }


    /**
     * Compute the left and right margins for the code snippet.
     */
    fn compute(&mut self, max_line_len: usize) {
        // Trim the code when there is a lot of whitespace (>20) as it is useless.
        self.computed_left = if self.whitespace_left > 20 {
            self.whitespace_left - 16
        } else {
            0
        };

        self.computed_right = max(max_line_len, self.computed_left);

        if self.computed_right - self.computed_right > self.column_width {
            // If the code does not fit the terminal width, lets trim it then.
            if self.label_right - self.whitespace_left <= self.column_width {
                // Attempts to fit the code by only trimming whitespaces.
                self.computed_left = self.whitespace_left;
                self.computed_right = self.computed_left + self.column_width;
            } else if self.label_right - self.span_left <= self.column_width {
                // Attempts to fit the code by only considering spans and labels.
                let padding_left = (self.column_width - (self.label_right - self.span_left)) / 2;
                self.computed_left = self.span_left.saturating_sub(padding_left);
                self.computed_right = self.computed_left + self.column_width;
            } else if self.span_right - self.span_left <= self.column_width {
                // Attempts to fit the code by only considering spans plus padding.
                let padding_left = (self.column_width - (self.span_right - self.span_left)) / 5 * 2;
                self.computed_left = self.span_left.saturating_sub(padding_left);
                self.computed_right = self.computed_left + self.column_width;
            } else {
                // Give up but still don't show the full line.
                self.computed_left = self.span_left;
                self.computed_right = self.span_right;
            }
        }
    }


    /**
     * Returns the left most position to display the code.
     */
    fn left(&self, line_len: usize) -> usize {
        min(self.computed_left, line_len)
    }


    /**
     * Returns the right most position to display the code.
     */
    fn right(&self, line_len: usize) -> usize {
        if line_len.saturating_sub(self.computed_left) <= self.column_width {
            line_len
        } else {
            min(line_len, self.computed_right)
        }
    }
}
