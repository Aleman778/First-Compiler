#![allow(dead_code)]
#![allow(unused_variables)]

/***************************************************************************
 * The `emitter` module is used to convert diagnostic to string
 * Note: this code is based on the rust compiler (but simplified).
 ***************************************************************************/

extern crate unicode_width;

use std::rc::Rc;
use std::io;
use std::io::Write;
use std::cmp::{min, max, Reverse};
use std::collections::HashMap;
use termcolor::{StandardStream, ColorChoice, ColorSpec};
use termcolor::{WriteColor, Color};
use crate::sqrrlc::error::{
    styled_buffer::*,
    diagnostic::*,
    snippet::*,
};
use crate::sqrrlc::source_map::*;


/**
 * The emitter struct prints the diagnostic information
 * to the console.
 */
pub struct Emitter {
    /// The source map containing all the source files.
    pub sm: Rc<SourceMap>,

    /// The maximum width of the terminal.
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
        Emitter {
            sm: source_map,
            terminal_width: None,
        }
    }
    

    /**
     * Emit diagnostic information to console.
     */
    pub fn emit_diagnostic(&self, d: &Diagnostic) {
        if d.cancelled() {
            return;
        }        
        let max_linum_len = self.get_max_linum_len(&d.span, &d.children);
        self.emit_message(&d.level, &d.code, &d.message, &d.span, max_linum_len, false);
        for sub in &d.children {
            self.emit_message(
                &sub.level,
                &None,
                &sub.message,
                &sub.span,
                max_linum_len,
                true
            );
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
        match self.write_buffer(level, buf) {
            Ok(_) => {}
            Err(e) => eprintln!("{}", e)
        }
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
                
            let mut multilines = HashMap::new();
            for line_idx in 0..annotated_file.lines.len() {
                let prev_buf_line = buf.num_lines();

                // Draw the source line with annotations.
                let depths = self.draw_source_line(
                    buf,
                    file,
                    &annotated_file.lines[line_idx],
                    max_linum_len,
                    &margin
                );
                
                let mut to_add = HashMap::new();

                // Keep track of which multiline lines have been drawn and which to add.
                for (depth, style) in depths {
                    if multilines.get(&depth).is_some() {
                        multilines.remove(&depth);
                    } else {
                        to_add.insert(depth, style);
                    }
                }

                // Draw the multiline lines at specified depth and style.
                for (depth, style) in &multilines {
                    for line in prev_buf_line..buf.num_lines() {
                        draw_multiline_line(buf, line, margin.width_offset, *depth, *style);
                    }
                }

                // Check if lines that come between lines should be printed out or elided.
                if line_idx < (annotated_file.lines.len() - 1){
                    let line_idx_delta = annotated_file.lines[line_idx + 1].line_index -
                        annotated_file.lines[line_idx].line_index;
                    if line_idx_delta > 2 {
                        let last_buf_line_num = buf.num_lines();
                        buf.puts("...", Style::LineNumber, last_buf_line_num, 0);

                        for (depth, style) in &multilines {
                            draw_multiline_line(buf,
                                                last_buf_line_num,
                                                margin.width_offset,
                                                *depth,
                                                *style);
                        }
                    } else if line_idx_delta == 2 {
                        let unannotated_line = annotated_file.file
                            .get_line(annotated_file.lines[line_idx].line_index as u32);

                        let last_buf_line_num = buf.num_lines();

                        self.draw_line(
                            buf,
                            &unannotated_line,
                            annotated_file.lines[line_idx + 1].line_index - 1,
                            last_buf_line_num,
                            &margin,
                        );

                        for (depth, style) in &multilines {
                            draw_multiline_line(
                                buf,
                                last_buf_line_num,
                                margin.width_offset,
                                *depth,
                                *style,
                            );
                        }
                    }
                }
                multilines.extend(&to_add);
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
            format!("{}:{}{}", annotated_file.file.filename, first_line.line_index, col)
        } else {
            format!("{}", annotated_file.file.filename)
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
    ) -> Vec<(usize, Style)> {
        if line.line_index == 0 {
            return Vec::new();
        }

        let source_string = file.get_line(line.line_index as u32);
        let line_offset = buf.num_lines();
        let left = margin.left(source_string.len());
        let left = source_string.chars().take(left)
            .map(|ch| unicode_width::UnicodeWidthChar::width(ch).unwrap_or(1))
            .sum();

        self.draw_line(
            buf,
            &source_string,
            line.line_index,
            line_offset,
            margin
        );

        // Special case, if there is only one annotation then simplify output to:
        //
        // 2 | / fn foo() {
        // 3 | |
        // 4 | | }
        //   | |_^ span_label
        if let [ann] = &line.annotations[..] {
            if let AnnotationType::MultilineStart(depth) = ann.annotation_type {
                if source_string.chars().take(ann.start_col - 1).all(|c| c.is_whitespace()) {
                    let style = get_underline_style(ann.is_primary);
                    buf.putc('/', style, line_offset, margin.width_offset + depth - 1);
                    return vec![(depth, style)]
                }
            }
        }

        // We want to display span labels loke this:
        //
        //     vec.push(vec.pop().unwrap());
        //     ---      ^^^               - previous borrow ends here
        //     |        |
        //     |        error occurs here
        //     previous borrow of `vec` occurs here

        let mut annotations = line.annotations.clone();
        annotations.sort_by_key(|a| Reverse(a.start_col));
        let mut annotations_pos = vec![];
        let mut line_len = 0;
        let mut p = 0;
        for (i, ann) in annotations.iter().enumerate() {
            for (j, next) in annotations.iter().enumerate() {
                if overlaps(next, ann, 0) && j > i && p == 0 {
                    // If we overlap with an unlabelled annotation with same span just merge them.
                    if next.same_span(ann) && !next.has_label() {
                        continue;
                    }

                    // This annotation requires a new line in the output.
                    p += 1;
                    break;
                }
            }

            annotations_pos.push((p, ann));
            for (j, next) in annotations.iter().enumerate() {
                if j > i {
                    let l = next.label.as_ref().map_or(0, |label| label.len() + 2);
                    if (overlaps(next, ann, l)
                        && ann.has_label() && next.has_label())
                        || (ann.takes_space() && next.has_label())
                        || (ann.has_label()   && next.takes_space())
                        || (ann.takes_space() && next.takes_space())
                        || (overlaps(next, ann, l)
                            && next.end_col <= ann.end_col
                            && next.has_label() && p == 0)
                    {
                        // This annotation requires a new line in the output.
                        p += 1;
                        break;
                    }
                }
            }
            line_len = max(line_len, p);
        }


        if line_len != 0 {
            line_len += 1;
        }

        if line.annotations.iter().all(|a| a.is_line()) {
            return vec![];
        }


        for pos in 0..=line_len {
            draw_col_separator(buf, line_offset + pos + 1, margin.width_offset - 2);
            // buf.putc('|', Style::LineNumber, line_offset + pos + 1, marign.width_offset - 2); // FIXME: What why do this twice?
        }


        // Draw the horizontal lines for multiline annotations
        for &(pos, ann) in &annotations_pos {
            let style = get_underline_style(ann.is_primary);
            let pos = pos + 1;
            match ann.annotation_type {
                AnnotationType::MultilineStart(depth) |
                AnnotationType::MultilineEnd(depth) => {
                    draw_range(
                        buf,
                        '_',
                        style,
                        line_offset + pos,
                        margin.width_offset + depth,
                        margin.code_offset + ann.start_col - left,
                    )
                },
                _ => { },
            }
        }


        // Draw the vertical lines for labels that are on different levels
        for &(pos, ann) in &annotations_pos {
            let style = get_underline_style(ann.is_primary);
            let pos = pos + 1;

            if pos > 1 && (ann.has_label() || ann.takes_space()) {
                for p in line_offset + 1..=line_offset + pos {
                    buf.putc('|', style, p, margin.code_offset + ann.start_col - 1 - margin.computed_left);
                }
            }
            match ann.annotation_type {
                AnnotationType::MultilineStart(depth) => {
                    for p in line_offset + pos + 1..line_offset + line_len + 2 {
                        buf.putc('|', style, p, margin.width_offset + depth - 1);
                    }
                },
                AnnotationType::MultilineEnd(depth) => {
                    for p in line_offset..=line_offset + pos {
                        buf.putc('|', style, p, margin.width_offset + depth - 1);
                    }
                },
                _ => { },
            }
        }


        // Draw the labels on annotations that actually have a label
        for &(pos, ann) in &annotations_pos {
            let style = get_label_style(ann.is_primary);
            let (pos, col) = if pos == 0 {
                (pos + 1, ann.end_col.saturating_sub(left))
            } else {
                (pos + 2, (ann.start_col - 1).saturating_sub(left))
            };
            if let Some(ref label) = ann.label {
                buf.puts(&label, style, line_offset + pos, margin.code_offset + col);
            }
        }


        //Sort from biggest span to smallest span so smaller spans are represented in the output
        annotations_pos.sort_by_key(|(_, ann)| {
            (Reverse(ann.len()), ann.is_primary)
        });
        

        // Draw the underlines
        for &(_, ann) in &annotations_pos {
            let (symbol, style) = if ann.is_primary {
                ('^', Style::UnderlinePrimary)
            } else {
                ('-', Style::UnderlineSecondary)
            };
            
            match ann.annotation_type {
                AnnotationType::MultilineLine(_) => { },
                _ => {
                    for p in ann.start_col - 1..ann.end_col - 1 {
                        buf.putc(symbol, style, line_offset + 1, (margin.code_offset + p).saturating_sub(left));
                    }
                },
            }
        }

        // Collects multiline annotation location and styles for further processing.
        annotations_pos.iter().filter_map(|&(_, ann)| {
            match ann.annotation_type {
                AnnotationType::MultilineStart(p) |
                AnnotationType::MultilineEnd(p) => {
                    let style = get_underline_style(ann.is_primary);
                    Some((p, style))
                },
                _ => None
            }
        }).collect::<Vec<_>>()
    }


    /**
     * Draws the provided source string including the line number and separator.
     * The result is stored in the provided buffer
     */
    fn draw_line(
        &self,
        buf: &mut StyledBuffer,
        source_string: &str,
        line_index: usize,
        line_offset: usize,
        margin: &Margin
    ) {
        let line_len = source_string.len();
        let left = margin.left(line_len);
        let right = margin.right(line_len);
        
        // For long lines, we strip the soruce line, accounting for unicode.
        let mut taken = 0;
        let code: String = source_string.chars().skip(left).take_while(|ch| {
            let next = unicode_width::UnicodeWidthChar::width(*ch).unwrap_or(1);
            if taken + next > right - left {
                return false;
            }
            taken += next;
            true
        }).collect();

        buf.puts(&code, Style::Quotation, line_offset, margin.code_offset);
        if margin.was_cut_left() {
            buf.puts("...", Style::LineNumber, line_offset, margin.code_offset);
        }
        if margin.was_cut_right(line_len) {
            buf.puts("...", Style::LineNumber, line_offset, margin.code_offset + taken - 3);
        }
        buf.puts(&line_index.to_string(), Style::LineNumber, line_offset, 0);
        draw_col_separator(buf, line_offset, margin.width_offset - 2);
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
            let source_string = file.get_line(line.line_index as u32);
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
            max_line_len = max(max_line_len, file.get_line(line.line_index as u32).len());
            for ann in &line.annotations {
                span_right_margin = max(span_right_margin, ann.start_col);
                span_right_margin = max(span_right_margin, ann.end_col);
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
    fn write_buffer(&self, level: &Level, buf: &StyledBuffer) -> io::Result<usize> {
        let mut dest = StandardStream::stderr(ColorChoice::AlwaysAnsi);
        let data = buf.render();
        for line in &data {
            for string in line {
                let spec = self.get_color_spec(level, &string.style);
                dest.set_color(&spec)?;
                dest.write(string.text.as_bytes())?;
            }
            dest.write(&['\n' as u8])?;
        }
        dest.set_color(&ColorSpec::new())?;
        dest.write(&['\n' as u8])?;
        Ok(0)
    }

    
    /**
     * Returns the color spec based on the diagnostic level and style type.
     */
    fn get_color_spec(&self, level: &Level, style: &Style) -> ColorSpec {
        let mut spec = ColorSpec::new();
        match style {
            Style::LineNumber => {
                spec.set_bold(true);
                spec.set_fg(Some(Color::Blue));
            }
            Style::UnderlinePrimary |
            Style::LabelPrimary => {
                if let Level::Fatal = level {
                    spec.set_fg(Some(Color::Red));
                    spec.set_bold(true);
                } else {
                    spec = self.get_level_spec(level);
                }
            }
            Style::UnderlineSecondary |
            Style::LabelSecondary => {
                spec.set_bold(true);
                spec.set_fg(Some(Color::Blue));
            }
            Style::Level(lvl) => {
                spec = self.get_level_spec(&lvl);
            },
            Style::MainHeaderMsg => {
                spec.set_bold(true);
            }
            Style::HeaderMessage |
            Style::LineAndColumn |
            Style::Quotation |
            Style::NoStyle => {}
        };
        spec
    }


    /**
     * Returns the color spec based on the diagnostic level.
     */
    fn get_level_spec(&self, level: &Level) -> ColorSpec {
        let mut spec = ColorSpec::new();
        match level {
            Level::Fatal => {
                spec.set_bg(Some(Color::Red))
                    .set_fg(Some(Color::Black))
                    .set_bold(true);
            }
            Level::Error => {
                spec.set_fg(Some(Color::Red))
                    .set_bold(true);
            }
            Level::Warning => {
                spec.set_fg(Some(Color::Yellow))
                    .set_bold(true);
            }
            Level::Note => {
                spec.set_fg(Some(Color::Green))
                    .set_bold(true);
            }
            Level::Cancelled => unreachable!(),
        }
        spec
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


/***************************************************************************
 * Simple draw helper commands.
 ***************************************************************************/


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
 * Draws multiline line separators with specific offset and depth value.
 */
fn draw_multiline_line(buf: &mut StyledBuffer, line: usize, offset: usize, depth: usize, style: Style) {
    buf.putc('|', style, line, offset + depth - 1);
}


/**
 * Draws the provided symbol on a sepcified line with a specific column range.
 */
fn draw_range(buf: &mut StyledBuffer, symbol: char, style: Style, line: usize, col_from: usize, col_to: usize) {
    for col in col_from..col_to {
        buf.putc(symbol, style, line, col);
    }
}


/***************************************************************************
 * Helper struct, margin used for handling source code snippet rendering.
 ***************************************************************************/


/**
 * The margin defines values for different margins used for 
 * helping the rendering of code snippets.
 */
struct Margin {
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
