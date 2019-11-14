
/***************************************************************************
 * Styled Buffer is used to create diagnostic messages and code snippets.
 * This makes it possible to easilty put characters where ever in the text.
 * There is also possible to assign a style to each character.
 * Note: this code is based on the rust compiler (but somewhat simplified).
 ***************************************************************************/


use crate::sqrrlc::error::{
    snippet::{Style, StyledString},
};


/**
 * Styled buffer contains the text with the 
 */
pub struct StyledBuffer {
    text: Vec<Vec<char>>,
    styles: Vec<Vec<Style>>,
}


/**
 * Implementation of the styled buffer.
 */
impl StyledBuffer {
    /**
     * Creates a new empty styled buffer.
     */
    pub fn new() -> Self {
        StyledBuffer {
            text: vec![],
            styles: vec![],
        }
    }


    /**
     * Renders the output list of styled string for each line.
     */
    pub fn render(&self) -> Vec<Vec<StyledString>> {
        let mut output: Vec<Vec<StyledString>> = vec![];
        let mut current_line: Vec<StyledString> = vec![];
        
        for (line, line_styles) in self.text.iter().zip(&self.styles) {
            let mut current_text = String::new();
            let mut current_style = Style::NoStyle;
            
            for (&chr, &style) in line.iter().zip(line_styles) {
                if style != current_style {
                    if !current_text.is_empty() {
                        current_line.push(StyledString {
                            text: current_text,
                            style: current_style,
                        });
                    }
                    current_style = style;
                    current_text = String::new();
                }
                current_text.push(chr);
            }
            if !current_text.is_empty() {
                current_line.push(StyledString {
                    text: current_text,
                    style: current_style,
                });
            }

            output.push(current_line);
            current_line = vec![];
        }

        return output;
    }
    
    
    /**
     * Ensures that the styled buffer has enough capacity
     * to put characters at the specific line.
     */
    pub fn ensure_lines(&mut self, line: usize) {
        while line >= self.text.len() {
            self.text.push(vec![]);
            self.styles.push(vec![]);
        }
    }
    
    
    /**
     * Puts the specified character at the given location in the buffer.
     */
    pub fn putc(&mut self, chr: char, style: Style, line: usize, col: usize) {
        self.ensure_lines(line);
        if col < self.text[line].len() {
            self.text[line][col] = chr;
            self.styles[line][col] = style;
        } else {
            let idx = self.text[line].len();
            while idx < col {
                self.text[line].push(' ');
                self.styles[line].push(Style::NoStyle);
            }
            self.text[line].push(chr);
            self.styles[line].push(style);
        }
    }


    /**
     * Puts the specified string at the given location in the buffer.
     */
    pub fn puts(&mut self, string: &str, style: Style, line: usize, col: usize) {
        let mut idx = col;
        for chr in string.chars() {
            self.putc(chr, style, line, idx);
            idx += 1;
        }
    }


    /**
     * Prepends a string and style to a specific line.
     * The current content is pushed to the right to make room
     * for the prepended string.
     */
    pub fn prepend(&mut self, string: &str, style: Style, line: usize) {
        self.ensure_lines(line);
        let len = string.chars().count();
        for _ in 0..len {
            self.text[line].insert(0, ' ');
            self.styles[line].insert(0, Style::NoStyle);
        }
        self.puts(string, style, line, 0);
    }


    /**
     * Appends a string and style to a specific line.
     */
    pub fn append(&mut self, string: &str, style: Style, line: usize) {
        if line >= self.text.len() {
            self.puts(string, style, line, 0);
        } else {
            self.puts(string, style, line, self.text[line].len());
        }
    }
}
