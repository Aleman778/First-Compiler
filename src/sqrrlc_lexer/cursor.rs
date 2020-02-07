
/***************************************************************************
 * The cursor helps with tokenizign the input source code.
 ***************************************************************************/


pub const EOF_CHAR: char = '\0';


/**
 * Cursor struct includes the source code with
 * an incrementing cursor for helping the lexer.
 */
pub struct Cursor<'a> {
    input: &'a str,
    cursor: usize,
}


impl<'a> Cursor<'a> {
    /**
     * Creates a new cursor with the given source code.
     */
    pub fn new(input: &'a str) -> Self {
        Cursor {
            input,
            cursor: 0,
        }
    }


    /**
     * Consumes the current character and returns it.
     * Returns EOF_CHAR if there are no more characters.
     */
    pub fn eat(&mut self) -> char {
        if self.hungry() {
            self.cursor += 1;
            self.input[self.cursor - 1]
        } else {
            EOF_CHAR
        }
    }


    /**
     * Peek at the next character without consuming it.
     */
    pub fn peek(&self) -> char {
        if self.hungry() {
            self.input[self.cursor]
        } else {
            EOF_CHAR
        }
    }


    /**
     * Consume the next character, returns true
     * if successful, false otherwise.
     */
    pub fn consume(&mut self) -> bool {
        if self.hungry() {
            self.cursor += 1;
            true
        } else {
            false
        }
    }


    /**
     * Returns true there more characters to eat, false otherwise.
     */
    pub fn hungry(&self) -> bool {
        self.cursor < self.input.len()
    }
}
