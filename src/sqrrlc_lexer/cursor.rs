
/***************************************************************************
 * The cursor helps with tokenizign the input source code.
 ***************************************************************************/


use std::str::Chars;
use std::iter::Peekable;


pub const EOF_CHAR: char = '\0';


/**
 * Cursor struct includes the source code with
 * an incrementing cursor for helping the lexer.
 */
pub struct Cursor<'a> {
    chars: Peekable<Chars<'a>>,
    prev: char,
    base_len: usize,
    len: usize,
}


impl<'a> Cursor<'a> {
    /**
     * Creates a new cursor with the given source code.
     */
    pub fn new(input: &'a str) -> Self {
        Cursor {
            chars: input.chars().peekable(),
            prev: EOF_CHAR,
            base_len: input.len(),
            len: 0,
        }
    }


    /**
     * Consumes the current character and returns it.
     * Returns EOF_CHAR if there are no more characters.
     */
    pub fn eat(&mut self) -> char {
        self.len += 1;
        self.prev = self.chars.next().unwrap_or(EOF_CHAR);
        self.prev
    }


    /**
     * Consumes charaters until the predicate is false.
     * The number of eaten characters is returned.
     */
    pub fn eat_while<P>(&mut self, mut predicate: P) -> usize
    where
        P: FnMut(char) -> bool
    {
        let mut eaten: usize = 0;
        while predicate(self.peek()) && self.is_hungry() {
        eaten += 1;
            self.consume();
        }
        
        eaten
    }


    /**
     * Peek at the next character without consuming it.
     */
    pub fn peek(&mut self) -> char {
        *self.chars.peek().unwrap_or(&EOF_CHAR)
    }


    /**
     * Consume the next character, returns true
     * if successful, false otherwise.
     */
    pub fn consume(&mut self) -> bool {
        if self.is_hungry() {
            self.len += 1;
            self.prev = self.chars.next().unwrap_or(EOF_CHAR);
            true
        } else {
            false
        }
    }
    

    /**
     * Resets the counters and returns the number of characters eaten.
     */
    pub fn next(&mut self) -> usize {
        let eaten = self.len;
        self.base_len = self.base_len - self.len;
        self.len = 0;
        eaten
    }

    
    /**
     * Returns true there more characters to eat, false otherwise.
     */
    pub fn is_eof(&self) -> bool {
        
    }
}
