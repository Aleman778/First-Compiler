
/***************************************************************************
 * The cursor helps with tokenizign the input source code.
 ***************************************************************************/


use std::str::Chars;


pub const EOF_CHAR: char = '\0';


/**
 * The cursor helps the lexical analysis by
 * providing simple consuming and peeking of characters.
 */
pub struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>,
    #[cfg(debug_assertions)]
    prev: char,
}


impl<'a> Cursor<'a> {
    /**
     * Creates a new cursor with the given source code.
     */
    pub fn new(input: &'a str) -> Self {
        Cursor {
            initial_len: input.len(),
            chars: input.chars(),
            #[cfg(debug_assertions)]
            prev: EOF_CHAR,
        }
    }


    /**
     * Returns the previously consumed character.
     * Note: can only be used inside debug_assert!().
     */
    pub fn prev(&self) -> char {
        #[cfg(debug_assertions)]
        {
            self.prev
        }
        #[cfg(not(debug_assertions))]
        {
            '\0'
        }
    }


    /**
     * Consumes the current character and returns it.
     * Returns EOF_CHAR if there are no more characters.
     */
    pub fn eat(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }
        return Some(c);
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
        while predicate(self.first()) && !self.is_eof() {
            eaten += 1;
            self.eat();
        }
        eaten
    }


    /**
     * Peek at the first character without consuming it.
     */
    pub fn first(&mut self) -> char {
        self.chars().nth(0).unwrap_or(EOF_CHAR)
    }


    /**
     * Peek at the second character without consuming it.
     */
    pub fn second(&mut self) -> char {
        self.chars().nth(1).unwrap_or(EOF_CHAR)
    }    


    /**
     * Returns the number of characters consumed.
     */
    pub fn len_consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }

    /**
     * Returns a `Chars` iterator over remaining characters.
     */
    pub fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }
    
    /**
     * Returns true there more characters to eat, false otherwise.
     */
    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }
}
