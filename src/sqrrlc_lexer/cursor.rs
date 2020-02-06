
/***************************************************************************
 * The cursor helps with tokenizign the input source code.
 ***************************************************************************/


use std::str::chars;


pub const EOF: char = '\0';


struct Cursor<'a> {
    input: &str,
    ptr: usize,
}


impl<'a> Cursor<'a> {
    /**
     * Creates a new cursor with the given source code.
     */
    pub fn new(input: &str) {
        Cursor {
            input,
            ptr: 0,
        }
    }


    /**
     * Consumes the current character and returns it.
     * Returns EOF if there are no more characters.
     */
    pub fn eat() -> char {
        if ptr < input.len() {
            input[ptr++]
        } else {
            EOF
        }
    }


    pub fn peek() -> char {
        if ptr < input.len() {
            input[ptr++]
        } else {
            EOF
        }
    }


    pub fn consume() -> bool {
        if ptr < input.len() {
            ptr++;
            true
        } else {
            false
        }
    }
}
