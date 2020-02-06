
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
        let char = 
    }


    pub fn peek() -> char {

    }


    pub fn consume() -> bool {
        
    }
}
