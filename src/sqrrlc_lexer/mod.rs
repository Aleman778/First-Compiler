
/***************************************************************************
 * The lexer stage of the compiler converts input source code and 
 * converts it into tokens.
 ***************************************************************************/

mod cursor;
pub mod tokens;


use std::iter::Iterator;


pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    std::iter::from_fn(move || => {
        if input.is_empty() {
            return None;
        }
        let token = next_token();
    });
}





pub fn next_token(cursor: &mut Cursor) -> Token {
    
}



/**
 * Check if the given character is a whitespace character.
 */
pub fn is_whitespace(c: char) -> bool {
    

}
