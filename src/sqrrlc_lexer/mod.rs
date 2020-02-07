
/***************************************************************************
 * The lexer stage of the compiler converts input source code and 
 * converts it into tokens.
 ***************************************************************************/

mod cursor;
pub mod tokens;


use std::iter::Iterator;
use crate::sqrrlc_lexer::tokens::*;
use crate::sqrrlc_lexer::cursor::*;


/**
 * The tokenize method defines an iterator used to iterate
 * through tokens that are lexed based on the provided input string.
 */
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        if input.is_empty() {
            return None;
        }
        let token = next_token(&mut cursor);
        input = &input[token.len..];
        Some(token)
    })
}



pub fn next_token(cursor: &mut Cursor) -> Token {
    println!("{}", cursor.eat());
    Token::new(Token::Unknown, 0)
}



/**
 * Check if the given character is a whitespace character.
 */
pub fn is_whitespace(c: char) -> bool {
    

}
