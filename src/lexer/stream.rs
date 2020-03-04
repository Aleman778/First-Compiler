//! The resulting token stream from lexing the tokens


use crate::lexer::{
    tokens::{Token, DUMMY_TOKEN},
    cursor::Cursor,
    advance_token,
    is_whitespace,
};
use std::iter::Iterator;


/**
 * The token stream implements an iterator over a stream of tokens
 * that are lexed by the provided tokenizer. The token stream allows
 * for peeking any amount of tokens.
 */
pub struct TokenStream<'a> {
    input: &'a str,
    peeked: Vec<Option<Token>>,
    base_pos: usize,
}


impl<'a> TokenStream<'a> {
    /**
     * Creates a new token stream.
     */
    pub fn new(input: &'a str, base: usize) -> TokenStream {
        TokenStream { 
            input: input,
            peeked: Vec::new(), 
            base_pos: base,
        }
    }


    /**
     * Peek at the next token in the stream.
     */
    pub fn first(&mut self) -> &Token {
        self.nth(0).unwrap_or(&DUMMY_TOKEN)
    }


    /**
     * Peek at the second next token in the stream.
     */
    pub fn second(&mut self) -> &Token {
        self.nth(1).unwrap_or(&DUMMY_TOKEN)
    }


    /**
     * Peek at the third next token in the stream.
     */
    pub fn third(&mut self) -> &Token {
        self.nth(2).unwrap_or(&DUMMY_TOKEN)
    }

    /**
     * Peek at the nth next token in the stream.
     */
    pub fn nth(&mut self, n: usize) -> Option<&Token> {
        if let Some(token) = self.peeked.get(n) {
            token.map(|t| &t)
        } else {
            let len = n - self.peeked.len() + 1;
            for i in 0..len {
                self.peeked.push(self.eat())
            }
            self.peeked.last()?.map(|t| &t)
        }
    }


    /**
     * Removes all the peeked tokens.
     */
    pub fn consume(&mut self, n: usize) {
        for i in 0..n {
            self.peeked.remove(0);
        }
    }
    

    /**
     * Consumes the next token even if there are already peeked tokens.
     */
    fn eat(&mut self) -> Option<Token> {
        if self.input.is_empty() {
            return None;
        }

        let cur = &mut Cursor::new(&self.input); 
        if is_whitespace(cur.first()) {
            let len_consumed = cur.eat_while(|c| is_whitespace(c));
            self.base_pos += len_consumed;
            self.input = &self.input[len_consumed..];

            if self.input.is_empty() {
                return None;
            }
        }

        let token = advance_token(&mut Cursor::new(&self.input), self.base_pos);
        self.base_pos += token.len;
        self.input = &self.input[token.len..];
        Some(token)
    }
}


impl<'a> Iterator for TokenStream<'a> {
    /**
     * The item type to iterate over is the Token struct.
     */
    type Item = Token;

    /**
     * Parses the next token, returns None if it has reached the end of file.
     */
    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.len() > 0 {
            self.peeked.remove(0)
        } else {
            self.eat()
        }
    }
}
