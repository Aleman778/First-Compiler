//! The resulting token stream from lexing the tokens


use crate::lexer::{
    tokens::{Token, TokenKind, DUMMY_TOKEN},
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
    /// The input string slice.
    pub input: &'a str,
    
    /// The tokens currently lexed and not consumed.
    pub peeked: Vec<Option<Token>>,
    
    /// The consumed tokens position in the entire program.
    cur_pos: usize,

    /// The base position of parsed tokens in the entire program.
    base_pos: usize,
    
    #[cfg(debug_assertions)]
    /// The previous consumed token, used for debug asserts.
    pub prev: TokenKind,
}


impl<'a> TokenStream<'a> {
    /**
     * Creates a new token stream.
     */
    pub fn new(input: &'a str, base: usize) -> TokenStream {
        TokenStream { 
            input: input,
            peeked: Vec::new(), 
            cur_pos: base,
            base_pos: base,
            prev: TokenKind::Unknown,
        }
    }


    /**
     * Peek at the next token in the stream.
     */
    pub fn peek(&mut self) -> &Token {
        self.nth(0).unwrap_or(&DUMMY_TOKEN)
    }


    /**
     * Peek at the second next token in the stream.
     */
    pub fn peek2(&mut self) -> &Token {
        self.nth(1).unwrap_or(&DUMMY_TOKEN)
    }


    /**
     * Peek at the third next token in the stream.
     */
    pub fn peek3(&mut self) -> &Token {
        self.nth(2).unwrap_or(&DUMMY_TOKEN)
    }


    /**
     * Peek at the nth next token in the stream.
     */
    pub fn nth(&mut self, n: usize) -> Option<&Token> {
        if self.peeked.len() > n {
            return self.peeked[n].as_ref()
        }
        
        let len = n - self.peeked.len() + 1;
        for _i in 0..len {
            let token = self.eat();
            self.peeked.push(token)
        }
        self.peeked.last()?.as_ref()
    }


    /**
     * Removes all the peeked tokens.
     */
    pub fn consume(&mut self, n: usize) {
        for _i in 0..n {
            self.peeked.remove(0);
        }
    }


    /**
     * Get the current position of consumed
     * tokens i.e. excluding peeked tokens.
     * This position is aboslute for entire program.
     */
    pub fn cur_pos(&self) -> usize {
        self.cur_pos
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
        let token = if self.peeked.len() > 0 {
            self.peeked.remove(0)?
        } else {
            self.eat()?
        };
        #[cfg(debug_assertions)]
        {
            self.prev = token.kind;
        }
        self.cur_pos = token.base + token.len;
        Some(token)
    }
}
