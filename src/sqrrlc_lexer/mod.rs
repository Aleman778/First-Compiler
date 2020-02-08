
/***************************************************************************
 * The lexer stage of the compiler converts input source code and 
 * converts it into tokens.
 ***************************************************************************/

mod cursor;
pub mod tokens;


use std::iter::Iterator;
use crate::sqrrlc_lexer::tokens::*;
use crate::sqrrlc_lexer::cursor::*;

use TokenKind::*;
use LiteralKind::*;


/**
 * The tokenize method defines an iterator used to iterate
 * through tokens that are lexed based on the provided input string.
 */
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        if !cursor.is_hungry() {
            return None;
        }
        Some(cursor.next_token())
    })
}



impl Cursor<'_> {
    /**
     * Parses the next token from input string.
     */
    pub fn next_token(&mut self) -> Token {
        let first_char = self.eat().unwrap();
        let token_kind = match first_char {
            // Matching against beginning slash.
            '/' => match self.peek() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => TokenKind::Slash,
            },
            
            // Match against whitespaces.
            c if is_whitespace(c) => self.whitespace(),

            // Matching against number literals
            '0'..='9' => self.numeric_literal(),

            'b' => if self.peek() == '\'' {
            },

            // Matching against raw identifiers
            'r' => if self.peek() == '#' {
                self.raw_identifier();
            },

            // Matching against identifiers and keywords.
            c if is_ident_start(c) => self.identifier(),

            // Matching against single character tokens.
            ';' => Semi,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBrace,
            ']' => CloseBrace,
            '@' => At,
            '#' => Pound,
            '~' => Tilde,
            '?' => Question,
            ':' => Colon,
            '$' => Dolar,
            '=' => Eq,
            '!' => Not,
            '<' => Lt,
            '>' => Gt,
            '+' => Plus,
            '-' => Minus,
            '&' => And,
            '|' => Or,
            '*' => Star,
            '/' => Slash,
            '^' => Caret,
            '%' => Percent,
            
            // No matched characters, unknown token type.
            _ => Unknown,
        };
        Token::new(token_kind, self.next())
    }


    /**
     * Parses line comment token.
     */
    fn line_comment(&mut self) -> TokenKind {
        self.consume();
        self.eat_while(|c| c != '\n');
        LineComment
    }


    /**
     * Parses block comment token. e.g.
     * /* ... */ or nested /* ... /* ... */ */ 
     */
    fn block_comment(&mut self) -> TokenKind {
        self.consume();
        let mut depth: usize = 1;
        while let Some(c) = self.eat() {
            match c {
                '/' => if self.peek() == '*' {
                    self.consume();
                    depth += 1;
                },
                '*' => if self.peek() == '/' {
                    self.consume();
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => (),
            }
        }
        BlockComment { terminated: depth == 0 }
    }


    /**
     * Parses whitespace token.
     */
    fn whitespace(&mut self) -> TokenKind {
        self.eat_while(|c| is_whitespace(c));
        Whitespace
    }
 

    /**
     * Parsers numberical literal token.
     */
    fn numeric_literal(&mut self) -> LitKind{
        let radix = Radix::Decimal;
        let first_char = self.prev;
        if first_char == '0' {
            let has_digits = match self.peek() {
                // Binary prefix
                'b' => {
                    radix = Radix::Binary;
                    self.consume();
                    self.eat_decimal_numbers()
                }
                // Octal prefix
                'o' => {
                    radix = Radix::Binary;
                    self.consume();
                    self.eat_decimal_numbers()
                }
                // Hexadecimal prefix
                'x' => {
                    radix = Radix::Binary;
                    self.consume();
                    self.eat_decimal_numbers()
                }
                // No radix prefix
                '0'..='9' {
                    self.eat_decimal_numbers();
                    true
                }
                // Just a 0
                _ => Int { radix }
            };
        } else {
            self.eat_decimal_numbers();
        }

        match self.peek() 
                
    }
    

    /**
     * Parses indentifier or keyword.
     */
    fn identifier(&mut self) -> TokenKind {
        self.eat_while(|c| is_ident_continue(c));
        Ident
    }


    /**
     * Prases raw identifiers
     */
    fn raw_identifier(&mut self) -> TokenKind {
        self.consume();
        let c = self.peek();
        if is_ident_start(c) {
            self.identifier()
        } else {
            Unknown
        }
    }
}


/**
 * Check if the given character is a whitespace character.
 */
fn is_whitespace(c: char) -> bool {
    match c {
        // ASCII whitespaces
        | '\u{0009}' // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
            => true,
        _ => false,
    }
    

    /**
     * Eats characters while there are decimal numbers or underscores.
     * 
     */
    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => {
                    self.consume();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.consume();
                }
                _ => break;
            }
        }
        has_digits
    }


    /**
     * Eats characters while there are hexadecimal digits or undescores.
     * Returns true if any hexadecimal digits were eaten, false otherwise.
     */
    fn eat_hexadecimal_digits(&mut self) {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => {
                    self.consume();
                },
                'a'..='f' |
                'A'..='F' |
                '0'..='9' => {
                    has_digits = true;
                    self.consume();
                }
                _ => break;
            }
        }
        has_digits
    }
}


/**
 * Check if the character is a non zero decimal digit.
 */
fn is_non_zero_dec_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}


/**
 * Check if this character the start of an identifier.
 */
fn is_ident_start(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || c == '_'
        || (c > '\x7f' && unicode_xid::UnicodeXID::is_xid_start(c))
}


/**
 * Check if this character is the continuation of an identifier.
 */
fn is_ident_continue(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '_'
        || (c > '\x7f' && unicode_xid::UnicodeXID::is_xid_continue(c))
}
