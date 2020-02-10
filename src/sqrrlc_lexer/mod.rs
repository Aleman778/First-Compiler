
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
use LitKind::*;


/**
 * Tokenize the input string.
 */
pub fn tokenize(mut input: &str) -> impl Iterator<Item = Token> + '_ {
    std::iter::from_fn(move || {
        if input.is_empty() {
            return None
        }
        let token = advance_token(&mut Cursor::new(input));
        input = &input[token.len..0];
        Some(token)
    })
}


/**
 * Parses the next token from input string.
 */
fn advance_token(cursor: &mut Cursor) -> Token {
    let first_char = cursor.eat();
    let token_kind = match first_char {
        // Matching against beginning slash.
        '/' => match cursor.peek() {
            '/' => line_comment(cursor),
            '*' => block_comment(cursor),
            _ => TokenKind::Slash,
        },
        
        // Match against whitespaces.
        c if is_whitespace(c) => whitespace(cursor),

        // Matching against number literals
        '0'..='9' => Literal { kind: number_literal(cursor) },

        'b' => if cursor.peek() == '\'' {
            Literal { kind: byte_literal(cursor) },
        },

        // Matching against strings
        '"' => Literal { kind: string_literal(cursor) },

        // Matching against raw identifiers
        'r' => if cursor.peek() == '#' {
            raw_identifier(cursor);
        },

        // Matching against identifiers and keywords.
        c if is_ident_start(c) => identifier(cursor),

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
        '$' => Dollar,
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
    Token::new(token_kind, cursor.num_eaten())
}


/**
 * Parses line comment token.
 */
fn line_comment(cursor: &mut Cursor) -> TokenKind {
    cursor.consume();
    cursor.eat_while(|c| c != '\n');
    LineComment
}


/**
 * Parses block comment token. e.g.
 * /* ... */ or nested /* ... /* ... */ */ 
 */
fn block_comment(cursor: &mut Cursor) -> TokenKind {
    cursor.consume();
    let mut depth: usize = 1;
    while let Some(c) = cursor.eat() {
        match c {
            '/' => if cursor.peek() == '*' {
                cursor.consume();
                depth += 1;
            },
            '*' => if cursor.peek() == '/' {
                cursor.consume();
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
fn whitespace(cursor: &mut Cursor) -> TokenKind {
    cursor.eat_while(|c| is_whitespace(c));
    Whitespace
}


/**
 * Parsers numberical literal token.
 */
fn number_literal(cursor: &mut Cursor) -> LitKind {
    let radix = Radix::Decimal;
    let first_char = cursor.prev;
    if first_char == '0' {
        let has_digits = match cursor.peek() {
            // Binary prefix
            'b' => {
                radix = Radix::Binary;
                cursor.consume();
                eat_decimal_digits(cursor)
            }
            // Octal prefix
            'o' => {
                radix = Radix::Octal;
                cursor.consume();
                eat_decimal_digits(cursor)
            }
            // Hexadecimal prefix
            'x' => {
                radix = Radix::Hexadecimal;
                cursor.consume();
                eat_decimal_digits(cursor)
            }
            // No radix prefix
            '0'..='9' | '_' | '.' | 'e' | 'E' => {
                eat_decimal_digits(cursor);
                true
            }
            // Integer number 0
            _ => return Int { radix, empty: false }
        };

        if !has_digits {
            return Int { radix, empty: true }
        }
    } else {
        eat_decimal_digits(cursor);
    }

    // Floating point numbers
    match cursor.peek() {

    }
}


fn character_literal(cursor &mut Cursor) -> {

}


fn byte_literal(cursor: &mut Cursor) -> {

}


fn string_literal(cursor: &mut Cursor) -> {

}


fn raw_string_literal(cursor: &mut Cursor) -> {

}


/**
 * Parses indentifier or keyword.
 */
fn identifier(cursor: &mut Cursor) -> TokenKind {
    cursor.eat_while(|c| is_ident_continue(c));
    Ident
}


/**
 * Prases raw identifiers
 */
fn raw_identifier(cursor: &mut Cursor) -> TokenKind {
    cursor.consume();
    let c = cursor.peek();
    if is_ident_start(c) {
        cursor.identifier()
    } else {
        Unknown
    }
}


/**
 * Consumes characters while the next character is
 * either a decimal digit or an underscore character.
 */
fn eat_decimal_digits(cursor: &mut Cursor) -> bool {
    let mut has_digits = false;
    loop {
        match cursor.peek() {
            '_' => {
                cursor.consume();
            }
            '0'..='9' => {
                has_digits = true;
                cursor.consume();
            }
            _ => break
        }
    }
    has_digits
}


/**
 * Consumes characters while the next character is 
 * either a hexadecimal digit or an underscore character.
 */
fn eat_hexdecimal_digits(cursor: &mut Cursor) -> bool {
    let mut has_digits = false;
    loop {
        match cursor.peek() {
            '_' => {
                cursor.consume();
            }
            'a'..='f' |
            'A'..='F' |
            '0'..='9' => {
                has_digits = true;
                cursor.consume();
            }
        }
    }
    has_digits
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
