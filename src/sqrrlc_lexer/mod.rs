//! The lexer stage of the compiler converts input source code and converts it into tokens.


mod cursor;
pub mod tokens;


pub use crate::sqrrlc_lexer::tokens::*;
use crate::sqrrlc_lexer::cursor::*;
use std::iter::Iterator;
use LitKind::*;
use TokenKind::*;


/**
 * Tokenize the input string.
 * The base position should match start_pos in FileSource.
 */
pub fn tokenize(input: &str, base_pos: usize) -> TokenStream {
    TokenStream::new(input, base_pos)
}


/**
 * The token stream implements an iterator over a stream of tokens
 * that are lexed by the provided tokenizer.
 */
#[derive(Clone)]
pub struct TokenStream<'a> {
    input: &'a str,
    base_pos: usize,
}


impl<'a> TokenStream<'a> {
    /**
     * Creates a new token stream.
     */
    pub fn new(input: &'a str, base_pos: usize) -> TokenStream {
        TokenStream { input, base_pos }
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
        if self.input.is_empty() {
            return None;
        }
        let cur = &mut Cursor::new(&self.input); 
        if is_whitespace(cur.first()) {
            let len_consumed = cur.eat_while(|c| is_whitespace(c));
            self.base_pos += len_consumed;
            self.input = &self.input[len_consumed..];
        }
        let token = advance_token(&mut Cursor::new(&self.input), self.base_pos);
        self.base_pos += token.len;
        self.input = &self.input[token.len..];
        Some(token)
    }
}


/**
 * Parses the next token from input string.
 * Panics if cursor has no more characters to consume.
 */
fn advance_token(cur: &mut Cursor, base_pos: usize) -> Token {
    let token_kind = match cur.eat().unwrap() {
        // Matching against beginning slash
        '/' => match cur.first() {
            '/' => line_comment(cur),
            '*' => block_comment(cur),
            _ => Slash,
        },

        // Matching against byte literals
        'b' => match (cur.first(), cur.second()) {
            ('\'', _) => {
                cur.eat();
                let terminated = eat_character(cur);
                let kind = Byte { terminated };
                let suffix_start = cur.len_consumed();
                eat_literal_suffix(cur);
                Literal { kind, suffix_start }
            }
            ('"', _) => {
                cur.eat();
                let terminated = eat_string(cur);
                let kind = ByteStr { terminated };
                let suffix_start = cur.len_consumed();
                eat_literal_suffix(cur);
                Literal { kind, suffix_start }
            }
            ('r', '"') | ('r', '#') => {
                cur.eat();
                let (num_hashes, started, terminated) = eat_raw_string(cur);
                let kind = RawByteStr {
                    num_hashes,
                    started,
                    terminated,
                };
                let suffix_start = cur.len_consumed();
                eat_literal_suffix(cur);
                Literal { kind, suffix_start }
            }
            _ => identifier(cur),
        },

        // Matching against raw literals
        'r' => match (cur.first(), cur.second()) {
            ('#', c) if is_ident_start(c) => raw_identifier(cur),
            ('#', _) | ('"', _) => {
                let (num_hashes, started, terminated) = eat_raw_string(cur);
                let kind = RawStr {
                    num_hashes,
                    started,
                    terminated,
                };
                let suffix_start = cur.len_consumed();
                eat_literal_suffix(cur);
                Literal { kind, suffix_start }
            }
            _ => identifier(cur),
        },

        // Matching against identifiers and keywords
        c if is_ident_start(c) => identifier(cur),

        // Matching against number literals
        c @ '0'..='9' => {
            let kind = number(cur, c);
            let suffix_start = cur.len_consumed();
            eat_literal_suffix(cur);
            Literal { kind, suffix_start }
        }

        // Matching against single character tokens
        ';' => Semi,
        '.' => Dot,
        '(' => OpenParen,

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
        '^' => Caret,
        '%' => Percent,

        // Natching agains character literal
        '\'' => {
            let terminated = eat_character(cur);
            let kind = Char { terminated };
            let suffix_start = cur.len_consumed();
            eat_literal_suffix(cur);
            Literal { kind, suffix_start }
        }

        // Matching against string literal
        '"' => {
            let terminated = eat_string(cur);
            let kind = Str { terminated };
            let suffix_start = cur.len_consumed();
            eat_literal_suffix(cur);
            Literal { kind, suffix_start }
        }

        // No matched characters, unknown token type
        _ => Unknown,
    };
    Token::new(token_kind, base_pos, cur.len_consumed())
}


/**
 * Parses line comment token.
 */
fn line_comment(cur: &mut Cursor) -> TokenKind {
    debug_assert!(cur.prev == '/');
    cur.eat();
    debug_assert!(cur.prev == '/');
    cur.eat_while(|c| c != '\n');
    LineComment
}


/**
 * Parses block comment token. e.g.
 * /* ... */ or nested /* ... /* ... */ */
 */
fn block_comment(cur: &mut Cursor) -> TokenKind {
    debug_assert!(cur.prev == '/');
    cur.eat();
    debug_assert!(cur.prev == '/');
    let mut depth: usize = 1;
    while let Some(c) = cur.eat() {
        match c {
            '/' => {
                if cur.first() == '*' {
                    cur.eat();
                    depth += 1;
                }
            }
            '*' => {
                if cur.first() == '/' {
                    cur.eat();
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
            }
            _ => (),
        }
    }
    BlockComment {
        terminated: depth == 0,
    }
}


/**
 * Parsers numberical literal token.
 */
fn number(cur: &mut Cursor, first_char: char) -> LitKind {
    debug_assert!(cur.prev.is_digit(10));
    let mut radix = Radix::Decimal;
    if first_char == '0' {
        let has_digits = match cur.first() {
            // Binary prefix
            'b' => {
                radix = Radix::Binary;
                cur.eat();
                eat_decimal_digits(cur)
            }
            // Octal prefix
            'o' => {
                radix = Radix::Octal;
                cur.eat();
                eat_decimal_digits(cur)
            }
            // Hexadecimal prefix
            'x' => {
                radix = Radix::Hexadecimal;
                cur.eat();
                eat_hexadecimal_digits(cur)
            }
            // No radix prefix
            '0'..='9' | '_' | '.' | 'e' | 'E' => {
                eat_decimal_digits(cur);
                true
            }
            // Integer number 0
            _ => {
                return Int {
                    radix,
                    empty: false,
                }
            }
        };

        // There are no digits after the prefix.
        if !has_digits {
            return Int { radix, empty: true };
        }
    } else {
        eat_decimal_digits(cur);
    }

    // Optional floating point numbers
    match cur.first() {
        // Floating point number with decimal place,
        // Note: it could be range pattern or method call, e.g. `0..10` or `0.test()`
        '.' => {
            if cur.second() != '.' && !is_ident_start(cur.second()) {
                cur.eat();
                eat_decimal_digits(cur);
                let empty_exponent = if cur.first() == 'e' || cur.first() == 'E' {
                    !eat_float_exponent(cur)
                } else {
                    false
                };
                Float {
                    radix,
                    empty_exponent,
                }
            } else {
                Int {
                    radix,
                    empty: false,
                }
            }
        }
        'e' | 'E' => {
            let empty_exponent = !eat_float_exponent(cur);
            Float {
                radix,
                empty_exponent,
            }
        }
        _ => Int {
            radix,
            empty: false,
        },
    }
}


/**
 * Parses indentifier or keyword.
 */
fn identifier(cur: &mut Cursor) -> TokenKind {
    debug_assert!(is_ident_start(cur.prev));
    cur.eat_while(is_ident_continue);
    Ident
}


/**
 * Parses raw identifiers.
 */
fn raw_identifier(cur: &mut Cursor) -> TokenKind {
    debug_assert!(cur.prev == 'r' || cur.first() == '#' || is_ident_start(cur.second()));
    cur.eat();
    cur.eat_while(is_ident_continue);
    RawIdent
}


/**
 * Consumes characters while the next character is
 * either a decimal digit or an underscore character.
 */
fn eat_decimal_digits(cur: &mut Cursor) -> bool {
    let mut has_digits = false;
    loop {
        match cur.first() {
            '_' => {
                cur.eat();
            }
            '0'..='9' => {
                has_digits = true;
                cur.eat();
            }
            _ => break,
        }
    }
    has_digits
}


/**
 * Consumes characters while the next character is
 * either a hexadecimal digit or an underscore character.
 */
fn eat_hexadecimal_digits(cur: &mut Cursor) -> bool {
    let mut has_digits = false;
    loop {
        match cur.first() {
            '_' => {
                cur.eat();
            }
            'a'..='f' | 'A'..='F' | '0'..='9' => {
                has_digits = true;
                cur.eat();
            }
            _ => break,
        }
    }
    has_digits
}


/**
 * Eats float exponent characters e.g. e+5 or E-4 etc.
 * Returns true if exponent has digits, false otherwise.
 */
fn eat_float_exponent(cur: &mut Cursor) -> bool {
    debug_assert!(cur.first() == 'e' || cur.first() == 'E');
    cur.eat();
    if cur.first() == '+' || cur.first() == '-' {
        cur.eat();
    }
    eat_decimal_digits(cur)
}


/**
 * Eats a suffix which is just an identifier.
 */
fn eat_literal_suffix(cur: &mut Cursor) {
    if is_ident_start(cur.first()) {
        cur.eat_while(is_ident_continue);
    }
}


/**
 * Eats a single quoted string literal, returns true if it 
 * was terminated, i.e. ended with `'` character.
 */
fn eat_character(cur: &mut Cursor) -> bool {
    debug_assert!(cur.prev == '\'');
    while let Some(c) = cur.eat() {
        match c {
            // String is terminated.
            '\'' => return true,
            // Defines an escape character.
            '\\' => eat_escape_character(cur),
            // Illegal characters.
            '\n' | '\t' | '\r' => return false,
            // Match any other character.
            _ => (),
        }
    }
    false
}


/**
 * Eats a double quoted string literal, returns true if it 
 * was terminated, i.e. ended with `"` character.
 */
fn eat_string(cur: &mut Cursor) -> bool {
    debug_assert!(cur.prev == '"');
    while let Some(c) = cur.eat() {
        match c {
            // String is terminated.
            '"' => return true,
            // Eat the ecaped token.
            '\\' => eat_escape_character(cur),
            // Eat anything else.
            _ => (),
        }
    }
    false
}


/**
 * Eats a raw string literal, returns tuple containing the following:
 * - number of hashes used
 * - has the string started
 * - has the string terminated
 */
fn eat_raw_string(cur: &mut Cursor) -> (usize, bool, bool) {
    debug_assert!(cur.prev == 'r');
    let num_hashes = cur.eat_while(|c| c == '#');
    let mut started = false;
    let mut terminated = false;
    if cur.first() == '"' {
        cur.eat();
        started = true;
    }
    while let Some(c) = cur.eat() {
        match c {
            // String is terminated.
            '"' => {
                let found = cur.eat_while(|c| c == '#');
                if found == num_hashes {
                    terminated = true;
                    break;
                }
            }
            _ => (),
        }
    }
    (num_hashes, started, terminated)
}


/**
 * Consumes next character if is an escape character.
 * Returns true if consumed, false otherwise.
 */
fn eat_escape_character(cur: &mut Cursor) {
    debug_assert!(cur.prev == '\\');
    match cur.first() {
        // ASCII escape characters.
        'x' => {
            cur.eat();
            if cur.first().is_digit(7) {
                cur.eat();
            }
            if cur.second().is_digit(15) {
                cur.eat();
            }
        }
        'n' | 't' | 'r' | '\\' | '0' => {
            cur.eat();
        }
        // Unicode escape character.
        'u' => {
            cur.eat();
            if cur.second() == '{' {
                cur.eat();
                eat_hexadecimal_digits(cur);
                if cur.first() == '}' {
                    cur.eat();
                }
            }
        }
        // Quote escape characters.
        '\'' | '\"' => {
            cur.eat();
        }
        _ => (),
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
}


/**
 * Check if this character the start of an identifier.
 */
fn is_ident_start(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || c == '_'
}


/**
 * Check if this character is the continuation of an identifier.
 */
fn is_ident_continue(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '_'
}
