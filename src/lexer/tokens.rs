
/***************************************************************************
 * Defines the different kinds of tokens
 ***************************************************************************/


use std::fmt;
use crate::span::Span;


/**
 * Dummy token, is an unknown token that points to position 0 and has length of 0.
 */
pub const DUMMY_TOKEN: Token = Token { 
    kind: TokenKind::Unknown, 
    base: 0, 
    len: 0 
};


/**
 * Tokens doesn't contain any data, only the token kind and length.
 */
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub base: usize,
    pub len: usize,
}


impl Token {
    /**
     * Creates a new token with specific kind and length.
     */
    pub fn new(kind: TokenKind, base: usize, len: usize) -> Token {
        Token { kind, base, len }
    }


    pub fn to_span(&self) -> Span {
        Span::new(self.base, self.len)
    }
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}


/**
 * Different kinds of common lexeme tokens.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    /// Single Line comment e.g. `// comment` or `/// doc-comment`.
    LineComment,
    /// Block comments e.g. `/* comment /* recursive */ */` or `/** doc-comment */`.
    BlockComment { terminated: bool },
    /// Any whitespace characters sequence.
    Whitespace,
    /// Identifier e.g. `hello_word`, `MyStruct`, `x`, `let`, `while` etc.
    Ident,
    /// Raw identifiers e.g. `r#while`.
    RawIdent,
    /// Literal tokens e.g. `10_u8`, `"hello world!"`
    Literal { kind: LitKind, suffix_start: usize },
    /// Semicolon token `;`.
    Semi,
    /// Comma token `,`.
    Comma,
    /// Dot token `.`.
    Dot,
    /// Open parenthesis token `(`.
    OpenParen,
    /// Close parenthesis token `)`.
    CloseParen,
    /// Open brace token `{`.
    OpenBrace,
    /// Close brace token `}`.
    CloseBrace,
    /// Open bracket token `[`.
    OpenBracket,
    /// Close bracket token `]`.
    CloseBracket,
    /// At token `@`.
    At,
    /// Hashtag token `#`.
    Pound,
    /// Tilde token `~`.
    Tilde,
    /// Question token `?`.
    Question,
    /// Colon token `:`.
    Colon,
    /// Dollar token `$`.
    Dollar,
    /// Equal token `=`.
    Eq,
    /// Not token `!`.
    Not,
    /// Less than token`<`.
    Lt,
    /// Greater than token`>`.
    Gt,
    /// Plus token `+`.
    Plus,
    /// Minus sign token `-`.
    Minus,
    /// And token `&`.
    And,
    /// Or token `|`.
    Or,
    /// Star token `*`.
    Star,
    /// slash token `/`.
    Slash,
    /// Caret token `^`.
    Caret,
    /// Percent token `%`.
    Percent,
    /// Unknown token, not expected by the lexer.
    Unknown,
}


impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            TokenKind::LineComment => "line comment",
            TokenKind::BlockComment { terminated: _ } => "block comment",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Ident => "identifier",
            TokenKind::RawIdent => "raw identifier",
            TokenKind::Literal { kind, suffix_start: _ } => { 
                return write!(f, "{}", kind); 
            }
            TokenKind::Semi => ";",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::OpenParen => "(",
            TokenKind::CloseParen => ")",
            TokenKind::OpenBrace => "{",
            TokenKind::CloseBrace => "}",
            TokenKind::OpenBracket => "[",
            TokenKind::CloseBracket => "]",
            TokenKind::At => "@",
            TokenKind::Pound => "#",
            TokenKind::Tilde => "~",
            TokenKind::Question => "?",
            TokenKind::Colon => ":",
            TokenKind::Dollar => "$",
            TokenKind::Eq => "=",
            TokenKind::Not => "!",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::And => "&",
            TokenKind::Or => "|",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Caret => "^",
            TokenKind::Percent => "%",
            TokenKind::Unknown => "unknown",
        };
        write!(f, "{}", name)
    }
}

/**
 * Different kinds of literal tokens.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LitKind {
    /// Integer literal e.g. `12u8`, `0xFF`.
    Int { radix: Radix, empty: bool },
    /// Floating-point literal e.g. `32.52`, `0xb1111.11101`.
    Float { radix: Radix, empty_exponent: bool },
    /// Char literal e.g. `'a'`, `'\n'` etc.
    Char { terminated: bool },
    /// Byte literal e.g. `b'4'.
    Byte { terminated: bool },
    /// Byte string e.g. `b"Hello world!"`.
    ByteStr { terminated: bool },
    /// String literal e.g. `"Hello world!"`.
    Str { terminated: bool },
    /// Raw string literals e.g. `r##"you are "#one""##` => `you are "#one"`
    RawStr { num_hashes: usize, started: bool, terminated: bool },
    /// Raw byte string literals e.g. `br##"you are "#one""##`.
    RawByteStr { num_hashes: usize, started: bool, terminated: bool }
}


impl fmt::Display for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            LitKind::Int { radix: _, empty: _ } => 
                "integer",
            LitKind::Float { radix: _, empty_exponent: _ } => 
                "float",
            LitKind::Char { terminated: _ } => 
                "character",
            LitKind::Byte { terminated: _ } => 
                "byte",
            LitKind::ByteStr { terminated: _ } => 
                "byte string",
            LitKind::Str { terminated: _ } => 
                "string",
            LitKind::RawStr { num_hashes: _, started: _, terminated: _ } => 
                "raw string",
            LitKind::RawByteStr { num_hashes: _, started: _, terminated: _ } => 
                "raw byte strings"
        };
        write!(f, "{}", name)
    }
}


/**
 * Different kinds of radix supported for number literals.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Radix {
    /// Literals with binary base prefixed with `0b`.
    Binary,
    /// Literals with octal base prefixed with `0o`.
    Octal,
    /// Literals with hexadecimal base prefixed with `0x`.
    Hexadecimal,
    /// Literals with standard base 10 has no prefix.
    Decimal,
}
