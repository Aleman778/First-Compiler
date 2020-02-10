
/***************************************************************************
 * Defines the different kinds of tokens
 ***************************************************************************/


/**
 * Tokens doesn't contain any data, only the token kind and length.
 */
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}


impl Token {
    /**
     * Creates a new token with specific kind and length.
     */
    pub fn new(kind: TokenKind, len: usize) -> Token {
        Token { kind, len }
    }
}


/**
 * Different kinds of common lexeme tokens.
 */
#[derive(Clone, Copy, Debug)]
pub enum TokenKind {
    /// Single Line comment e.g. `// comment` or `/// doc-comment`
    LineComment,
    /// Block comments e.g. `/* comment /* recursive */ */` or `/** doc-comment */`
    BlockComment { terminated: bool },
    /// Whitespace any kind of whitespace characters e.g. `\n`, `\t` etc.
    Whitespace,
    /// Identifier e.g. `hello_word`, `MyStruct`, `x` etc.
    Ident,
    /// Raw identifiers e.g. `r#while`.
    RawIdent,
    /// Literal tokens e.g. `10`, `"hello world!"`
    Literal { kind: LitKind },
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


/**
 * Different kinds of literal tokens.
 */
#[derive(Clone, Copy, Debug)]
pub enum LitKind {
    /// Integer literal e.g. `12u8`, `0xFF`.
    Int { radix: Radix, empty: bool },
    /// Floating-point literal e.g. `32.52`, `0xb1111.11101`.
    Float { radix: Radix, empty: bool },
    /// Char literal e.g. `'a'`, `'\n'` etc.
    Char { terminated: bool },
    /// Byte literal e.g. `b'4'` etc.
    Byte { terminated: bool },
    /// String literal e.g. `"hello world!"`.
    Str { terminated: bool},
    /// Raw string literals e.g. `r##"you are "#one""##` => `you are "#one"`
    RawStr { num_hashes: usize, started: bool, terminated: bool },
}


/**
 * Different kinds of radix supported for number literals.
 */
#[derive(Clone, Copy, Debug)]
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
