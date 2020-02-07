
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
    BlockComment,
    /// Whitespace any kind of whitespace characters e.g. `\n`, `\t` etc.
    Whitespace,
    /// Keyword can be e.g. `while`, `let` etc. See `KeywordKind` for more details.
    Keyword { kind: KeywordKind },
    /// Identifier e.g. `hello_word`, `MyStruct`, `x` etc.
    Ident,
    /// Identifiers starting with `r#` used for allowing keywords in identifiers.
    RawIdent,
    /// Literal tokens e.g. `10`, `"hello world!"`
    Literal { kind: LiteralKind },
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
 * Different kinds of keyword lexem tokens.
 */
#[derive(Clone, Copy, Debug)]
pub enum KeywordKind {
    /// Function keyword.
    Fn,
    /// While loop keyword.
    While,
    /// If statement keyword.
    If,
    /// Else statement keyword.
    Else,
    /// Let statement keyword.
    Let,
    /// Mutability keyword.
    Mut,
    /// Extern keyword for FFI.
    Extern,
}


/**
 * Different kinds of literal tokens.
 */
#[derive(Clone, Copy, Debug)]
pub enum LiteralKind {
    /// Integer literal e.g. `12u8`, `0xFF`.
    Int { base: Radix },
    /// Floating-point literal e.g. `32.52`, `0xb1111.11101`.
    Float { base: Radix },
    /// String literal e.g. `"hello world!"`.
    Str,
    /// Char literal e.g. `'a'`, `'\n'` etc.
    Char,
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
