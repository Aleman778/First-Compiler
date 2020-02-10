# Lexer Grammar - Lexical Analysis 
This document specifies the EBNF grammar used by the sqrrl programming
language. The EBNF grammar notation is based on this
[wikipedia](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
article. The only difference is the inclusion of regular expression
notation, this is defined using a preceding `r` e.g. `r[0-9]` equals
to `"0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"`.

This language uses a lexer to convert source code into simple tokens
e.g. `1 + a` would be converted into `INTEGER`, `WHITESPACE`,
`PLUS`, `WHITESPACE`, `IDENTIFIER`. Tokens are mostly very simple only
containing one character. An example of a simple token is `+`, it
turns into a `PLUS` token. Tokens also doesn't bear any information
from the source code except its length. This is enough information in
order to reconstruct the tokens matched string, since no chars are
stripped from the source. The EBNF grammar is defined in the following
sections. The lexer eats the first character and matches it agains
every token type defined in each section below. If one matches then
the rest is not considered, so ordering may be important in some cases.

## Extended Backus-Naur Form Grammar
### Comments
Comment tokens not discarded in this language, since their length is
useful when extracting the substring in the source code. See the section
on [parsing]{#parser} for more informatoin about this. This is also
useful for generating automatic documentation, however these tokens
are discarded by the parser.

#### Line Comment
```EBNF
line comment = "//", { r. }, "\n";
```
Note: `r.` matches any character except newlines.

Note: This also allows for `\\\` documentation comments.

#### Block Comment
```EBNF
block comment = "/*", { "/*" | "*/" | "\n" | r. };
```
Note: the `block comment` token includes the flag `terminated` for
later reporting errors when there are mismatched `/*` and `*/`
characters.

Note: this also allows for `\** ... *\` documentation comments.

### Whitespace
```EBNF
whitespace = { "\u{0009}" | "\u{000A}" | "\u{000B}" | "\u{000C}" |
"\u{000D}" | "\u{0020}" | "\u{0085}" | "\u{200E}" | "\u{200F}" |
"\u{2028}" | "\u{2029}" };
```
The unicode characters above are the following characters:
- ASCII whitespaces: `\t`, `\n`, `vertical tab`, `form feed`, `\r`,
  `space`
- From latin1: `NEXT LINE`
- Bidirectional markers: `LEFT-TO-RIGHT MARK`, `RIGHT-TO-LEFT MARK`
- Unicode whitespaces: `LINE SEPARATOR`, `PARAGRAPH SEPARATOR`

### Identifiers
In the lexing phase keywords are considered as identifiers since they
match against a similar pattern. The parser can then convert it to a
keyword if any matched. There are two types of identfiers, regular
identifiers are common in most languages and does not allow the use of
keywords in identifiers. There are also raw identifiers that does not
convert the identifier.
```EBNF
identifier = identifier start, { identifier continue };
raw identifier = "r#", identifier;

identifier start = r[a-z] | r[A-Z] | "_" | unicode identifier start;
identifier end = r[0-9] | r[a-z] | r[A-Z] | "_" | unicode identifier continue;
```
Note: `unicode identifier start` and `unicode identifier continue` are
elided from this document. See
[unicode-xid](https://github.com/unicode-rs/unicode-xid) repository
for more implementation of unicode identifier matching.

### Literals
The different kinds of literals supported in the langugae.
- Integer literal e.g. `12u8`, `0xFF`.
- Floating-point literal e.g. `32.52`, `0xb1111.11101`.
- Char literal e.g. `'a'`, `'\n'` etc.
- Byte literal e.g. `b'4'`, `b'\t'` etc.
- String literal e.g. `"hello world!"`.
- Raw string literals e.g. `r##"you are "#one""##` becomes `you are "#one"`.
  This allows for writing strings without escaping e.g. `"` symbols
  but `#` have to be taken into account. If there exists one `#`
  symbol there has to be aleast two `#` symbols around the string, see
  example given.
  

```EBNF
literal = 
    integer literal
    | float literal
    | character literal
    | byte literal
    | string literal
    | raw string literal;
```

#### Integer Literals
Integers and floating-point numbers are lexed in the same procedure
this is done to reduce backtracking of discarded characters. Integers
and floats are the exact same except for the dot (`.`) and exponent
(`e` or `E`) parts. For lexing numbers first consider normal numeric
lexing and after that if any of the float symbols occur simply
continue as normal. If no float symbols occurs then the number is an
integer otherwise it is a floating-point number. The grammar below is
not the actual lexer grammar. However the lexer stores enough
information inside the token so that the parser know if any of these
grammar rules are broken.
```EBNF
integer literal = ( decimal literal | binary literal | octal literal |
    hexadecimal literal ), [ integer suffix ];
    
decimal literal     = DEC_DIGIT, { DEC_DIGIT | "_" };
binary literal      = "0b", { bin digit | "_" }, bin digit, { bin digit | "_" };
octal literal       = "0o", { oct digit | "_" }, oct digit, { oct digit | "_" };
hexadecimal literal = "0x", { hex digit | "_" }, hex digit, { hex digit | "_" };

dec digit = r[0-9];
oct digit = r[0-7];
hex digit = r[a-f] | r[A-F] | r[0-9];

integer suffix = "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
               | "i8" | "i16" | "i32" | "i64" | "i128" | "isize";
```

#### Floating-Point Literals
```EBNF
float literal = 
    decimal literal,
    ( float exponent | ( ".", [ decimal literal [ float exponent ] ) ] ),
    [ float suffix ];
                
flaot exponent = ( "e" | "E" ), [ "+" | "-" ], decimal literal;

float suffix = f32 | f64;
```

#### Character Literals
```EBNF
character literal = 


#### Byte Literals

#### String Literals

#### Raw String Literals

### Symbols
Basic symbols only contains a single character.
```EBNF
semi       = ";";
dot        = ".";
openparen  = "(";
closeparen = ")";
openbrace  = "{";
closebrace = "}";
openbrace  = "[";
closebrace = "]";
at         = "@";
pound      = "#";
tilde      = "~";
question   = "?";
colon      = ":";
dollar     = "$";
eq         = "=";
not        = "!";
lt         = "<";
gt         = ">";
plus       = "+";
minus      = "-";
and        = "&";
or         = "|";
star       = "*";
slash      = "/";
caret      = "^";
percen     = "%";
```

### Unknown Token
If everything above in the lexing phase failed to match then the token
`UNKNOWN` is used. The parser can then report error if that token appears.
