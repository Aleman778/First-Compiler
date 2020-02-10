# Extended Backus-Naur Form Grammar
The grammar used by the sqrrl programming language. The EBNF notation
is based on this
[wikipedia](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
article. The only difference is the inclusion of regular expression
notation, this is defined using a preceding `r` e.g. `r[0-9]` equals
to `"0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"`.

## Lexical Analysis
This language uses a lexer to convert source code into simple tokens
e.g. `1 + a` would be converted into `INTEGER`, `WHITESPACE`,
`PLUS`, `WHITESPACE`, `IDENTIFIER`. Tokens are mostly very simple only
containing one character. An example of a simple token is `+`, it
turns into a `PLUS` token. Tokens also doesn't bear any information
from the source code except its length. This is enough information in
order to reconstruct the tokens matched string, since no chars are
stripped from the source. The EBNF grammer is defined in the following
sections. The lexer eats the first character and matches it agains
every token type defined in each section below. If one matches then
the rest is not considered, so ordering may be important in some cases.

### Comments
Comment tokens not discarded in this language, since their length is
useful when extracting the substring in the source code. See the section
on [parsing]{#parser} for more informatoin about this. This is also
useful for generating automatic documentation, however these tokens
are discarded by the parser.
#### Line Comment
```EBNF
line comment = "//", { r. }, "\n"
```
Note: `r.` matches any character except newlines.

#### Block Comment

### Whitespace

### Identifiers

#### Regular identifiers

#### Raw identifiers

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
float literal = decimal literal
```

#### Character Literals

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

## Parsing
From the lexed tokens the parser then combine them together to form
semantic meaning and will report errors when unexpected token patterns
arises.
```EBNF

```
