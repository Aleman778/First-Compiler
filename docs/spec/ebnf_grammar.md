# Extended Backus-Naur Form Grammar
The grammar used by the sqrrl programming language. The EBNF notation
is based on this
[wikipedia](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
article. The only difference is the inclusion of regular expression
notation, this is defined using a preceding `r` e.g. `r[0-9]` equals
to `0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9`.

## Lexical Analysis
This language uses a lexer to convert source code into simple tokens
e.g. `1 + a` would be converted into `INTEGER`, `WHITESPACE`,
`PLUS`, `WHITESPACE`, `IDENTIFIER`. Tokens are mostly very simple only
containing one character, these are not covered in this document. An
example of a simple token is `+`, it turns into a `PLUS` token. Tokens
also doesn't bear any information from the source code except its
length. This is enough information in order to reconstruct the tokens
matched string, since no chars are stripped from the source. 
In the next sections are definitions or tokens containing one or more
symbols.

## Literals
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
LITERAL = 
    INTEGER_LITERAL
    | FLOAT_LITERAL
    | CHARACTER_LITERAL
    | BYTE_LITERAL
    | STRING_LITERAL
    | RAW_STRING_LITERAL
```
### Number Literals
For number literals the lexer handles both integers and floating point
numbers at the same time. Floating-point numbers are simply an
extension of the integer lexer.

```EBNF
INTEGER_LITERAL = ()

```

## Parsing
From the lexed tokens the parser then combine them together to form
semantic meaning and report errors when unexpected token patterns
arises.

```EBNF
Program = {FnDecl};

FnDecl = "fn" IDENT "(" [Argument {"," Argument}] ")" ["->" TYPE] Block;

Argument = IDENT ":" TYPE;

Expr = BinOp | UnOp | Local | Assign | If | While | Block | Keywords | Atom;

BinOp = Atom BINOP Expr;

UnOp = UNOP Expr;

Local = "let" ["mut"] IDENT ":" TYPE "=" Expr;

Assign = Ident "=" Expr;

If = "if" Expr Block ["else" Block];

While = "while" Expr Block;

Block = "{" {Expr ";"} "}";

Keywords = ("return" [Expr]) | KEYWORDS;

FnCall = IDENT "(" [Expr {"," Expr}] ")" ";";

Literal = NUM | BOOL;

Paren = "(" Expr ")";

Atom = Paren | Literal | FnCall | IDENT;
```

### Terminals
```EBNF
BINOP = "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||" | "+" | "-" | "*" | "/" | "%";

UNOP = "!" | "-";

TYPE = "i32" | "bool";

IDENT = r[a-zA-Z][a-zA-Z0-9_]*;

KEYWORDS = "break" | "continue";

BOOL = "true" | "false";

NUM = DIGIT+;

DIGIT = r[0-9];

MULTISPACE = r[ \r\n\t] => skip;
```
