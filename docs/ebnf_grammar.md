# Extended Backus-Naur Form Grammar
Note: that I use regular expressions to simplify the EBNF grammar definition,
and I choose to denote the use of regex with a preceding r e.g. r[0-9].
This is done so that [] - optional syntax is not confused with regex optionals.
Note: this grammar is based on my parser implementation from assignment 2 but when using LALRPOP the syntax is different.

## Non-terminals
```EBNF
File = {Item};

Item = FnItem | ForeignMod;

ForeignItem = ForeignFnItem | ForeignMod;

ForeignMod = {

FnItem

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

## Terminals
```EBNF
BINOP = "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||" | "+" | "-" | "*" | "/" | "%";

UNOP = "!" | "-";

TYPE = "i32" | "bool";

IDENT = r[a-zA-Z][a-zA-Z0-9_]*;

KEYWORDS = "break" | "continue";

BOOL = "true" | "false";

NUM = DIGIT+;

DIGIT = r[0-9];

MULTISPACE = " " | "\r" | "\n" | "\t" => skip;
```
