# Parser grammar 
This document specifies the EBNF grammar used by the parser phase of
the compiler. From the lexed tokens the parser then combine them
together to form semantic meaning and will report errors when
unexpected token patterns arises.


## Expressin Parser
```EBNF
expression =
    literal
```

## Literal Parsers
The literal parsers uses the tokens matched strings in source
code and converts the string into an actual literal.
```EBNF
integer = 
    
```
