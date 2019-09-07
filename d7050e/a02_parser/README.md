# Assignment 2 - Parse a subset of the Rust syntax
In this assignment I will parse the targeted grammar into an Abstract Syntax Tree.
Note: precedence is not considered and I do not differentiate between types
i.e. the two types are treated as the same when parsing.
Type checking is not done at parsing and is out of scope for this assignment.

## Targeted grammar

### Functions and local variables, explicit/implicit returns
```rust
fn main() -> i32 {
    let a: i32 = f(2) + f(3);
    a + 5
}

fn f(a: i32) -> i32 {
    return a + 1;
}
```

### Conditionals
```rust
if <expr> {
    <if-body>   
}
```
or
```rust
if <expr> {
    <if-body>   
} else {
    <else-body>
}
```

### Loops
```rust
while <expr> {
    <while-body>
}
```
Maybe add support for `break` keyword

### Supported Types
- 32-bit signed integers (denoted as i32)
- Boolean type (denoted as bool), also includes keywords `true`/ `false`

### Binary Operations (two operands)
`+`, `-`, `*`, `/`, `%`, `&&`, `||`, `==`, `<`, `<=`, `>`, `>=`

### Unary Operations (single operand)
`-`, `!`
