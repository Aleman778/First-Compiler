![logo](https://raw.githubusercontent.com/Aleman778/sqrrl-lang/dev/0.1.0/logo.png)

# The Sqrrl Programming Language

Let's make a compiler in Rust called Sqrrl. This is my project in the course Compiler construction and formal languages \(D7050E\) at Luleå University of Technology.

## Course Aim \(from the course contents\)

Fundamental theories about computation and different models of computation. Construction of compilers. Lexical analysis, syntax analysis, and translation into abstract syntax. Regular expressions and grammars, context-free languages and grammars, lexer and parser generators. Identifier handling and symbol table organization. Type-checking, logical inference systems. Intermediate representations and transformations for different languages. Code optimization and register allocation. Machine code generation for common architectures.
In the course you will learn and develop your skills through hands on implementation work building your own complier from scratch. In this way theoretical aspects such as formal grammars, Structural Operational Semantics (SOS), and type rule formalisations becomes tangible. We will even touch upon memory safety and how guarantees can be achieved through static (compile time) borrow checking. Compiler backend (code optimization etc.) will be discussed in context of LLVM, which you will optionally interface as a library for code generation.

## Draft outline \(from the course contents\)

### W1 The big picture, parsing, semantic analysis, code generation.

Practical assigment:

- Define a minimal subset of Rust, including 
  - Function definitions
  - Commands (let, assignment, if then (else), while)
  - Expressions (includig function calls)
  - Primitive types (boolean, i32) and their literals
  - Explicit types everywhere
  - Explicit return(s)

- Begin writing a parser for expressions in Rust using `nom` (parser combinator library)

### W2 Formal languages and Structural Operational Semantics

Theory:

- Regular expressions and automata
- EBNF
- Structural Operational Semantics

Practical assignment:

- Formulate an EBNF for your language
- Continue on the parser implementation (you may use other tools)

### W3 Context Free Grammars, Push Down Automata and Type Checking

Theory:

- DFA/NFA (regular expressions)
- Push Down Automata (PDA) for Context Free Grammars (CFG)
- Typing Rules and their Derivations

Practical assignment:

- Formulate SOS rules for your language
- Finish parser
- Implement interpreter. Panic! on run-time error.

### W4 Parsing strategies, Mutability and Memory References

Theory:

- Parsing stratigies, pros and cons. L(1), LALR, Parising Expression Grammars (PEG), etc.
- Mutability and memory references

Practical assignment

- Formalize type rules for your language (optional)
- Start to implement type checker
- Extend parser/AST/interpreter to support `&` and `&mut. Panic! on run-time error.

### W5 Borrow checking

Theory:

- Linear types and memory safety
- The Rust borrow model

Practical assigmnent

- Finish type checker. (A program passing type checking should never run into panics in the interpreter due to type errors.)

- Start to implement borrow checker

### W6 LLVM

Theory:

- SSA form
- Concept of `unique`
- Code optimization techniques (performed by LLVM)
- LLVM API (a minimal subset)

Practical assignment

- Borrow checker implementation.
- Optional. Use LLVM as library for code generation.

### W7 Wrapping it up

Practical assignment

- Compiler harness (cli interface)
- Finish work on the compiler

### W8 Home Exam

You will get the home exam to work on the last weeks of the course. This may imply further theoretical exercises and experiments on your compiler.

## Your parser

- You may implement your parser using any tool of choice.
- You are NOT required to account for operator precedence in expressions, however you MUST support parantesized sub expressions. (+ for precedence towards higher grades)
- You are NOT required to account for location information, but your error messages will be better if you do. (+ for spans, towards higher grades)
- Error recovery is NOT required (+ for recovery towards higher grades)

## Your interpreter

- Your interpreter should be able to correctly execute programs according to your SOS.
- Your interpreter should panic (with an appropriote error message) when encountering an evaluation error (e.g., 1 + false)

## Your type checker

- Your type checker should reject ill-typed programs according to your typing rules.
- (+ for higher grades)
  - span information in type errors
  - multiple error reporting
  - type inference (relaxing explicit typing where possible)

## Your borrow checker

- Your borrow checker should reject borrow errors according to lexical scoping
- (+ for higher grades)
  - Non Lexical Lifetimes (likely hard)

## Your LLVM bindings (Optional)

Implement for higher grades
- Basic code generation.
- Pass `noalias` where possible allowing for better optimization (assuming your borrowchecker prevents aliasing).
- Other attributes, intrinsics, etc. that enables further LLVM optimizations.