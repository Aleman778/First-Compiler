# Home

![Squirrel](https://github.com/Aleman778/sqrrl-lang/blob/master/logo.png)

## The Sqrrl Programming Language

Let's make a compiler in Rust called Sqrrl. This is my project in the course Compiler construction and formal languages \(D7050E\) at Luleå University of Technology.

### The plan \(based on the course contents\)

1. Create a parser that can parse rust-like \(probably simplified\) syntax. And create an Abstract Syntax Tree \(AST\) representation of the program.
2. Next we will program an Interpreter for the AST
3. After that it is time to write a semantic analyser that includes a type checker that can reports errors.
4. Implement a borrow checker based on rusts ownership memory model, and or use LLVM to optimize and generate machine code.
5. Wrap everything up and integrate a simple command line interface \(CLI\) to the compiler and interpreter.

### The goals

Creating a working compiler however it is not suppose to be a perfect compiler, the overall goal is to learn about compilers, parsing, formal languages etc.

