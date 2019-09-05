# Squirrel Programming Language
Let's make a compiler in Rust called squirrel.
This is my project in the course Compiler construction and formal languages (D7050E).
I literally saw a squirrel outside right as I created this repository, that is why i called it sqrrl.

## The plan (based on the course contents)
1. Create a parser that can parse rust-like (probably simplified) syntax. And create an Abstract Syntax Tree (AST) representation of the program.
2. Next we will program an Interpreter for the AST
3. After that it is time to write a semantic analyser that includes a type checker that can reports errors.
4. Implement a borrow checker based on rusts ownership memory model, and or use LLVM to optimize and generate machine code.

## The goals
Creating a working compiler however it is not suppose to be a perfect compiler, the overall goal is to learn about compilers, parsing etc.