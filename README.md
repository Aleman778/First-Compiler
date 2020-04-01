![Squirrel](https://raw.githubusercontent.com/Aleman778/sqrrl-lang/master/logo.png)
# The Sqrrl Programming Language
The main source code repository for the Sqrrl compiler. This compiler is currently
under active development and is aimed towards game development. This compiler started
out as a school project. After increasing interest in compiler construction I decided
to incorperate more functionality beyond the scope of my compiler construction course.

Currently the beta release [`0.1.0`](https://github.com/Aleman778/sqrrl-lang/releases/tag/0.1.0) 
is a simple prototype compiler that includes parser (using parser combinator library `nom`), 
type checker and an interpreter. This version only includes a small subset of Rust language 
features and syntax and is unusable for any real world project.

The next version `0.2.0` is nearly a complete rewrite of the entire compiler and will be built
on my own parser and lexer for a complete langauge but the syntax is more moving towards
the Jai language syntax. There will also be code generation using LLVM as backend.

# Goals
Here I have list some goals that I want to acheive with this compiler:

- **Fast compilation for rapid development**, I prefer to compile more often with fewer errors
than less often with many errors. 
- **Fast compiled code for release builds**.
- **Improved language syntax over Rust**, I like the Rust syntax aesthetically but there 
are some inconsistencies and annoyances when using it in practice. It is however important 
that the aesthetics of the syntax is not prioritized over function and usability.
- **More focus on data oriented design** and removal of any bad object oriented programming (OOP)
constructs such as inheritance, encapsulation. Some interesting videos on this topic:
  - [CppCon 2014: Mike Acton "Data-Oriented Design and C++"](https://www.youtube.com/watch?v=rX0ItVEVjHc)
  - [Object-Oriented Programming is Bad](https://www.youtube.com/watch?v=QM1iUe6IofM)
- *More to come later...*

Note that some of these goals might not be in the next release `0.2.0` since some of these are long-term goals.


# About
## Syntax
The syntax is inspired by the Jai and Rust programming languages.
[Here is a taste of the new syntax](https://github.com/Aleman778/sqrrl-lang/blob/impl/parser/examples/syntax.sq).

## Documentations
I will be working on the documentation for this language over the over the course of the development.
This documentation will be updated as I'm working so anything is subject to change.
[Link to documentation](https://sqrrl-docs.alemen.se/).
