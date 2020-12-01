# Home Exam D7050E

- Fork this repo and put your answers (or links to answers) in THIS file.

## Your repo

- Link to your repo here:

## Your syntax

- Give an as complete as possible EBNF grammar for your language

- Give an example that showcases all rules of your EBNF. The program should "do" something as used in the next excercise.

- If you support pointers, make sure your example covers pointers as well.

- Compare your solution to the requirements (as stated in the README.md). What are your contributions to the implementation.



## Your semantics

- Give an as complete as possible Structural Operetional Semantics (SOS) for your language

- Explain (in text) what an interpretation of your example should produce, do that by dry running your given example step by step. Relate back to the SOS rules. You may skip repetions to avoid cluttering.

- Compare your solution to the requirements (as stated in the README.md). What are your contributions to the implementation.

## Your type checker

- Give an as complete as possible set of Type Checking Rules for your language (those rules look very much like the SOS rules, but over types not values).

- Demonstrate each "type rule" by an example.

- Compare your solution to the requirements (as stated in the README.md). What are your contributions to the implementation.

## Your borrrow checker

- Give a specification for well versus ill formed borrows. (What are the rules the borrow checker should check).

- Demonstrate the cases of ill formed borrows that your borrow checker is able to detect and reject.

- Compare your solution to the requirements (as stated in the README.md). What are your contributions to the implementation.

## Your LLVM backend

- Let your backend produces LLVM-IR for your example program.

- Describe where and why you introduced allocations and phi nodes.

- If you have added optimization passes and/or attributed your code for better optimization (using e.g., `noalias`).

- Compare your solution to the requirements (as stated in the README.md). What are your contributions to the implementation.

## Overal course goals and learning outcomes.

Comment on the alignment of the concrete course goals (taken from the course description) to the theory presented, work You have done and knowledge You have gained. (I have put some comments in [...]).

- Lexical analysis, syntax analysis, and translation into abstract syntax.

- Regular expressions and grammars, context-free languages and grammars, lexer and parser generators. [Nom is lexer/parser library (and replaces the need for a generator, while lalr-pop is a classical parser generator)]

- Identifier handling and symbol table organization. Type-checking, logical inference systems. [SOS is a logical inference system]

- Intermediate representations and transformations for different languages. [LLVM is a cross language compiler infrastructure]

- Code optimization and register allocation. Machine code generation for common architectures. [LLVM is a cross target compiler infrastructure, doing the "dirty work" of optimazation/register allocation leveraging the SSA form of the LLVM-IR]

Comment on additional things that you have experienced and learned throughout the course.
