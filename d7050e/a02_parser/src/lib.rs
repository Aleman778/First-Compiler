extern crate nom;

/***************************************************************************
 * Second assignment is to parse a subset of the rust syntax and construct
 * a data structure formally an Abstract Syntax Tree (or AST). The 
 * parser should support:
 * - Functions with arguments and return values
 * - Variable declarations (incl. assignments)
 * - Binary/ Unary- Operations
 * - Branches (if statements)
 * - Loops (while loop)
 * - Types supported: i32 and bool
 *
 * Notes from previous assignment:
 * - use enum for storing different types of Operations e.g. Op::Add
 * - inherit Debug and PartialEq
 * - VerboseError stack erros give context for each parser, int, expr etc.
 * - Test the AST not the calculted value (new)
 * - Possibly use nom_locate to provide more debug information
 *     * Found at: https://github.com/fflorent/nom_locate
 ***************************************************************************/


pub mod error;
pub mod ast;
pub mod parser;
