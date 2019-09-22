
/***************************************************************************
 * AST module implements an Abstract Syntax Tree data structure.
 ***************************************************************************/


/**
 * Requires the LocatedSpan struct from the nom locate crate.
 */
use nom_locate::LocatedSpan;


/**
 * Type alias of LocatedSpan for convenience.
 * Every node in the AST is recommended to include this span.
 */
pub type Span<'a> = LocatedSpan<&'a str>;


pub mod base;
pub mod atom;
pub mod expr;
