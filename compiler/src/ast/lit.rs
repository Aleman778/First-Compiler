
/***************************************************************************
 * Literal AST sub module defines different types of literals
 * e.g. integers, booleans etc.
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * Literal enum defines different types of literals supported.
 * e.g. 5, false etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// Literal for integers e.g. 5
    Int(LitInt),
    /// Literal for booleans e.g. false
    Bool(LitBool)
}


/**
 * Literal integer struct has an i32 value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitInt {
    pub value: i32,
    pub span: Span,
}


/**
 * Literal boolean struct has a bool value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitBool {
    pub value: bool,
    pub span: Span,
}
