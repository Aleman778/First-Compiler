#![allow(dead_code)]

/***************************************************************************
 * Atoms are basic structures such as parenthesized expressions,
 * literals, function calls and identifiers. This is used for example to
 * prevent infinite left recursion when parsing binary operations.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    expr::Expr,
};


/**
 * Atom enum contains different types of values used
 * in expressions e.g. integers, bools etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Paren(Paren),
    Ident(Ident),
    Num(LitInt),
    Bool(LitBool),
}


/**
 * Parenthesized expressions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Paren {
    pub expr: Box<Expr>,
    pub span: Span,
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


/**
 * Function call contains the identifier and arguments.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    pub ident: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}


/**
 * Identifier struct contains a user defined name.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub id: i32,
    pub span: Span,
}
