
/***************************************************************************
 * Atoms are basic structures such as parenthesized expressions,
 * literals, function calls and identifiers. This is used for example to
 * prevent infinite left recursion when parsing binary operations.
 ***************************************************************************/


use crate::ast::{
    Span,
    expr::Expr,
};


/**
 * Atom enum contains different types of values used
 * in expressions e.g. integers, bools etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    Paren(Paren<'a>),
    Ident(Ident<'a>),
    Num(LitInt<'a>),
    Bool(LitBool<'a>),
}


/**
 * Parenthesized expressions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Paren<'a> {
    pub expr: Box<Expr<'a>>,
    pub span: Span<'a>,
}


/**
 * Literal integer struct has an i32 value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitInt<'a> {
    pub value: i32,
    pub span: Span<'a>,
}


/**
 * Literal boolean struct has a bool value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitBool<'a> {
    pub value: bool,
    pub span: Span<'a>,
}


/**
 * Function call contains the identifier and arguments.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnCall<'a> {
    pub ident: Ident<'a>,
    pub args: Vec<Expr<'a>>,
    pub span: Span<'a>,
}


/**
 * Identifier struct contains a user defined name.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Ident<'a> {
    pub to_string: &'a str,
    pub span: Span<'a>,
}
