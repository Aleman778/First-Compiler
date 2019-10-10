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
 * Parenthesized expressions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprParen {
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
pub struct ExprFnCall {
    pub ident: ExprIdent,
    pub args: Vec<Expr>,
    pub span: Span,
}


/**
 * Identifier struct contains a user defined name.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprIdent {
    pub to_string: String,
    pub span: Span,
}
