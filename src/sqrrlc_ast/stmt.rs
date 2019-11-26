#![allow(dead_code)]

/***************************************************************************
 * Statement AST sub module defines all the different statements in
 * the sqrrl language e.g. let-bindings, expressions. Usually these
 * ends with a semicolon token except for inexplicit returns.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    base::Item,
    expr::{Expr, ExprIdent},
    ty::Ty,
};


/**
 * Block contains a vector of statements.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}


/**
 * Statement can be a let binding, expression with
 * or without ending semicolon token.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Let binding for local variable assignment e.g. `let a: i32 = 5;`.
    Local(Local),

    /// Item definition.
    Item(Item),

    /// Expression with a trailing semicolon.
    Semi(Expr),
    
    /// An expression without a trailing semicolon.
    Expr(Expr),
}


/**
 * Local variable declartion defines information about
 * the variable e.g. let mut a: i32 = 53;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub mutable: bool,
    pub ident: ExprIdent,
    pub ty: Ty,
    pub init: Box<Option<Expr>>,
    pub span: Span,
}
