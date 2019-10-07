#![allow(dead_code)]

/***************************************************************************
 * Expressions AST sub module can be really anything from simple
 * arithmetic expressions to if statements etc.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    op::*,
    atom::Atom,
    atom::Ident,
    base::Type,
};


/**
 * Expressions enum contains all the different types of expressions,
 * e.g. binary operations, local variable assignment, atoms etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinOp(ExprBinOp),
    UnOp(ExprUnOp),
    Local(ExprLocal),
    Assign(ExprAssign),
    Block(ExprBlock),
    If(ExprIf),
    While(ExprWhile),
    Return(ExprReturn),
    Break(ExprBreak),
    Continue(ExprContinue),
    Atom(Atom),
}


/**
 * Binary operation has a left and right operand
 * and also the operator in between, e.g. 1 + 2.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinOp {
    pub left: Box<Atom>,
    pub op: BinOp,
    pub right: Box<Expr>,
    pub span: Span,
}


/**
 * Unary operation has an operator to the left and
 * the operand to the right, e.g. !running.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnOp {
    pub op: UnOp,
    pub right: Box<Expr>,
    pub span: Span,
}


/**
 * Local variable declartion defines information about
 * the variable e.g. let mut a: i32 = 53;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprLocal {
    pub mutable: bool,
    pub ident: Ident,
    pub ty: Type,
    pub init: Box<Expr>,
    pub span: Span,
}


/**
 * Assignment of mutable variable, e.g. x = 5;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprAssign {
    pub ident: Ident,
    pub expr: Box<Expr>,
    pub span: Span,
}


/**
 * Block contains a vector of expressions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub stmts: Vec<Expr>,
    pub span: Span,
}


/**
 * If statement has a condition and a block
 * that is executed if condition is true otherwise the
 * second block is optionally executed instead.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprIf {
    pub cond: Box<Atom>,
    pub then_block: ExprBlock,
    pub else_block: Option<ExprBlock>,
    pub span: Span,
}


/**
 * While loops includes a condition and a block that is
 * executed each time the condition is true.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprWhile {
    pub cond: Box<Expr>,
    pub block: ExprBlock,
    pub span: Span,
}


/**
 * Return statement can optionally return an expression
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprReturn {
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}


/**
 * Breaks the loop.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBreak {
    pub span: Span,
}


/**
 * Continue to next cycle of the loop.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprContinue {
    pub span: Span,
}
