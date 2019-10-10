#![allow(dead_code)]

/***************************************************************************
 * Expressions AST sub module can be really anything from simple
 * arithmetic expressions to if statements etc.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    base::Type,
    op::*,
    atom::*,
};


/**
 * Expressions enum contains all the different types of expressions,
 * e.g. binary operations, local variable assignment, atoms etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Expression for binary opeations
    BinOp(ExprBinOp),
    /// Expression for unary operations
    UnOp(ExprUnOp),
    /// Expression for local variable assignment
    Local(ExprLocal),
    /// Expression for mutation for variable
    Assign(ExprAssign),
    /// Expression for blocks (a.k.a. body)
    Block(ExprBlock),
    /// Expression for if statements
    If(ExprIf),
    /// Expression for while statements
    While(ExprWhile),
    /// Expression for return statements
    Return(ExprReturn),
    /// Expression for break statements
    Break(ExprBreak),
    /// Expression for continue statements
    Continue(ExprContinue),
    /// Parenthesized expression
    Paren(ExprParen),
    /// Expression for identifiers
    Ident(ExprIdent),
    /// Expression for function calls
    FnCall(ExprFnCall),
    /// Expression for integer literals
    Num(LitInt),
    /// Expression for boolean literals
    Bool(LitBool),
}


/**
 * Binary operation has a left and right operand
 * and also the operator in between, e.g. 1 + 2.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinOp {
    pub left: Box<Expr>,
    pub op: Op,
    pub right: Box<Expr>,
    pub span: Span,
}


/**
 * Unary operation has an operator to the left and
 * the operand to the right, e.g. !running.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnOp {
    pub op: Op,
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
    pub ident: ExprIdent,
    pub ty: Type,
    pub init: Box<Expr>,
    pub span: Span,
}


/**
 * Assignment of mutable variable, e.g. x = 5;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprAssign {
    pub ident: ExprIdent,
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
    pub cond: Box<Expr>,
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
