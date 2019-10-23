#![allow(dead_code)]

/***************************************************************************
 * Expressions AST sub module defines all the different expressions in
 * the squirrel language e.g. binary operations, if statements etc.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    base::Type,
    lit::Lit,
    op::*,
};


/**
 * Expressions enum contains all the different types of expressions,
 * e.g. binary operations, local variable assignment, atoms etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Expression for mutation for variable e.g. `a = calc()`.
    Assign(ExprAssign),
    
    /// Expression for binary opeations e.g. `5 + a`, `b && check()`.
    Binary(ExprBinary),
    
    /// Expression for blocks i.e. `{ ... }`.
    Block(ExprBlock),
    
    /// Expression for break statements i.e. `break;`.
    Break(ExprBreak),
    
    /// Expression for function calls e.g. `foo(bar)`.
    Call(ExprCall),
    
    /// Expression for continue statements e.g. `continue;`.
    Continue(ExprContinue),
    
    /// Expression for identifiers e.g. `foo`, `my_function`, `__PATH__`.
    Ident(ExprIdent),
    
    /// Expression for if statements e.g. `if a > 5 { a = 6; } else { a = 4; }`.
    If(ExprIf),
    
    /// Expression for literals e.g. `32`, `true`.
    Lit(ExprLit),
    
    /// Expression for local variable assignment e.g. `let a: i32 = 5;`.
    Local(ExprLocal),
    
    /// Parenthesized expression e.g. `(5 + 3)`.
    Paren(ExprParen),
    
    /// Expression for return statements e.g. `return true;`, `return;`.
    Return(ExprReturn),
    
    /// Expression for unary operations e.g. `-a`, `!is_err()`.
    Unary(ExprUnary),
    
    /// Expression for while statements e.g. `while true { do_something(); }`.
    While(ExprWhile),
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
 * Binary operation has a left and right operand
 * and also the operator in between, e.g. 1 + 2.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: BinOp,
    pub right: Box<Expr>,
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
 * Breaks the loop.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBreak {
    pub span: Span,
}


/**
 * Function call contains the identifier and arguments.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall {
    pub ident: ExprIdent,
    pub args: Vec<Expr>,
    pub span: Span,
}


/**
 * Continue to next cycle of the loop.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprContinue {
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
 * 
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprLit {
    pub lit: Lit,
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
 * Parenthesized expressions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprParen {
    pub expr: Box<Expr>,
    pub span: Span,
}


/**
 * Return statement can optionally return an expression
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprReturn {
    pub expr: Box<Option<Expr>>,
    pub span: Span,
}


/**
 * Unary operation has an operator to the left and
 * the operand to the right, e.g. !running.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnOp,
    pub right: Box<Expr>,
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
