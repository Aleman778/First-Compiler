#![allow(dead_code)]

/***************************************************************************
 * Expressions AST sub module defines all the different expressions in
 * the sqrrl language e.g. binary operations, if statements etc.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    stmt::Block,
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

    /// Expression for block statements e.g. `{ ... }`.
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
        
    /// Parenthesized expression e.g. `(5 + 3)`.
    Paren(ExprParen),

    /// Reference expression e.g. &342, &mut false.
    Reference(ExprReference),
    
    /// Expression for return statements e.g. `return true;`, `return;`.
    Return(ExprReturn),
    
    /// Expression for unary operations e.g. `-a`, `!is_err()`.
    Unary(ExprUnary),
    
    /// Expression for while statements e.g. `while true { do_something(); }`.
    While(ExprWhile),
}


impl Expr {
    /**
     * Returns the span of this expression
     */
    pub fn get_span(&self) -> Span {
        match self {
            Expr::Assign(expr)    => expr.span,
            Expr::Binary(expr)    => expr.span,
            Expr::Block(expr)     => expr.span,
            Expr::Call(expr)      => expr.span,
            Expr::Ident(expr)     => expr.span,
            Expr::If(expr)        => expr.span,
            Expr::Lit(expr)       => expr.span,
            Expr::Paren(expr)     => expr.span,
            Expr::Reference(expr) => expr.span,
            Expr::Return(expr)    => expr.span,
            Expr::Unary(expr)     => expr.span,
            Expr::While(expr)     => expr.span,
            _ => Span::new_empty(),
        }
    }
}


/**
 * Assignment of mutable variable, e.g. x = 5;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprAssign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
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
 * Block expressions used for sub block expresions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub block: Block,
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
    pub then_block: Block,
    pub else_block: Option<Block>,
    pub span: Span,
}

/**
 * Literal expression .
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprLit {
    pub lit: Lit,
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
 * Reference expression.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprReference {
    pub mutable: bool,
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
    pub block: Block,
    pub span: Span,
}
