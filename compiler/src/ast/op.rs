#![allow(dead_code)]

/***************************************************************************
 * All the supported operators
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * Binary operators e.g. `+`, `&&` etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add{span: Span},
    /// The `-` operator (subtraction)
    Sub{span: Span},
    /// The `*` operator (multiplication)
    Mul{span: Span},
    /// The `/` operator (division)
    Div{span: Span},
    /// The `**` operator (power)
    Pow{span: Span},
    /// The `%` operator (modulus)
    Mod{span: Span},
    /// The `&&` operator (logical and)
    And{span: Span},
    /// The `||` operator (logical or)
    Or{span: Span},
    /// The `=` operator (equality)
    Eq{span: Span},
    /// The `!=` operator (not equal to)
    Ne{span: Span},
    /// The `<` operator (less than)
    Lt{span: Span},
    /// The `<=` operator (less than or equal to)
    Le{span: Span},
    /// The `>` operator (greater than)
    Gt{span: Span},
    /// The `>=` operator (greater than or equal to)
    Ge{span: Span},
}


/**
 * Unary operators e.g. `-`, `!` or `*`
 */
#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    /// The `*` operator (dereferencing)
    Deref{span: Span},
    /// The `!` operator (logical inversion)
    Not{span: Span},
    /// The `-` operator (negation)
    Neg{span: Span},
}


/**
 * An operator can either be left or right associative.
 */
pub enum Assoc {
    Left,
    Right,
}


/**
 * Returns the precedence and associativity of the given operator.
 */
pub fn get_prec(op: &BinOp) -> (u8, Assoc) {
    match op {
        BinOp::Add{span: _} => (1, Assoc::Left),
        BinOp::Sub{span: _} => (1, Assoc::Left),
        BinOp::Mul{span: _} => (2, Assoc::Left),
        BinOp::Div{span: _} => (2, Assoc::Left),
        BinOp::Pow{span: _} => (3, Assoc::Right),
        _ => (1, Assoc::Left),
    }
}


/**
 * Returns the span info of the given binary operator.
 */
pub fn get_binop_span(op: &BinOp) -> Span {
    match op {
        BinOp::Add{span} => *span,
        BinOp::Sub{span} => *span,
        BinOp::Mul{span} => *span,
        BinOp::Div{span} => *span,
        BinOp::Pow{span} => *span,
        BinOp::Mod{span} => *span,
        BinOp::And{span} => *span,
        BinOp::Or{span} => *span,
        BinOp::Eq{span} => *span,
        BinOp::Ne{span} => *span,
        BinOp::Lt{span} => *span,
        BinOp::Le{span} => *span,
        BinOp::Gt{span} => *span,
        BinOp::Ge{span} => *span,
    }
}


/**
 * Returns the span info of the given unary operator.
 */
pub fn get_unop_span(op: &UnOp) -> Span {
    match op {
        UnOp::Deref{span} => *span,
        UnOp::Not{span} => *span,
        UnOp::Neg{span} => *span,
    }
}
