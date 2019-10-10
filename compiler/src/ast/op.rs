#![allow(dead_code)]

/***************************************************************************
 * All the supported operators
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * Binary and unary operators operators e.g. `+`, `&&`, `!` etc.
 */
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    /// The `+` operator (addition)
    Add{span: Span},
    /// The `-` operator (subtraction)
    Sub{span: Span},
    /// The `*` operator (multiplication/ dereferencing)
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
    /// The `!` operator (logical inversion)
    Not{span: Span},
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
pub fn get_prec(op: &Op) -> (u8, Assoc) {
    match op {
        Op::Add{span: _} => (1, Assoc::Left),
        Op::Sub{span: _} => (1, Assoc::Left),
        Op::Mul{span: _} => (2, Assoc::Left),
        Op::Div{span: _} => (2, Assoc::Left),
        Op::Pow{span: _} => (3, Assoc::Right),
        _ => (1, Assoc::Left),
    }
}


/**
 * Returns the span info of the given binary operator.
 */
pub fn get_span(op: &Op) -> Span {
    match op {
        Op::Add{span} => *span,
        Op::Sub{span} => *span,
        Op::Mul{span} => *span,
        Op::Div{span} => *span,
        Op::Pow{span} => *span,
        Op::Mod{span} => *span,
        Op::And{span} => *span,
        Op::Or{span} => *span,
        Op::Eq{span} => *span,
        Op::Ne{span} => *span,
        Op::Lt{span} => *span,
        Op::Le{span} => *span,
        Op::Gt{span} => *span,
        Op::Ge{span} => *span,
        Op::Not{span} => *span,
    }
}
