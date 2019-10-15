#![allow(dead_code)]

/***************************************************************************
 * All the supported operators
 ***************************************************************************/


use crate::ast::span::Span;


/**
 * Binary  operators e.g. `+`, `&&`, `!` etc.
 */
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add{span: Span},
    /// The `-` binary operator (subtraction)
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
 * Unary operators e.g. `-`, `!`, `*` etc.
 */
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    /// The `-` unary operator (negation)
    Neg{span: Span},
    /// The `!` operator (logical inversion)
    Not{span: Span},
    /// The `&` operator (referencing)
    Ref{span: Span},
    /// The `*` operator (dereferencing)
    Deref{span: Span},
}


/**
 * An operator can either be left or right associative.
 */
pub enum Assoc {
    Left,
    Right,
}


/**
 * Implementation of the binary operator node.
 */
impl BinOp {
    /**
     * Returns the precedence and associativity of this operator.
     * These are based on C++ operator precedence.
     */
    pub fn get_prec(&self) -> (u8, Assoc) {
        match self {
            // Precedence: 1 Associativity: Left-to-right
            BinOp::And{span: _} => (1, Assoc::Left),
            BinOp::Or{span: _}  => (1, Assoc::Left),
            
            // Precedence: 2 Associativity: Left-to-right
            BinOp::Eq{span: _}  => (2, Assoc::Left),
            BinOp::Ne{span: _}  => (2, Assoc::Left),
            
            // Precedence: 3 Associativity: Left-to-right
            BinOp::Lt{span: _}  => (3, Assoc::Left),
            BinOp::Le{span: _}  => (3, Assoc::Left),
            BinOp::Gt{span: _}  => (3, Assoc::Left),
            BinOp::Ge{span: _}  => (3, Assoc::Left),
            
            // Precedence: 4 Associativity: Left-to-right
            BinOp::Add{span: _} => (4, Assoc::Left),
            BinOp::Sub{span: _} => (4, Assoc::Left),
            
            // Precedence: 5 Associativity: Left-to-right
            BinOp::Mul{span: _} => (5, Assoc::Left),
            BinOp::Div{span: _} => (5, Assoc::Left),
            BinOp::Mod{span: _} => (5, Assoc::Left),
            
            // Precedence: 6 Associativity: Right-to-left
            BinOp::Pow{span: _} => (6, Assoc::Right),
            
        }
    }
}


/**
 * Implementation of the unary operator node.
 */
impl UnOp {
    /**
     * Returns the associativity of this operator.
     * All unary operators have precedence 7 and 
     * are right-to-left associative.
     * These are based on C++ operator precedence.
     */
    pub fn get_prec(&self) -> (u8, Assoc) {
        (7, Assoc::Right)
    }
}
