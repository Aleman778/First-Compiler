#![allow(dead_code)]

/***************************************************************************
 * All the supported operators
 ***************************************************************************/


use std::fmt;
use crate::ast::{BinOp, UnOp};


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
     * These are based on C++ operator precedence:
     * https://en.cppreference.com/w/cpp/language/operator_precedence
     */
    pub fn get_prec(&self) -> (u8, Assoc) {
        match self {
            BinOp::Pow(_)      => (12,  Assoc::Right),
            BinOp::Mul(_)      => (11,  Assoc::Left),
            BinOp::Div(_)      => (11,  Assoc::Left),
            BinOp::Mod(_)      => (11,  Assoc::Left),
            BinOp::Add(_)      => (10,  Assoc::Left),
            BinOp::Sub(_)      => (10,  Assoc::Left),
            BinOp::Shl(_)      => (9,  Assoc::Left),
            BinOp::Shr(_)      => (9,  Assoc::Left),
            BinOp::Lt(_)       => (8,  Assoc::Left),
            BinOp::Le(_)       => (8,  Assoc::Left),
            BinOp::Gt(_)       => (8,  Assoc::Left),
            BinOp::Ge(_)       => (8,  Assoc::Left),
            BinOp::Eq(_)       => (7, Assoc::Left),
            BinOp::Ne(_)       => (7, Assoc::Left),
            BinOp::BitAnd(_)   => (6, Assoc::Left),
            BinOp::BitXor(_)   => (5, Assoc::Left), 
            BinOp::BitOr(_)    => (4, Assoc::Left),
            BinOp::And(_)      => (3, Assoc::Left),
            BinOp::Or(_)       => (2, Assoc::Left),
            BinOp::Assign(_)   => (1, Assoc::Right),
            BinOp::AddEq(_)    => (1, Assoc::Right),
            BinOp::SubEq(_)    => (1, Assoc::Right),
            BinOp::MulEq(_)    => (1, Assoc::Right),
            BinOp::DivEq(_)    => (1, Assoc::Right),
            BinOp::ModEq(_)    => (1, Assoc::Right),
            BinOp::BitAndEq(_) => (1, Assoc::Right),
            BinOp::BitOrEq(_)  => (1, Assoc::Right),
            BinOp::BitXorEq(_) => (1, Assoc::Right),
            BinOp::ShlEq(_)    => (1, Assoc::Right),
            BinOp::ShrEq(_)    => (1, Assoc::Right),

        }
    }
}


/**
 * Display formatting for binary oprators, displays the name of the operator.
 */
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinOp::Add(_)      => "+",
            BinOp::Sub(_)      => "-",
            BinOp::Mul(_)      => "*",
            BinOp::Div(_)      => "/",
            BinOp::Pow(_)      => "**",
            BinOp::Mod(_)      => "%",
            BinOp::BitAnd(_)   => "&",
            BinOp::And(_)      => "&&",
            BinOp::BitXor(_)   => "|",
            BinOp::Or(_)       => "||", 
            BinOp::BitOr(_)    => "^",
            BinOp::Shl(_)      => "<<",
            BinOp::Shr(_)      => ">>",
            BinOp::Eq(_)       => "==",
            BinOp::Ne(_)       => "!=",
            BinOp::Lt(_)       => "<",
            BinOp::Le(_)       => "<=",
            BinOp::Gt(_)       => ">",
            BinOp::Ge(_)       => ">=",
            BinOp::Assign(_)   => "=",
            BinOp::AddEq(_)    => "+=",
            BinOp::SubEq(_)    => "-=",
            BinOp::MulEq(_)    => "*=",
            BinOp::DivEq(_)    => "/=",
            BinOp::ModEq(_)    => "%=",
            BinOp::BitAndEq(_) => "&=",
            BinOp::BitOrEq(_)  => "|=",
            BinOp::BitXorEq(_) => "^=",
            BinOp::ShlEq(_)    => "<<=",
            BinOp::ShrEq(_)    => ">>=",
        };
        write!(f, "{}", s)
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
        (13, Assoc::Right)
    }
}


/**
 * Display formatting for unary operators, displays the name of operator.
 */
impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnOp::Neg(_)   => "-",
            UnOp::Not(_)   => "!",
            UnOp::Ptr(_)   => "^",
            UnOp::Deref(_) => "*",
        };
        write!(f, "{}", s)
    }
}
