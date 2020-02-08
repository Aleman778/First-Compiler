#![allow(dead_code)]

/***************************************************************************
 * All the supported operators
 ***************************************************************************/


use std::fmt;
use crate::sqrrlc_ast::{BinOp, UnOp};


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
            // Precedence: 1, Associativity: Left-to-right
            BinOp::And(_) => (1, Assoc::Left),
            BinOp::Or(_)  => (1, Assoc::Left),
            
            // Precedence: 2, Associativity: Left-to-right
            BinOp::Eq(_)  => (2, Assoc::Left),
            BinOp::Ne(_)  => (2, Assoc::Left),
            
            // Precedence: 3, Associativity: Left-to-right
            BinOp::Lt(_)  => (3, Assoc::Left),
            BinOp::Le(_)  => (3, Assoc::Left),
            BinOp::Gt(_)  => (3, Assoc::Left),
            BinOp::Ge(_)  => (3, Assoc::Left),
            
            // Precedence: 4, Associativity: Left-to-right
            BinOp::Add(_) => (4, Assoc::Left),
            BinOp::Sub(_) => (4, Assoc::Left),
            
            // Precedence: 5, Associativity: Left-to-right
            BinOp::Mul(_) => (5, Assoc::Left),
            BinOp::Div(_) => (5, Assoc::Left),
            BinOp::Mod(_) => (5, Assoc::Left),
            
            // Precedence: 6, Associativity: Right-to-left
            BinOp::Pow(_) => (6, Assoc::Right),
            
        }
    }


    /**
     * Returns the token string used by the given operator.
     */
    pub fn token(&self) -> &'static str {
        match self {
            BinOp::Add(_) => "+",
            BinOp::Sub(_) => "-",
            BinOp::Mul(_) => "*",
            BinOp::Div(_) => "/",
            BinOp::Pow(_) => "**",
            BinOp::Mod(_) => "%",
            BinOp::And(_) => "&&",
            BinOp::Or(_)  => "||",
            BinOp::Eq(_)  => "==",
            BinOp::Ne(_)  => "!=",
            BinOp::Lt(_)  => "<",
            BinOp::Le(_)  => "<=",
            BinOp::Gt(_)  => ">",
            BinOp::Ge(_)  => ">=",
        }
    }
}


/**
 * Display formatting for binary oprators, displays the name of the operator.
 */
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add(_) => write!(f, "add"),
            BinOp::Sub(_) => write!(f, "subtract"),
            BinOp::Mul(_) => write!(f, "multiplicate"),
            BinOp::Div(_) => write!(f, "divide"),
            BinOp::Pow(_) => write!(f, "power"),
            BinOp::Mod(_) => write!(f, "modolu"),
            BinOp::And(_) => write!(f, "logical and"),
            BinOp::Or(_)  => write!(f, "logical or"),
            BinOp::Eq(_)  => write!(f, "compare equal"),
            BinOp::Ne(_)  => write!(f, "compare not equal"),
            BinOp::Lt(_)  => write!(f, "compare less than"),
            BinOp::Le(_)  => write!(f, "compare less than or equal"),
            BinOp::Gt(_)  => write!(f, "compare greater than"),
            BinOp::Ge(_)  => write!(f, "compare greater than or equal"),
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


    /**
     * Returns the token string used by the given operator.
     */
    pub fn token(&self) -> &'static str {
        match self {
            UnOp::Neg(_)   => "-",
            UnOp::Not(_)   => "!",
            UnOp::Deref(_) => "*",
        }
    }
}


/**
 * Display formatting for unary operators, displays the name of operator.
 */
impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg(_)   => write!(f, "negated"),
            UnOp::Not(_)   => write!(f, "logical inverted"),
            UnOp::Deref(_) => write!(f, "dereferenced"),
        }
    }
}
