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
            BinOp::And{span: _} => (1, Assoc::Left),
            BinOp::Or{span: _}  => (1, Assoc::Left),
            
            // Precedence: 2, Associativity: Left-to-right
            BinOp::Eq{span: _}  => (2, Assoc::Left),
            BinOp::Ne{span: _}  => (2, Assoc::Left),
            
            // Precedence: 3, Associativity: Left-to-right
            BinOp::Lt{span: _}  => (3, Assoc::Left),
            BinOp::Le{span: _}  => (3, Assoc::Left),
            BinOp::Gt{span: _}  => (3, Assoc::Left),
            BinOp::Ge{span: _}  => (3, Assoc::Left),
            
            // Precedence: 4, Associativity: Left-to-right
            BinOp::Add{span: _} => (4, Assoc::Left),
            BinOp::Sub{span: _} => (4, Assoc::Left),
            
            // Precedence: 5, Associativity: Left-to-right
            BinOp::Mul{span: _} => (5, Assoc::Left),
            BinOp::Div{span: _} => (5, Assoc::Left),
            BinOp::Mod{span: _} => (5, Assoc::Left),
            
            // Precedence: 6, Associativity: Right-to-left
            BinOp::Pow{span: _} => (6, Assoc::Right),
            
        }
    }


    /**
     * Returns the token string used by the given operator.
     */
    pub fn token(&self) -> &'static str {
        match self {
            BinOp::Add{span: _} => "+",
            BinOp::Sub{span: _} => "-",
            BinOp::Mul{span: _} => "*",
            BinOp::Div{span: _} => "/",
            BinOp::Pow{span: _} => "**",
            BinOp::Mod{span: _} => "%",
            BinOp::And{span: _} => "&&",
            BinOp::Or{span: _}  => "||",
            BinOp::Eq{span: _}  => "==",
            BinOp::Ne{span: _}  => "!=",
            BinOp::Lt{span: _}  => "<",
            BinOp::Le{span: _}  => "<=",
            BinOp::Gt{span: _}  => ">",
            BinOp::Ge{span: _}  => ">=",
        }
    }
}


/**
 * Display formatting for binary oprators, displays the name of the operator.
 */
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add{span: _} => write!(f, "add"),
            BinOp::Sub{span: _} => write!(f, "subtract"),
            BinOp::Mul{span: _} => write!(f, "multiplicate"),
            BinOp::Div{span: _} => write!(f, "divide"),
            BinOp::Pow{span: _} => write!(f, "power"),
            BinOp::Mod{span: _} => write!(f, "modolu"),
            BinOp::And{span: _} => write!(f, "logical and"),
            BinOp::Or{span: _}  => write!(f, "logical or"),
            BinOp::Eq{span: _}  => write!(f, "compare equal"),
            BinOp::Ne{span: _}  => write!(f, "compare not equal"),
            BinOp::Lt{span: _}  => write!(f, "compare less than"),
            BinOp::Le{span: _}  => write!(f, "compare less than or equal"),
            BinOp::Gt{span: _}  => write!(f, "compare greater than"),
            BinOp::Ge{span: _}  => write!(f, "compare greater than or equal"),
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
            UnOp::Neg{span: _}   => "-",
            UnOp::Not{span: _}   => "!",
            UnOp::Deref{span: _} => "*",
        }
    }
}


/**
 * Display formatting for unary operators, displays the name of operator.
 */
impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg{span: _}   => write!(f, "negated"),
            UnOp::Not{span: _}   => write!(f, "logical inverted"),
            UnOp::Deref{span: _} => write!(f, "dereferenced"),
        }
    }
}
