#![allow(dead_code)]

/***************************************************************************
 * All the supported operators
 ***************************************************************************/


/**
 * Binary operators e.g. `+`, `&&` etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Mod,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `=` operator (equality)
    Eq,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `>` operator (greater than)
    Gt,
    /// The `>=` operator (greater than or equal to)
    Ge,
}


/**
 * Unary operators e.g. `-`, `!` or `*`
 */
#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    /// The `*` operator (dereferencing)
    Deref,
    /// The `!` operator (logical inversion)
    Not,
    /// The `-` operator (negation)
    Neg,
}
