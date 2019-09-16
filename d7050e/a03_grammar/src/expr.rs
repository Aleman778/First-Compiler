/**
 * Expressions can be a binary operation (two operands),
 * unary operation (single operand) or a literal.
 */
#[derive(Debug, PartialEq)]
pub enum Expr {
    // Binary Operator
    BinOp(Box<Expr>, Op, Box<Expr>),

    // Unary operator
    UnOp(Op, Box<Expr>),

    // Parentheses
    Paren(Box<Expr>),
    
    // Local variable declaration
    Local(bool, Box<Expr>, Type, Box<Expr>),

    // Assignment of mutable local variable
    Assign(Box<Expr>, Box<Expr>),

    // Variable
    Var(String),

    // Function call
    FuncCall(Box<Expr>, Vec<Expr>),

    // Block of expressions
    Block(Vec<Expr>),

    // If statement with optional else statement
    If(Box<Expr>, Box<Expr>, Box<Option<Expr>>),

    // While loop
    While(Box<Expr>, Box<Expr>),

    // Break loop
    Break(),
    
    // Continue next loop cycle
    Continue(),
        
    // Return expression
    Return(Box<Option<Expr>>),

    // Number literal
    Num(i32),

    // Boolean literal
    Bool(bool),
}


/**
 * The different kinds of operators used by
 * both the binary and unary operations.
 */
#[derive(Debug, PartialEq)]
pub enum Op {
    Equal,      // ==
    NotEq,      // !=
    LessThan,   // <
    LessEq,     // <=
    LargerThan, // >
    LargerEq,   // >=
    And,        // &&
    Or,         // ||
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Mod,        // %
    Not,        // !
}


/**
 * The different kinds of supported types.
 */
#[derive(Debug, PartialEq)]
pub enum Type {
    Int32,
    Bool,
}
