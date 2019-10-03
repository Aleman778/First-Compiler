/**
 * Require LocatedSpan from crate nom_locate.
 */
use nom_locate::LocatedSpan;


/**
 * Type alias of LocatedSpan for convenience.
 */
pub type Span<'a> = LocatedSpan<&'a str>;


/***************************************************************************
 * Abstract Syntax Tree (AST)
 ***************************************************************************/


/**
 * Expressions can be a binary operation (two operands),
 * unary operation (single operand) or a literal.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    // Binary Operator
    BinOp(Box<SpanExpr<'a>>, SpanOp<'a>, Box<SpanExpr<'a>>),

    // Unary operator
    UnOp(SpanOp<'a>, Box<SpanExpr<'a>>),

    // Parentheses
    Paren(Box<SpanExpr<'a>>),
    
    // Local variable declaration
    Local(bool, Box<SpanExpr<'a>>, SpanType<'a>, Box<SpanExpr<'a>>),

    // Assignment of mutable local variable
    Assign(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>),

    // Function call
    Call(Box<SpanExpr<'a>>, Vec<SpanExpr<'a>>),

    // Block of expressions
    Block(Vec<SpanExpr<'a>>),

    // If statement with optional else statement
    If(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>, Box<Option<SpanExpr<'a>>>),

    // While loop
    While(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>),

    // Break loop
    Break(),
    
    // Continue next loop cycle
    Continue(),
        
    // Return expression
    Return(Box<Option<SpanExpr<'a>>>),
    
    // Identifier
    Ident(&'a str),

    // Number  literal
    Num(i32),

    // Boolean literal
    Bool(bool),
}


/**
 * Type alias of expressions to include span.
 */
pub type SpanExpr<'a> = (Span<'a>, Expr<'a>);


/**
 * Argument struct defines an identifier with a type
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Argument<'a>(pub SpanExpr<'a>, pub SpanType<'a>);


/**
 * Type alias of argument to include span.
 */
pub type SpanArg<'a> = (Span<'a>, Argument<'a>);

    
/**
 * Function struct defines the functions characteristics,
 * i.e. function name identifier, vector of arguments, the return type
 * and the block containing the function implementation.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a>(pub Box<SpanExpr<'a>>, pub Vec<SpanArg<'a>>, pub Option<SpanType<'a>>, pub Box<SpanExpr<'a>>);


/**
 * Type alias of function to include span.
 */
pub type SpanFn<'a> = (Span<'a>, Function<'a>);


/**
 * Root of the Abstract Syntax Tree, contains different itesm
 * currently I only support functions. So this defines a vector
 * of functions defined in a source file.
 */
#[derive(Debug, PartialEq)]
pub struct AST<'a>(pub Vec<SpanFn<'a>>);


/**
 * The different kinds of operators used by
 * both the binary and unary operations.
 */
#[derive(Debug, Clone, PartialEq)]
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
 * Type alias of operator to include span.
 */
pub type SpanOp<'a> = (Span<'a>, Op);


/**
 * The different kinds of supported types.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32,
    Bool,
}


/**
 * Type alias of the type enum to include span.
 */
pub type SpanType<'a> = (Span<'a>, Type);
