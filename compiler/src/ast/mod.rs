
/***************************************************************************
 * AST module implements the Abstract Syntax Tree data structure.
 ***************************************************************************/


/**
 * Requires the LocatedSpan struct from the nom locate crate.
 */
use nom_locate::LocatedSpan;


/**
 * Type alias of LocatedSpan for convenience.
 * Every node in the AST is recommended to include this span.
 */
pub type Span<'a> = LocatedSpan<&'a str>;


/**
 * File struct is the root of the AST in a source file.
 * The file structure contains a vector of items. Currently
 * only function items are supported, but can easily be
 * extended to support any item such as structs, type alias etc.
 */
pub struct File<'a> {
    items: Vec<Item<'a>>,
    span: Span<'a>
}


/**
 * Items enum contains all types of items that appear in a file.
 * This currently only supports item functions.
 */
pub enum Item<'a> {
    ItemFn {
        ident: Ident<'a>,
        decl: FnDecl<'a>,
        block: Expr<'a>,
        span: Span<'a>,
    }
}


/**
 * Function declaration struct contains information about the
 * functions input arguments and the output type.
 */
pub struct FnDecl<'a> {
    inputs: Vec<Argument<'a>>,
    output: Type<'a>,
    span: Span<'a>,
}


/**
 * Argument struct contains an identifier and a type.
 */
pub struct Argument<'a> {
    ident: Ident<'a>,
    ty: Type<'a>,
    span: Span<'a>,
}


/**
 * Identifier struct contains a user defined name.
 */
pub struct Ident<'a> {
    to_string: &'a str,
    span: Span<'a>,
}


/**
 * Type enum currently only supports i32 and bool.
 */
pub enum Type<'a> {
    Int32 {
        span: Span<'a>,
    },
    Bool {
        span: Span<'a>,
    },
}


/**
 * Expression enum contains different kinds of expressions
 * that are supported e.g. atoms, binary operations, unary operations etc.
 */
pub enum Expr<'a> {
    /**
     * Atom is either a literal, function call, or identifier.
     * E.g. 53, min(6, 8), x.
     */
    Atom(Atom<'a>),

    
    /**
     * Binary operation has a left and right operand
     * and also the operator in between, e.g. 1 + 2.
     */
    BinOp {
        left: Box<Expr<'a>>,
        op: Op,
        right: Box<Expr<'a>>,
        span: Span<'a>,
    },


    /**
     * Unary operation has an operator to the left and
     * the operand to the right, e.g. !running.
     */
    UnOp {
        op: Op,
        right: Box<Expr<'a>>,
        span: Span<'a>,
    },

    
    /**
     * Local variable declartion defines information about
     * the variable e.g. let mut a: i32 = 53;
     */
    Local {
        mutable: bool,
        ident: Ident<'a>,
        ty: Type<'a>,
        init: Box<Expr<'a>>,
        span: Span<'a>,
    },

    
    /**
     * Assignment of mutable variable, e.g. x = 5;
     */
    Assign {
        ident: Ident<'a>,
        expr: Box<Expr<'a>>,
        span: Span<'a>,
    },


    /**
     * Block contains a vector of expressions.
     */
    Block {
        stmts: Vec<Expr<'a>>,
        span: Span<'a>,
    },


    /**
     * If statement has a condition and a block
     * that is executed if condition is true otherwise the
     * second block is optionally executed instead.
     */
    If {
        cond: Box<Expr<'a>>,
        then_block: Expr::Block<'a>,
        else_block: Option<Expr::Block<'a>>,
        span: Span<'a>,
    },


    /**
     * While loops includes a condition and a block that is
     * executed each time the condition is true.
     */
    While {
        cond: Box<Expr<'a>>,
        block: Expr::Block<'a>,
        span: Span<'a>,
    },


    /**
     * Breaks the loop.
     */
    Break {
        span: Span<'a>
    },


    /**
     * Continue to next cycle of the loop.
     */
    Continue {
        span: Span<'a>
    },
    
        
    /**
     * Return statement can optionally return an expression
     */
    Return {
        expr: Box<Option<SpanExpr<'a>>>,
        span: Span<'a>,
    },
}


/**
 * Atom enum contains different types of values used
 * in expressions e.g. integers, bools etc.
 */
pub enum Atom<'a> {
    /**
     * Literal integer struct has an i32 value.
     */
    LitInt {
        value: i32,
        span: Span<'a>,
    },


    /**
     * Literal boolean struct has a bool value.
     */
    LitBool {
        value: bool,
        span: Span<'a>,
    },


    /**
     * Function call contains the identifier and arguments.
     */
    FnCall {
        ident: Ident<'a>,
        args: Vec<Expr<'a>>,
        span: Span<'a>,
    },

    
    /**
     * Identifier.
     */
    Ident(Ident<'a>),
}


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
