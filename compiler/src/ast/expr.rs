
/***************************************************************************
 * Expressions can be really anything from simple arithmetic expressions
 * to if statements and blocks.
 ***************************************************************************/


/**
 * Expressions enum contains all the different types of expressions,
 * e.g. binary operations, local variable assignment, atoms etc.
 */
pub enum Expr<'a> {
    BinOp(BinOp<'a>),
    UnOp(UnOp<'a>),
    Local(Local<'a>),
    Assign(Assign<'a>),
    Block(Block<'a>),
    If(If<'a>),
    While(While<'a>),
    Return(Return<'s>),
    Break(Break<'a>),
    Continue(Continue<'a>),
    Atom(Atom<'a>),
}


/**
 * Binary operation has a left and right operand
 * and also the operator in between, e.g. 1 + 2.
 */
pub struct BinOp<'a> {
    left: Box<Atom<'a>>,
    op: Op,
    right: Box<Expr<'a>>,
    span: Span<'a>,
}


/**
 * Unary operation has an operator to the left and
 * the operand to the right, e.g. !running.
 */
pub struct UnOp<'a> {
    op: Op,
    right: Box<Expr<'a>>,
    span: Span<'a>,
}


/**
 * Local variable declartion defines information about
 * the variable e.g. let mut a: i32 = 53;
 */
pub struct Local<'a> {
    mutable: bool,
    ident: Ident<'a>,
    ty: Type<'a>,
    init: Box<Expr<'a>>,
    span: Span<'a>,
}


/**
 * Assignment of mutable variable, e.g. x = 5;
 */
pub struct Assign<'a> {
    ident: Ident<'a>,
    expr: Box<Expr<'a>>,
    span: Span<'a>,
}


/**
 * Block contains a vector of expressions.
 */
pub struct Block<'a> {
    stmts: Vec<Expr<'a>>,
    span: Span<'a>,
}


/**
 * If statement has a condition and a block
 * that is executed if condition is true otherwise the
 * second block is optionally executed instead.
 */
pub struct If<'a> {
    cond: Box<Atom<'a>>,
    then_block: Block<'a>,
    else_block: Option<Block<'a>>,
    span: Span<'a>,
}


/**
 * While loops includes a condition and a block that is
 * executed each time the condition is true.
 */
pub struct While {
    cond: Box<Expr<'a>>,
    block: Block<'a>,
    span: Span<'a>,
}


/**
 * Return statement can optionally return an expression
 */
pub struct Return {
    expr: Option<Box<Expr<'a>>>,
    span: Span<'a>,
}


/**
 * Breaks the loop.
 */
pub struct Break {
    span: Span<'a>
}


/**
 * Continue to next cycle of the loop.
 */
pub struct Continue {
    span: Span<'a>
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
