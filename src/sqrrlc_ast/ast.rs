
/***************************************************************************
 * Abstract Syntax Tree implementation
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    op::*,
};


#[derive(Clone, Debug)]
pub struct Item {
    /// Kind of item.
    pub kind: ItemKind,
    /// Location of item.
    pub span: Span,
}


#[derive(Clone, Copy, Debug)]
pub enum ItemKind<'ast> {
    Fn(FnSig)
}


#[derive(Clone, Debug)]
pub struct Stmt<'ast> {
    pub kind: StmtKind<'ast>,
    pub span: Span,
}


/**
 * Statement can be a let binding, expression with
 * or without ending semicolon token.
 */
#[derive(Clone, Copy, Debug)]
pub enum StmtKind<'ast> {
    /// Let binding for local variable assignment e.g. `let a: i32 = 5;`.
    Local(&'ast Local),

    /// Item definition.
    Item(&'ast Item),

    /// Expression with a trailing semicolon.
    Semi(&'ast Expr),
    
    /// An expression without a trailing semicolon.
    Expr(&'ast Expr),
}


/**
 * Block contains a vector of statements.
 */
#[derive(Clone, Debug)]
pub struct Block<'ast> {
    /// Statements in the block.
    pub stmts: &'ast [&'ast Stmt<'ast>],
    /// The last expression, implicit returns.
    pub expr: Option<&'ast Expr>,
    /// The location of this block.
    pub span: Span,
}


/**
 * Local variable declartion defines information about
 * the variable e.g. `let mut a: i32 = 53;`
 */
#[derive(Clone, Debug)]
pub struct Local<'ast> {
    /// Is the local variable mutable?
    pub mutable: bool,
    /// Local variable identifier.
    pub ident: Ident,
    /// Type annotation, if any was provided.
    pub ty: Option<Ty<'ast>>,
    /// Initialization expression of local variable.
    pub init: Option<&'ast Expr<'ast>>,
    /// Location of local variable declaration.
    pub span: Span,
}


/**
 * All nodes are expressions but exepct for two exceptions
 * those are items and local variables.
 */
#[derive(Clone, Debug)]
pub struct Expr<'ast> {
    /// The expression identifier.
    pub ast_id: AstId,
    /// Kind of expression.
    pub kind: ExprKind,
    /// Location of expression.
    pub span: Span,
}


#[derive(Clone, Copy, Debug)]
enum ExprKind<'ast> {
       /// Expression for mutation for variable e.g. `a = calc()`.
    Assign(&'ast Expr<'ast>, &'ast Expr<'ast>),
    
    /// Expression for binary opeations e.g. `5 + a`, `b && check()`.
    Binary(BinOp, &'ast Expr<'ast>, &'ast Expr<'ast>),

    /// Expression for block statements e.g. `{ ... }`.
    Block(&'ast Block<'ast>),
    
    /// Expression for break statements i.e. `break;`.
    Break,
    
    /// Expression for function calls e.g. `foo(bar)`.
    Call(&'ast Expr<'ast>, &'ast [&'ast Expr<'ast>]),
    
    /// Expression for continue statements e.g. `continue;`.
    Continue(),
    
    /// Expression for identifiers e.g. `foo`, `my_function`, `__PATH__`.
    Ident(),
    
    /// Expression for if statements e.g. `if a > 5 { a = 6; } else { a = 4; }`.
    If(),
    
    /// Expression for literals e.g. `32`, `true`.
    Lit(),
        
    /// Parenthesized expression e.g. `(5 + 3)`.
    Paren(),

    /// Reference expression e.g. &342, &mut false.
    Reference(),
    
    /// Expression for return statements e.g. `return true;`, `return;`.
    Return(),
    
    /// Expression for unary operations e.g. `-a`, `!is_err()`.
    Unary(),
    
    /// Expression for while statements e.g. `while true { do_something(); }`.
    While(), 
}



pub struct Ident {

}


/**
 * The type enum contains the different types of supported types.
 * Can be an integer, boolean or reference.
 */
#[derive(Clone, Debug)]
pub struct Ty<'ast> {
    /// Kind of type annotation.
    pub kind: TyKind<'ast>,
    /// Location of type declaration.
    pub span: Span,
}


/**
 * Type reference struct defines the type as a reference.
 * e.g. `&mut i32` defines a mutable i32 type reference.
 */
#[derive(Debug, Clone)]
pub struct TypeRef<'ast>{
    /// Mutable reference flag.
    pub mutable: bool,
    /// Type element 
    pub elem: Ty<'ast>,
    /// Location of type reference declaration.
    pub span: Span,
}


/**
 * The different kinds of types supported.
 */
#[derive(Clone, Copy, Debug)]
pub enum TyKind<'ast> {
    /// Different kinds of integer types e.g. `i32`.
    Int(IntTy),
    ///
    Float(FloatTy),
    /// Boolean type defined by `bool`.
    Bool,
    /// Type reference is defined by `&` and another type.
    Ref(TypeRef<'ast>),
    /// Infer means that no specific type was given and should infer to something.
    Infer,
    /// This type has no type, used for functions that does not return anything.
    None,
}


/**
 * Different kinds of signed integer types.
 * The number defines the number of bits.
 */
#[derive(Clone, Copy, Debug)]
pub enum IntTy {
    /// 32-/ 64-bit signed integer.
    ISize,
    /// 8-bit signed integer.
    I8,
    /// 16-bit signed integer.
    I16,
    /// 32-bit signed integer.
    I32,
    /// 64-bit signed integer.
    I64,
    /// 128-bit signed integer.
    I128,
}


/**
 * Different kinds of unsigned integer types.
 * The number defines the number of bits.
 */
#[derive(Clone, Copy, Debug)]
pub enum UIntTy {
    /// 32-/ 64-bit unsigned integer.
    USize,
    /// 8-bit unsigned integer.
    U8,
    /// 16-bit unsigned integer.
    U16,
    /// 32-bit unsigned integer.
    U32,
    /// 64-bit unsigned integer.
    U64,
    /// 128-bit unsigned integer.
    U128,
}


/**
 * Different kinds of floating point numbers.
 * There exists both 32-bit and 64-bit floating point numbers.
 * Note: f64 may also be called double in other languages.
 */
#[derive(Clone, Copy, Debug)]
pub enum FloatTy {
    /// 32-bit floating point number (a.k.a. float).
    F32,
    /// 64-bit floating point number (a.k.a. double).
    F64,
}


/**
 * An AST Node is the base object for every node in the tree.
 */
#[derive(Clone, Copy, Debug)]
pub enum Node<'ast> {
    /// Item node defines e.g. functions, structs etc.
    Item(&'ast Item<'ast>),
    /// Statements are usually 
    Stmt(&'ast Stmt<'ast>),
    Expr(&'ast Expr<'ast>),
    Block(&'ast Block<'ast>),
    Local(&'ast Local<'ast>),
    Ty(&'ast Ty<'ast>),
}
