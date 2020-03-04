//! Implementation of Abstract Syntax Tree data structure.
//! The root of the tree is not defined as a node but rather
//! it is represented as a map that maps ids to nodes.


pub mod map;
pub mod op;


use crate::span::{symbol::Symbol, Span};


/**
 * Every node in the AST can be identified using this NodeId.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeId(pub u32);


/**
 * Node is a general definintion of an AST node.
 * A node can be for example an item, statement, expression etc.
 * Every node has a parent node except for the root.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    /// Id of the parent node.
    pub parent: NodeId,
    /// Specific kind of node.
    pub kind: NodeKind,
    /// Location of this node.
    pub span: Span,
}


/**
 * An AST Node is base object for every node in the tree.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    /// Item node defines e.g. functions, structs etc.
    Item(Box<Item>),
    /// Statements is base type of AST but most statements are expressions.
    Stmt(Box<Stmt>),
    /// Almost everything is expressions, except local variables and items.
    Expr(Box<Expr>),
    /// Block contains list of statements.
    Block(Box<Block>),
    /// Local variable declaration.
    Local(Box<Local>),
    /// Type declaration.
    Ty(Box<Ty>),
}


/**
 * Items are usually everything defined in global scope.
 * However you can have nested items e.g. `fn test() { fn test2() { ... } ... }`
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    /// Identifier of this item.
    pub ident: Ident,
    /// The nodes identifier.
    pub node_id: NodeId,
    /// Kind of item.
    pub kind: ItemKind,
    /// The visibility of the item.
    pub vis: Visibility,
    /// Location of item.
    pub span: Span,
}


/**
 * Different kinds of items available.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    /// Function are defined by a signature and the block code.
    Fn(FnSig, Box<Block>),
    /// Foreign functions are defined only with a signature, code resides elsewhere.
    ForeignFn(FnSig),
}


/**
 * Visibility of a specific item.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Visibility {
    /// Kind of visibility
    pub kind: VisibilityKind,
    /// Location of visibility notation.
    pub span: Span,
}


/**
 * Different kinds of outside visibility for items.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum VisibilityKind {
    /// Items are accessable from anywhere (default).
    Visible,
    /// Hidden items are not accessible outside its scope.
    Hidden,
}


/**
 * Function signature defines the attributes
 * of a given function e.g. input/ output types.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct FnSig {
    /// Input arguments to function.
    pub inputs: Vec<Box<Ty>>,
    /// Return type of function.
    pub output: Box<Ty>,
}


/**
 * Blocks contains a vector of statements.
 * Statements can be local variable, expressions and items.
 * Mostly statements ends with a semicolon.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Stmt {
    /// Kind of statement.
    pub kind: StmtKind,
    /// Location of statement.
    pub span: Span,
}


/**
 * Statement can be a let binding, expression with
 * or without ending semicolon token.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind {
    /// Let binding for local variable assignment e.g. `let a: i32 = 5;`.
    Local(Box<Local>),

    /// Item definition.
    Item(Box<Item>),

    /// Expression with a trailing semicolon.
    Semi(Box<Expr>),
    
    /// An expression without a trailing semicolon.
    Expr(Box<Expr>),
}


/**
 * Block contains a vector of statements.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    /// Statements in the block.
    pub stmts: Vec<Box<Stmt>>,
    /// The last expression, implicit returns.
    pub expr: Option<Box<Expr>>,
    /// The location of this block.
    pub span: Span,
}


/**
 * Local variable declartion defines information about
 * the variable e.g. `let mut a: i32 = 53;`
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Local {
    /// Is the local variable mutable?
    pub mutable: bool,
    /// Local variable identifier.
    pub ident: Ident,
    /// Type annotation, if any was provided.
    pub ty: Option<Box<Ty>>,
    /// Initialization expression of local variable.
    pub init: Option<Box<Expr>>,
    /// Location of local variable declaration.
    pub span: Span,
}


/**
 * All nodes are expressions but exepct for two exceptions
 * those are items and local variables.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    /// The expression identifier.
    pub node_id: NodeId,
    /// Kind of expression.
    pub kind: ExprKind,
    /// Location of expression.
    pub span: Span,
}


#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    /// Expression for mutation for variable e.g. `a = calc()`.
    Assign(Box<Expr>, Box<Expr>),
    
    /// Expression for binary opeations e.g. `5 + a`, `b && check()`.
    Binary(BinOp, Box<Expr>, Box<Expr>),

    /// Expression for block statements e.g. `{ ... }`.
    Block(Box<Block>),
    
    /// Expression for break statements i.e. `break;`.
    Break,
    
    /// Expression for function calls e.g. `foo(bar)`.
    Call(Box<Expr>, Vec<Box<Expr>>),
    
    /// Expression for continue statements e.g. `continue;`.
    Continue,
    
    /// Expression for identifiers e.g. `foo`, `my_function`, `__PATH__`.
    Ident(Box<Ident>),
    
    /// Expression for if statements e.g. `if a > 5 { a = 6; } else { a = 4; }`.
    If(Box<Expr>, Box<Block>, Box<Expr>),
    
    /// Expression for literals e.g. `32`, `true`.
    Lit(Box<Lit>),
        
    /// Parenthesized expression e.g. `(5 + 3)`.
    Paren(Box<Expr>),

    /// Reference expression e.g. &342, &mut false.
    Reference(Box<Expr>),
    
    /// Expression for return statements e.g. `return true;`, `return;`.
    Return(Box<Expr>),
    
    /// Expression for unary operations e.g. `-a`, `!is_err()`.
    Unary(UnOp, Box<Expr>),
    
    /// Expression for while statements e.g. `while true { do_something(); }`.
    While(Box<Expr>, Box<Block>),
}



#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}


/**
 * The type enum contains the different types of supported types.
 * Can be an integer, boolean or reference.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct Ty {
    /// Kind of type annotation.
    pub kind: TyKind,
    /// Location of type declaration.
    pub span: Span,
}


/**
 * Type reference struct defines the type as a reference.
 * e.g. `&mut i32` defines a mutable i32 type reference.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct TypeRef {
    /// Mutable reference flag.
    pub mutable: bool,
    /// Type element.
    pub elem: Box<Ty>,
    /// Location of type reference declaration.
    pub span: Span,
}


/**
 * The different kinds of types supported.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum TyKind {
    /// Different kinds of signed integer types e.g. `i32`, `i8`.
    Int(IntTy),
    /// Different kinds of unsigned integer types e.g. `u32`, `u8`.
    UInt(UIntTy),
    /// Floating point types e.g. `f32` or `f64`.
    Float(LitFloatTy),
    /// Boolean type defined by `bool`.
    Bool,
    /// Type reference is defined by `&` and another type.
    Ref(TypeRef),
    /// Infer means that no specific type was given and should infer to something.
    Infer,
    /// This type has no type, used for functions that does not return anything.
    None,
}


/**
 * Different kinds of signed integer types.
 * The number defines the number of bits.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
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
#[derive(Clone, Copy, Debug, PartialEq)]
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
 * Different integer types, either implicity
 * signed and unsigned integers or unsuffixed integers.
 * Unsuffixed will infere to correct int type.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LitIntTy {
    /// Signed integer type e.g. `42_i32`.
    Signed(IntTy),
    /// Unsigned integer type e.g. `42_u32`
    Unsigned(UIntTy),
    /// Unsuffixed integer type e.g. `42`
    Unsuffixed,
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LitFloatTy {
    /// Suffixed float e.g. `32.55_f32`.
    Suffixed(FloatTy),
    /// Unsuffixed float e.g. `32.55`.
    Unsuffixed,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Lit {
    /// Kind of literal.
    pub kind: LitKind,
    /// Location of literal.
    pub span: Span,
}


/**
 * Different kinds of literals.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum LitKind {
    /// String literal e.g. `"foo"` or `r#"foo"#`.
    Str(Symbol, StrKind),
    /// Byte literal e.g. `b'f'`.
    Byte(u8),
    /// Byte  string literal e.g. `b"test"`, `br#"test"#`, 
    ByteStr(Vec<u8>, StrKind),
    /// Char literal e.g. `'a'`.
    Char(char),
    /// Int literal e.g. `52`.
    Int(u128, LitIntTy),
    /// Float literal e.g. `125.99`.
    Float(f64, LitFloatTy),
    /// Boolean literals e.g. `false`.
    Bool(bool),
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StrKind {
    /// Normal string kind e.g. `"hello world"`.
    Normal,
    /// Raw string can contain symbol `"` and is defined as
    /// `r##"Hello world "# number one?""##` <==> `Hello world "# number one?"`.
    Raw(u16),
}


/**
 * Different kinds of floating point numbers.
 * There exists both 32-bit and 64-bit floating point numbers.
 * Note: f64 may also be called double in other languages.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FloatTy {
    /// 32-bit floating point number (a.k.a. float).
    F32,
    /// 64-bit floating point number (a.k.a. double).
    F64,
}


/**
 * Binary  operators e.g. `+`, `&&`, `!` etc.
 */
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinOp {
    /// The `+` operator for addition.
    Add(Span),
    /// The `-` operator for subtraction.
    Sub(Span),
    /// The `*` operator for multiplication.
    Mul(Span),
    /// The `/` operator for division.
    Div(Span),
    /// The `**` operator for power.
    Pow(Span),
    /// The `%` operator for remainder.
    Mod(Span),
    /// The `&` operator for bitwise and.
    BitAnd(Span),
    /// The `&&` operator for logical and.
    And(Span),
    /// The `&` operator for bitwise or.
    BitOr(Span),
    /// The `||` operator for logical or.
    Or(Span),
    /// The `^` operator for bitwise xor.
    BitXor(Span),
    /// The `<<` operator for shift left.
    Shl(Span),
    /// The `>>` operator for shift right.
    Shr(Span),
    /// The `=` operator for assignments.
    Assign(Span),
    /// The `==` operator for equality.
    Eq(Span),
    /// The `!=` operator for not equal to.
    Ne(Span),
    /// The `<` operator for less than.
    Lt(Span),
    /// The `<=` operator for less than or equal to.
    Le(Span),
    /// The `>` operator for greater than.
    Gt(Span),
    /// The `>=` operator for greater than or equal to.
    Ge(Span),
    /// The `+=` operator for addition and assignment.
    AddEq(Span),
    /// The `-=` operator for subtraction and assignment.
    SubEq(Span),
    /// The `*=` operator for subtraction and assignment.
    MulEq(Span),
    /// The `/=` operator for subtraction and assignment.
    DivEq(Span),
    /// The `%=` operator for remainder and assignment.
    ModEq(Span),
    /// The `&=` operator for bitwise and with assignment.
    BitAndEq(Span),
    /// The `|=` operator for bitwise or and assignment.
    BitOrEq(Span),
    /// The `^=` operator for bitwise xor and assignment.
    BitXorEq(Span),
    /// The `<<=` operator for shift left and assignment.
    ShlEq(Span),
    /// The `>>=` operator for shift right and assignment.
    ShrEq(Span),
}


/**
 * Unary operators e.g. `-`, `!`, `*` etc.
 */
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnOp {
    /// The `-` operator (negation)
    Neg(Span),
    /// The `!` operator (logical inversion)
    Not(Span),
    /// The `^` operator for pointers.
    Ptr(Span),
    /// The `*` operator (dereferencing)
    Deref(Span),
}
