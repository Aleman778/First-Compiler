
/***************************************************************************
 * Abstract Syntax Tree implementation
 ***************************************************************************/



#[derive(Clone, Copy, Debug)]
pub enum ItemKind<'ast> {
    
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
    pub stmts: &'ast [&'ast Stmt],
    /// The last expression
    pub expr: Option<&'ast Expr>,
    /// The location of this block.
    pub span: Span,
}


/**
 * Local variable declartion defines information about
 * the variable e.g. let mut a: i32 = 53;
 */
#[derive(Clone, Debug)]
pub struct Local<'ast> {
    /// Is the local variable mutable?
    pub mutable: bool,
    /// Local variable identifier.
    pub ident: ExprIdent,
    /// Type annotation, if any was provided.
    pub ty: Option<Ty<'ast>>,
    /// Initialization expression of local variable.
    pub init: Option<&'ast Expr<'ast>>,
    /// 
    pub span: Span,
}



/**
 * The type enum contains the different types of supported types.
 * Can be an integer, boolean or reference.
 */
#[derive(Clone, Debug)]
pub struct Ty<'ast> {
    /// Kind of type annotation.
    pub kind: TyKind<'ast>,
    // Location of type declaration.
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
#[derive(Clone, Copy, Debug)]
pub enum IntTy {
    ISize,
    I8,
    I16,
    I32,
    I64,
    I128,
}


/**
 * Different kinds of unsigned integer types.
 * The number defines the number of bits.
 */
#[derive(Clone, Copy, Debug)]
pub enum UIntTy {
    USize,
    U8,
    U16,
    U32,
    U64,
    U128,
}


/**
 * Different kinds of floating point numbers.
 * There exists both 32-bit and 64-bit floating point numbers.
 * Note: f64 may also be called double in other languages.
 */
#[derive(Clone, Copy, Debug)]
pub enum FloatTy {
    F32,
    F64,
}


/**
 * An AST Node is the base object for every node in the tree.
 */
#[derive(Clone, Copy, Debug)]
pub enum Node<'ast> {
    Item(&'ast Item<'ast>),
    Stmt(&'ast Stmt<'ast>),
    Expr(&'ast Expr<'ast>),
    Block(&'ast Block<'ast>),
    Local(&'ast Local<'ast>),
    Ty(&'ast Ty<'ast>),
}
