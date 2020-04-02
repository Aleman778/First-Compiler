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
#[derive(Clone, Debug)]
pub struct Node {
    /// Id of the parent node.
    pub parent: NodeId,
    /// Specific kind of node.
    pub kind: NodeKind,
    /// Location of this node.
    pub span: Span,
}


impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
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
#[derive(Clone, Debug)]
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


impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind 
            && self.ident == other.ident 
            && self.vis == other.vis
    }
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
    /// Structs are defined by a list of fields.
    Struct(Option<Vec<Box<Field>>>, Vec<Box<StructField>>),
    /// Enums are defined by list of fields with values and common type.
    Enum(Option<Box<Ty>>, Vec<Box<EnumField>>),
}


/**
 * Visibility of a specific item.
 */
#[derive(Clone, Debug)]
pub struct Visibility {
    /// Kind of visibility
    pub kind: VisibilityKind,
    /// Location of visibility notation.
    pub span: Span,
}


impl PartialEq for Visibility {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
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
#[derive(Clone, Debug)]
pub struct FnSig {
    /// Input arguments to function.
    pub inputs: Vec<Box<FnArg>>,
    /// Return type of function.
    pub output: Option<Box<Ty>>,
    /// Location of function signature.
    pub span: Span,
}



impl PartialEq for FnSig {
    fn eq(&self, other: &Self) -> bool {
        self.inputs == other.inputs
            && self.output == other.output
    }
}


/**
 * Function argument is defined by an identifier and type.
 * It is also valid for the last arguments to 
 */
#[derive(Clone, Debug)]
pub struct FnArg {
    /// Identifier of the function argument.
    pub ident: Ident,
    /// The type of the function argument.
    pub ty: Box<Ty>,
    /// Location of the function argument.
    pub span: Span,
}


impl PartialEq for FnArg {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
            && self.ty == other.ty
    }
}


/**
 * Blocks contains a vector of statements.
 * Statements can  be local variable, expressions and items.
 * Mostly statements ends with a semicolon.
 */
#[derive(Clone, Debug)]
pub struct Stmt {
    /// Kind of statement.
    pub kind: StmtKind,
    /// Location of statement.
    pub span: Span,
}


impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
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
#[derive(Clone, Debug)]
pub struct Block {
    /// Statements in the block.
    pub stmts: Vec<Box<Stmt>>,
    /// The last expression, implicit returns.
    pub expr: Option<Box<Expr>>,
    /// The location of this block.
    pub span: Span,
}


impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.stmts == other.stmts
            && self.expr == other.expr
    }
}



/**
 * Local variable declartion defines information about
 * the variable e.g. `let mut a: i32 = 53;`
 */
#[derive(Clone, Debug)]
pub struct Local {
    /// Local variable identifier.
    pub ident: Ident,
    /// Type annotation, if any was provided.
    pub ty: Option<Box<Ty>>,
    /// Initialization expression of local variable.
    pub init: Option<Box<Expr>>,
    /// Location of local variable declaration.
    pub span: Span,
}


impl PartialEq for Local {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
            && self.ty == other.ty
            && self.init == other.init
    }
}


/**
 * All nodes are expressions but exepct for two exceptions
 * those are items and local variables.
 */
#[derive(Clone, Debug)]
pub struct Expr {
    /// The expression identifier.
    pub node_id: NodeId,
    /// Kind of expression.
    pub kind: ExprKind,
    /// Location of expression.
    pub span: Span,
}


impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    /// Arrays e.g. `[1, 3, 4]`.
    Array(Vec<Box<Expr>>),

    /// Assigning value to variable e.g. `a = calc()`.
    Assign(Box<Expr>, Box<Expr>),

    /// Assigning value with operator to variable e.g. `a += 5`.
    AssignOp(BinOp, Box<Expr>, Box<Expr>),
    
    /// Binary opeations e.g. `5 + a`, `b && check()`.
    Binary(BinOp, Box<Expr>, Box<Expr>),

    /// Block statements e.g. `{ ... }`.
    Block(Box<Block>),
    
    /// Break statements i.e. `break;`.
    Break,
    
    /// Function calls e.g. `foo(bar)`.
    Call(Ident, Vec<Box<Expr>>),

    /// Type casting e.g. `32 as u8`.
    Cast(Box<Expr>, Box<Ty>),
    
    /// Continue statements e.g. `continue`.
    Continue,

    /// Field access expression e,g, `x.y`.
    Field(Box<Expr>, Ident),

    /// For loop e,g, `for i in 0..10 { do_something(); }`
    For(Ident, Box<Expr>, Box<Expr>),
    
    /// Identifiers e.g. `foo`, `my_function`, `__PATH__`.
    Ident(Ident),
    
    /// If expression e.g. `if a > 5 { a = 6; } else { a = 4; }`.
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),

    /// Array index e.g. `my_array[10]`, `my_array[func()]`.
    Index(Box<Expr>, Box<Expr>),
    
    /// Literals e.g. `32`, `true`.
    Lit(Lit),

    /// Loop statement e.g. `loop { do_something(); }`.
    Loop(Box<Expr>),
        
    /// Parenthesized expression e.g. `(5 + 3)`.
    Paren(Box<Expr>),

    /// Pointer expression e.g. *342, *test().
    Pointer(Box<Expr>),

    /// Struct expression e.g. `MyStruct { x: 5, y: 10 }`.
    Struct(Ident, Vec<Box<Field>>),

    /// Range expression e.g. `2..5` (excluding 5), `2..=5` (including 5).
    Range(Option<Box<Expr>>, Option<Box<Expr>>, RangeEnd),
    
    /// Return expression e.g. `return true;`, `return;`.
    Return(Option<Box<Expr>>),

    /// Tuple expression e.g. `(5, 3, 6)`, 
    Tuple(Vec<Box<Expr>>),
    
    /// Unary operations e.g. `-a`, `!is_err()`.
    Unary(UnOp, Box<Expr>),
    
    /// While loop e.g. `while true { do_something(); }`.
    While(Box<Expr>, Box<Expr>),
}

/**
 * Should the last value be included in the range
 * or excluded. Included is marked using `..=` before right expression.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum RangeEnd {
    Included,
    Excluded,
}


#[derive(Clone, Debug)]
pub struct Ident {
    /// Symbol representing this identifier.
    pub symbol: Symbol,
    /// Location of identifier.
    pub span: Span,
}


impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}


#[derive(Clone, Debug)]
pub struct Field {
    /// Identifier key of this field.
    pub key: Ident,
    /// Optionally the type stored in this field.
    pub value: Box<Expr>,
    /// Location of field.
    pub span: Span,
}


impl PartialEq for Field {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
            && self.value == other.value
    }
}


#[derive(Clone, Debug)]
pub struct StructField {
    /// Identifier key of this field.
    pub key: Ident,
    /// Struct fields requires type annotations.
    pub ty: Box<Ty>,
    /// Optionally the type stored in this field.
    pub value: Option<Box<Expr>>,
    /// Location of field.
    pub span: Span,
}


impl PartialEq for StructField {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
            && self.value == other.value
    }
}


#[derive(Clone, Debug)]
pub struct EnumField {
    /// Identifier key of this field.
    pub key: Ident,
    /// Optionally the type stored in this field.
    pub value: Option<Box<Expr>>,
    /// Location of field.
    pub span: Span,
}


impl PartialEq for EnumField {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
            && self.value == other.value
    }
}


/**
 * The type enum contains the different types of supported types.
 * Can be an integer, boolean or reference.
 */
#[derive(Clone, Debug)]
pub struct Ty {
    /// Kind of type annotation.
    pub kind: TyKind,
    /// Location of type declaration.
    pub span: Span,
}


impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
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
    Float(FloatTy),
    /// String type defined by `str`.
    Str,
    /// Character type defined by `char`.
    Char,
    /// Boolean type defined by `bool`.
    Bool,
    /// Custom type is defined by an identifier e.g. `MyStruct`.
    Custom(Ident),
    /// Reference type defined by `&i32`.
    Pointer(Box<Ty>),
    /// Array type declartion e.g. `[N] i32`.
    Array(Option<Box<Expr>>, Box<Ty>),
    /// Tuple type is defined by mutliple different types e.g. `(i32, str)`.
    Tuple(Vec<Box<Ty>>),
    /// Tuple type is defined by mutliple different types e.g. `(i32, str)`.
    FnSig(Vec<Box<Ty>>, Box<Ty>),
    /// Polymorphic type defined by either `$T` or `$T/MyStruct`.
    Polymorphic(Ident, Option<Box<Ty>>),
    /// Infer means that no specific type was given and should infer to something.
    Infer,
    /// This type has no type, used for functions that does not return anything, written `()`.
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


#[derive(Clone, Debug)]
pub struct Lit {
    /// Kind of literal.
    pub kind: LitKind,
    /// Location of literal.
    pub span: Span,
}


impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
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
    /// The `*` operator (dereferencing)
    Deref(Span),
}
