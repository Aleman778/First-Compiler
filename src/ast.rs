use std::{fmt, cmp};
use std::collections::HashMap;
use crate::parser::ParseSpan;

/**
 * File struct is the root of the AST in a source file.
 * The file structure contains a vector of items. Currently
 * only function items are supported, but can easily be
 * extended to support any item such as structs, type alias etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub source: String,
    pub filename: String,
    pub items: Vec<Item>,
    pub span: Span,
    pub lines: Vec<u32>, // bytepos for each line in the file
    pub imported_files: HashMap<String, Box<File>>,
}

/**
 * Items enum contains all types of items that appear in a file.
 * This currently only supports item functions.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// Function item e.g. `fn main() { }`
    Fn(FnItem),

    /// Extern function item, defined somewhere else.
    ForeignFn(ForeignFnItem),

    /// Extern module e.g. `extern { }`
    ForeignMod(ForeignModItem),
}

/**
 * Implementation of item.
 */
impl Item {
    /**
     * Returns the identifier string of the given item.
     */
    pub fn get_id(&self) -> String {
        match self {
            Item::Fn(func) => func.ident.to_string.clone(),
            Item::ForeignFn(func) => func.ident.to_string.clone(),
            Item::ForeignMod(_) => String::new(),
        }
    }

    pub fn get_ident(&self) -> ExprIdent {
        match self {
            Item::Fn(func) => func.ident.clone(),
            Item::ForeignFn(func) => func.ident.clone(),
            Item::ForeignMod(_) => ExprIdent{
                to_string: String::new(),
                span: Span::new()
            }
        }
    }
}

/**
 * Item function struct defines the properties of a function
 * the identifier, declaration and block.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnItem {
    pub ident: ExprIdent,
    pub decl: FnDecl,
    pub block: Block,
    pub span: Span,
}

/**
 * Foreign item function struct defines the properties of a
 * foreign function, the identifier and its declaration.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ForeignFnItem {
    pub ident: ExprIdent,
    pub decl: FnDecl,
    pub span: Span,
}

/**
 * Foreign function module item defines a module containing
 * foreign functions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ForeignModItem {
    pub abi: Option<LitStr>,
    pub items: Vec<ForeignFnItem>,
    pub span: Span,
}

/**
 * Function declaration struct contains information about the
 * functions input arguments and output type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub inputs: Vec<Argument>,
    pub output: Ty,
    pub span: Span,
}

/**
 * Argument struct contains an identifier and a type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub mutable: bool,
    pub ident: ExprIdent,
    pub ty: Ty,
    pub span: Span,
}

/**
 * Block contains a vector of statements.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/**
 * Statement can be a let binding, expression with
 * or without ending semicolon token.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Let binding for local variable assignment e.g. `let a: i32 = 5;`.
    Local(Local),

    /// Item definition.
    Item(Item),

    /// Expression with a trailing semicolon.
    Semi(Expr),

    /// An expression without a trailing semicolon.
    Expr(Expr),
}

/**
 * Local variable declartion defines information about
 * the variable e.g. let mut a: i32 = 53;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub mutable: bool,
    pub ident: ExprIdent,
    pub ty: Ty,
    pub init: Box<Option<Expr>>,
    pub span: Span,
}

/**
 * The type enum contains the different types of supported types.
 * Can be an integer, boolean or reference.
 */
#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

/**
 * The different kinds of types supported.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    /// Different kinds of integer types e.g. `i32`.
    Int(IntTy),

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
 * The different kinds of integer types.
 * The number defines the number of bits.
 * Default inferred type is `i32`.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum IntTy {
    I32,
    I64,
}

/**
 * Type reference struct defines the type as a reference.
 * e.g. `&mut i32` defines a mutable i32 type reference.
 */
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub mutable: bool,
    pub elem: Box<Ty>,
}

/**
 * Implementation of type enum.
 */
impl Ty {
    /**
     * Creates an empty type with kind `TyKind::None` and empty span.
     */
    pub fn new() -> Self {
        Ty{kind: TyKind::None, span: Span::new()}
    }

    /**
     * Returns true if type is i32.
     */
    pub fn is_i32(&self) -> bool {
        match &self.kind {
            TyKind::Int(int) =>
                match int {
                    IntTy::I32 => true,
                    _ => false,
                },
            _ => false,
        }
    }

    /**
     * Returns true if type is i64.
     */
    pub fn is_i64(&self) -> bool {
        match &self.kind {
            TyKind::Int(int) =>
                match int {
                    IntTy::I64 => true,
                    _ => false,
                },
            _ => false,
        }
    }

    /**
     * Returns true if type is i32.
     */
    pub fn is_bool(&self) -> bool {
        match self.kind {
            TyKind::Bool => true,
            _ => false,
        }
    }

    /**
     * Returns true if type is ref.
     */
    pub fn is_ref(&self) -> bool {
        match self.kind {
            TyKind::Ref(_) => true,
            _ => false,
        }
    }

    /**
     * Returns the type reference or None if type is not a reference.
     */
    pub fn get_ref<'a>(&'a self) -> Option<&'a TypeRef> {
        match &self.kind {
            TyKind::Ref(r) => Some(&r),
            _ => None,
        }
    }

    /**
     * Returns true if this has no type.
     */
    pub fn is_none(&self) -> bool {
        match self.kind {
            TyKind::None => true,
            _ => false,
        }
    }
}

/**
 * Display formatting for types.
 */
impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/**
 * Dispaly formatting for the different kinds of types.
 */
impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Int(int) => write!(f, "{}", int),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Ref(r) => write!(f, "{}", r),
            TyKind::Infer => write!(f, "infer"),
            TyKind::None => write!(f, "()")
        }
    }
}

/**
 * Display formatting for integer types.
 */
impl fmt::Display for IntTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntTy::I32 => f.write_str("i32"),
            IntTy::I64 => f.write_str("i64"),
        }
    }
}

/**
 * Display formatting of type references.
 */
impl fmt::Display for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut prefix = String::from("&");
        if self.mutable {
            prefix.push_str("mut ");
        }
        write!(f, "{}{}", prefix, self.elem)
    }
}

/**
 * Comparing partial equality of types.
 */
impl cmp::PartialEq for Ty {
    fn eq(&self, other: &Ty) -> bool {
        self.kind == other.kind
    }
}

/**
 * Comparing partial equality of type references.
 */
impl cmp::PartialEq for TypeRef {
    fn eq(&self, other: &TypeRef) -> bool {
        self.elem == other.elem && self.mutable == other.mutable
    }
}

/**
 * Binary  operators e.g. `+`, `&&`, `!` etc.
 */
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add{span: Span},
    /// The `-` binary operator (subtraction)
    Sub{span: Span},
    /// The `*` operator (multiplication)
    Mul{span: Span},
    /// The `/` operator (division)
    Div{span: Span},
    /// The `**` operator (power)
    Pow{span: Span},
    /// The `%` operator (modulus)
    Mod{span: Span},
    /// The `&&` operator (logical and)
    And{span: Span},
    /// The `||` operator (logical or)
    Or{span: Span},
    /// The `=` operator (equality)
    Eq{span: Span},
    /// The `!=` operator (not equal to)
    Ne{span: Span},
    /// The `<` operator (less than)
    Lt{span: Span},
    /// The `<=` operator (less than or equal to)
    Le{span: Span},
    /// The `>` operator (greater than)
    Gt{span: Span},
    /// The `>=` operator (greater than or equal to)
    Ge{span: Span},
}

/**
 * Unary operators e.g. `-`, `!`, `*` etc.
 */
#[derive(Debug, Copy, Clone,  PartialEq)]
pub enum UnOp {
    /// The `-` unary operator (negation)
    Neg{span: Span},
    /// The `!` operator (logical inversion)
    Not{span: Span},
    /// The `*` operator (dereferencing)
    Deref{span: Span},
}

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

/**
 * Expressions enum contains all the different types of expressions,
 * e.g. binary operations, local variable assignment, atoms etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Expression for mutation for variable e.g. `a = calc()`.
    Assign(ExprAssign),

    /// Expression for binary opeations e.g. `5 + a`, `b && check()`.
    Binary(ExprBinary),

    /// Expression for block statements e.g. `{ ... }`.
    Block(ExprBlock),

    /// Expression for break statements i.e. `break;`.
    Break(ExprBreak),

    /// Expression for function calls e.g. `foo(bar)`.
    Call(ExprCall),

    /// Expression for continue statements e.g. `continue;`.
    Continue(ExprContinue),

    /// Expression for identifiers e.g. `foo`, `my_function`, `__PATH__`.
    Ident(ExprIdent),

    /// Expression for if statements e.g. `if a > 5 { a = 6; } else { a = 4; }`.
    If(ExprIf),

    /// Expression for literals e.g. `32`, `true`.
    Lit(ExprLit),

    /// Parenthesized expression e.g. `(5 + 3)`.
    Paren(ExprParen),

    /// Reference expression e.g. &342, &mut false.
    Reference(ExprReference),

    /// Expression for return statements e.g. `return true;`, `return;`.
    Return(ExprReturn),

    /// Expression for unary operations e.g. `-a`, `!is_err()`.
    Unary(ExprUnary),

    /// Expression for while statements e.g. `while true { do_something(); }`.
    While(ExprWhile),
}

impl Expr {
    /**
     * Returns the span of this expression
     */
    pub fn get_span(&self) -> Span {
        match self {
            Expr::Assign(expr)    => expr.span,
            Expr::Binary(expr)    => expr.span,
            Expr::Block(expr)     => expr.span,
            Expr::Break(expr)     => expr.span,
            Expr::Call(expr)      => expr.span,
            Expr::Continue(expr)  => expr.span,
            Expr::Ident(expr)     => expr.span,
            Expr::If(expr)        => expr.span,
            Expr::Lit(expr)       => expr.span,
            Expr::Paren(expr)     => expr.span,
            Expr::Reference(expr) => expr.span,
            Expr::Return(expr)    => expr.span,
            Expr::Unary(expr)     => expr.span,
            Expr::While(expr)     => expr.span,
            _ => Span::new(),
        }
    }
}

/**
 * Assignment of mutable variable, e.g. x = 5;
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprAssign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: Span,
}

/**
 * Binary operation has a left and right operand
 * and also the operator in between, e.g. 1 + 2.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: BinOp,
    pub right: Box<Expr>,
    pub span: Span,
}

/**
 * Block expressions used for sub block expresions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub block: Block,
    pub span: Span,
}

/**
 * Breaks the loop.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBreak {
    pub span: Span,
}

/**
 * Function call contains the identifier and arguments.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall {
    pub ident: ExprIdent,
    pub args: Vec<Expr>,
    pub span: Span,
}

/**
 * Continue to next cycle of the loop.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprContinue {
    pub span: Span,
}

/**
 * Identifier struct contains a user defined name.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprIdent {
    pub to_string: String,
    pub span: Span,
}

/**
 * If statement has a condition and a block
 * that is executed if condition is true otherwise the
 * second block is optionally executed instead.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprIf {
    pub cond: Box<Expr>,
    pub then_block: Block,
    pub else_block: Option<Block>,
    pub span: Span,
}

/**
 * Literal expression .
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprLit {
    pub lit: Lit,
    pub span: Span,
}

/**
 * Parenthesized expressions.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprParen {
    pub expr: Box<Expr>,
    pub span: Span,
}

/**
 * Reference expression.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprReference {
    pub mutable: bool,
    pub expr: Box<Expr>,
    pub span: Span,
}

/**
 * Return statement can optionally return an expression
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprReturn {
    pub expr: Box<Option<Expr>>,
    pub span: Span,
}

/**
 * Unary operation has an operator to the left and
 * the operand to the right, e.g. !running.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnOp,
    pub right: Box<Expr>,
    pub span: Span,
}

/**
 * While loops includes a condition and a block that is
 * executed each time the condition is true.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ExprWhile {
    pub cond: Box<Expr>,
    pub block: Block,
    pub span: Span,
}

/**
 * Literal enum defines different types of literals supported.
 * e.g. 5, false etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// Literal for integers e.g. 5
    Int(LitInt),
    /// Literal for booleans e.g. false
    Bool(LitBool),
    /// Literal for strings e.g. "hello"
    Str(LitStr),
}

/**
 * Literal integer struct has an i32 value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitInt {
    pub value: i32,
    pub span: Span,
}

/**
 * Literal boolean struct has a bool value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitBool {
    pub value: bool,
    pub span: Span,
}

/**
 * Literal string struct has a str value.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct LitStr {
    pub value: String,
    pub span: Span,
}

/**
 * Custom span struct only includes lines and columns from the start to
 * the end of the span location. The location is a pointer to a source file in the source mapper.
 */
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    pub base: u32,
    pub len: u16,
    pub ctx: u16,
}

/**
 * Implementation of span.
 */
impl Span {
    /**
     * Constructs an empty span.
     */
    pub fn new() -> Self {
        Span {
            base: 0,
            len: 0,
            ctx: 0,
        }
    }


    /**
     * Constructs a new span from a parse span.
     */
    pub fn from_parse_span(s: ParseSpan) -> Self {
        Span {
            base: s.offset as u32,
            len: s.fragment.len() as u16,
            ctx: s.extra,
        }
    }

    /**
     * Returns two spans combined into one,
     */
    pub fn combine(s1: Span, s2: Span) -> Self {
        assert!(s1.ctx == s2.ctx);
        if s1.base < s2.base {
            Span {
                base: s1.base,
                len: (s2.base - s1.base) as u16 + s2.len,
                ctx: s1.ctx,
            }
        } else {
            Span {
                base: s2.base,
                len: (s1.base - s2.base) as u16 + s1.len,
                ctx: s1.ctx,
            }
        }
    }
}
