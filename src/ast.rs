#![allow(dead_code)]

use std::{fmt, cmp};
use std::collections::HashMap;
use std::cell::RefCell;
use string_interner::{StringInterner, DefaultSymbol};
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
    pub error_count: u32
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
    pub abi: Option<String>,
    pub items: Vec<Item>,
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
    pub mutable: bool,
    pub assigned: bool,
    pub span: Span,
    pub sym: Option<Symbol>, // used in type checking 
    pub first_declared_span: Span,
    pub first_assigned_span: Span,
}

/**
 * The different kinds of types supported.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    Int,
    Bool,
    Ref(TypeRef),
    Error, // used by type checker
    None,
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
    pub fn new(kind: TyKind, span: Span) -> Self {
        Ty {
            kind,
            mutable: false,
            assigned: false,
            span,
            sym: None,
            first_declared_span: Span::new(),
            first_assigned_span: Span::new(),
        }
    }

    /**
     * Returns true if type is i32.
     */
    pub fn is_int(&self) -> bool {
        match &self.kind {
            TyKind::Int => true,
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

impl Default for Ty {
    fn default() -> Self {
        Ty {
            kind: TyKind::None,
            mutable: false,
            assigned: false,
            span: Span::new(),
            sym: None,
            first_declared_span: Span::new(),
            first_assigned_span: Span::new(),
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
            TyKind::Int => write!(f, "i32"),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Ref(r) => write!(f, "{}", r),
            TyKind::Error => write!(f, "()"),
            TyKind::None => write!(f, "()"),
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
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
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
            BinOp::And => (1, Assoc::Left),
            BinOp::Or  => (1, Assoc::Left),

            // Precedence: 2, Associativity: Left-to-right
            BinOp::Eq  => (2, Assoc::Left),
            BinOp::Ne  => (2, Assoc::Left),

            // Precedence: 3, Associativity: Left-to-right
            BinOp::Lt  => (3, Assoc::Left),
            BinOp::Le  => (3, Assoc::Left),
            BinOp::Gt  => (3, Assoc::Left),
            BinOp::Ge  => (3, Assoc::Left),

            // Precedence: 4, Associativity: Left-to-right
            BinOp::Add => (4, Assoc::Left),
            BinOp::Sub => (4, Assoc::Left),

            // Precedence: 5, Associativity: Left-to-right
            BinOp::Mul => (5, Assoc::Left),
            BinOp::Div => (5, Assoc::Left),
            BinOp::Mod => (5, Assoc::Left),

            // Precedence: 6, Associativity: Right-to-left
            BinOp::Pow => (6, Assoc::Right),

        }
    }

    /**
     * Returns the token string used by the given operator.
     */
    pub fn token(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Pow => "**",
            BinOp::Mod => "%",
            BinOp::And => "&&",
            BinOp::Or  => "||",
            BinOp::Eq  => "==",
            BinOp::Ne  => "!=",
            BinOp::Lt  => "<",
            BinOp::Le  => "<=",
            BinOp::Gt  => ">",
            BinOp::Ge  => ">=",
        }
    }
}

/**
 * Display formatting for binary oprators, displays the name of the operator.
 */
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "subtract"),
            BinOp::Mul => write!(f, "multiplicate"),
            BinOp::Div => write!(f, "divide"),
            BinOp::Pow => write!(f, "power"),
            BinOp::Mod => write!(f, "modolu"),
            BinOp::And => write!(f, "logical and"),
            BinOp::Or  => write!(f, "logical or"),
            BinOp::Eq  => write!(f, "compare equal"),
            BinOp::Ne  => write!(f, "compare not equal"),
            BinOp::Lt  => write!(f, "compare less than"),
            BinOp::Le  => write!(f, "compare less than or equal"),
            BinOp::Gt  => write!(f, "compare greater than"),
            BinOp::Ge  => write!(f, "compare greater than or equal"),
        }
    }
}

/**
 * Unary operators e.g. `-`, `!`, `*` etc.
 */
#[derive(Debug, Copy, Clone,  PartialEq)]
pub enum UnOp {
    /// The `-` unary operator (negation)
    Neg,
    /// The `!` operator (logical inversion)
    Not,
    /// The `*` operator (dereferencing)
    Deref,
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
            UnOp::Neg   => "-",
            UnOp::Not   => "!",
            UnOp::Deref => "*",
        }
    }
}

/**
 * Display formatting for unary operators, displays the name of operator.
 */
impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg   => write!(f, "negated"),
            UnOp::Not   => write!(f, "logical inverted"),
            UnOp::Deref => write!(f, "dereferenced"),
        }
    }
}

/**
 * An operator can either be left or right associative.
 */
pub enum Assoc {
    Left,
    Right,
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
    pub sym: Symbol,
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
    pub expr: Box<Expr>,
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
    Int(i32),
    /// Literal for booleans e.g. false
    Bool(bool),
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

impl Span {
    pub fn new() -> Self {
        Span {
            base: 0,
            len: 0,
            ctx: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        return self.base == 0 && self.len == 0 && self.ctx == 0;
    }

    pub fn is_inside(&self, other: Span) -> bool {
        return other.base < self.base && (other.base + other.len as u32) < (self.base + self.len as u32);
    }

    pub fn from_parse_span(s: ParseSpan) -> Self {
        Span {
            base: s.offset as u32,
            len: s.fragment.len() as u16,
            ctx: s.extra,
        }
    }

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

    pub fn between(s1: Span, s2: Span) -> Self {
        assert!(s1.ctx == s2.ctx);
        if s1.base < s2.base {
            Span {
                base: s1.base,
                len: (s2.base - s1.base) as u16,
                ctx: s1.ctx,
            }
        } else {
            Span {
                base: s2.base,
                len: (s1.base - s2.base) as u16,
                ctx: s1.ctx,
            }
        }
    }
}

pub fn get_span_location_in_file(lines: &Vec<u32>, span: Span) -> (usize, usize, usize, usize) {
    let line_number = match lines.binary_search(&span.base) {
        Ok(line) => line.saturating_sub(1),
        Err(line) => line.saturating_sub(1),
    };
    let end_byte_pos = span.base + span.len as u32;
    let end_line_number = match lines.binary_search(&end_byte_pos) {
        Ok(line) => line,
        Err(line) => line,
    };
    let line_start = lines[line_number] as usize;
    let line_end = lines[end_line_number] as usize;
    let column_number = (span.base as usize).saturating_sub(line_start);
    (line_number + 1, column_number + 1, line_start, line_end)
}

thread_local!(static GLOBAL_STRING_INTERNER: RefCell<StringInterner> =
              RefCell::new(StringInterner::default()));

/**
 * Use string interner to reduce memory footprint by storing an index to a particular
 * string rather than storing clones of the same string everywhere.
 */
pub type Symbol = DefaultSymbol;

pub fn intern_string<'a>(string: &str) -> Symbol {
    GLOBAL_STRING_INTERNER.with(|interner_cell| {
        let interner = &mut *interner_cell.borrow_mut();
        interner.get_or_intern(string)
    })
}

pub fn resolve_symbol<'a>(symbol: Symbol) -> &'static str {
    GLOBAL_STRING_INTERNER.with(|interner_cell| unsafe {
        let interner = interner_cell.borrow();
        std::mem::transmute::<&str, &'static str>(interner.resolve(symbol).unwrap())
    })
}
