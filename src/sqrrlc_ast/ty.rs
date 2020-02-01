
/***************************************************************************
 * Type AST sub module contains the strctures used by the type system.
 ***************************************************************************/


use std::fmt;
use std::cmp;
use crate::sqrrlc_ast::span::Span;


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
        Ty{kind: TyKind::None, span: Span::new_empty()}
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
