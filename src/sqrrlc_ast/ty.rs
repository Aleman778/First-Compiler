
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
pub enum Type {
    /// The type path
    Int32{span: Span},
    Bool{span: Span},
    Reference(TypeReference),
    None,
}


/**
 * Type reference struct defines the type as a reference.
 * e.g. `&mut i32` defines a mutable i32 type reference.
 */
#[derive(Debug, Clone)]
pub struct TypeReference {
    pub mutability: bool,
    pub elem: Box<Type>,
    pub span: Span,
}


/**
 * Implementation of type enum.
 */
impl Type {
    /**
     * Returns clone of the span information.
     */
    pub fn get_span(&self) -> Span {
        match self {
            Type::Int32{span} => span.clone(),
            Type::Bool{span} => span.clone(),
            Type::Reference(reference) => reference.span.clone(),
            Type::None => Span::new_empty(),
        }
    }


    /**
     * Returns true if type is i32.
     */
    pub fn is_i32(&self) -> bool {
        match self {
            Type::Int32{span: _} => true,
            _ => false,
        }
    }

    
    /**
     * Returns true if type is i32.
     */
    pub fn is_bool(&self) -> bool {
        match self {
            Type::Bool{span: _} => true,
            _ => false,
        }
    }

    
    /**
     * Returns the type reference or None if type is not a reference.
     */
    pub fn get_ref<'a>(&'a self) -> Option<&'a TypeReference> {
        match self {
            Type::Reference(reference) => Some(reference),
            _ => None,
        }
    }
    

    /**
     * Returns true if this has no type.
     */
    pub fn is_none(&self) -> bool {
        match self {
            Type::None => true,
            _ => false,
        }
    }
}


/**
 * Implementation of display trait for retrieving the type identifier.
 */
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int32{span: _} => write!(f, "i32"),
            Type::Bool{span: _} => write!(f, "bool"),
            Type::Reference(reference) => write!(f, "{}", reference),
            Type::None => write!(f, "none"),
        }
    }
}


/**
 * Display formatting of type references.
 */
impl fmt::Display for TypeReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut prefix = String::from("&");
        if self.mutability {
            prefix.push_str("mut ");
        }
        write!(f, "{}{}", prefix, self.elem)
    }
}


/**
 * Comparing partial equality of types.
 */
impl cmp::PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match self {
            Type::Int32{span: _} => other.is_i32(),
            Type::Bool{span: _} => other.is_bool(),
            Type::Reference(self_ref) => match other.get_ref() {
                Some(other_ref) => self_ref == other_ref,
                None => false,
            },
            Type::None => other.is_none(),
        }
    }
}


/**
 * Comparing partial equality of type references.
 */
impl cmp::PartialEq for TypeReference {
    fn eq(&self, other: &TypeReference) -> bool {
        self.elem == other.elem
    }
}
