#![allow(dead_code)]

/***************************************************************************
 * Base AST sub module contains the base structures such as the file
 * and the item contents of a file e.g. functions and their respective
 * declarations. This module also 
 ***************************************************************************/

use std::fmt;
use crate::ast::{
    span::Span,
    expr::{ExprIdent, ExprBlock},
};


/**
 * File struct is the root of the AST in a source file.
 * The file structure contains a vector of items. Currently
 * only function items are supported, but can easily be
 * extended to support any item such as structs, type alias etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub items: Vec<Item>,
    pub span: Span
}


/**
 * Items enum contains all types of items that appear in a file.
 * This currently only supports item functions.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Fn(FnItem),
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
    pub block: ExprBlock,
    pub span: Span,
}


/**
 * Function declaration struct contains information about the
 * functions input arguments and output type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub inputs: Vec<Argument>,
    pub output: Option<Type>,
    pub span: Span,
}


/**
 * Argument struct contains an identifier and a type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub ident: ExprIdent,
    pub ty: Type,
    pub span: Span,
}


/**
 * Type enum currently only supports i32 and bool.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32{span: Span},
    Bool{span: Span},
}


/**
 * Implementation of type enum.
 */
impl Type {
    /**
     * Returns the span information from the given type.
     */
    pub fn get_span(&self) -> Span {
        match self {
            Type::Int32{span} => span.clone(),
            Type::Bool{span} => span.clone(),
        }
    }
}


/**
 * Implementation of display trait for retriving the type identifier.
 */
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int32{span: _} => write!(f, "i32"),
            Type::Bool{span: _} => write!(f, "bool"),
        }
    }
}
