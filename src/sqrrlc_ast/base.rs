#![allow(dead_code)]

/***************************************************************************
 * Base AST sub module contains the base structures such as the file
 * and the item contents of a file e.g. functions and their respective
 * declarations. This module also 
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    expr::{ExprIdent, ExprBlock},
    ty::Type,
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
 * Implementation of the file struct.
 */
impl File {
    /**
     * Extend the file with more items.
     */
    pub fn extend(&mut self, items: Vec<Item>) {
        self.items.extend(items.iter().cloned());
    }
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
        }
    }

    
    pub fn get_ident(&self) -> ExprIdent {
        match self {
            Item::Fn(func) => func.ident.clone(),
            Item::ForeignFn(func) => func.ident.clone(),
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
