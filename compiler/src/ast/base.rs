#![allow(dead_code)]

/***************************************************************************
 * Base AST sub module contains the base structures such as the file
 * and the item contents of a file e.g. functions and their respective
 * declarations. This module also 
 ***************************************************************************/


/**
 * Requires parts from other parts in the ast module.
 */
use crate::ast::{
    span::Span,
    atom::Ident,
    expr::ExprBlock,
};


/**
 * File struct is the root of the AST in a source file.
 * The file structure contains a vector of items. Currently
 * only function items are supported, but can easily be
 * extended to support any item such as structs, type alias etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    items: Vec<Item>,
    span: Span
}


/**
 * Items enum contains all types of items that appear in a file.
 * This currently only supports item functions.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    ItemFn(ItemFn),
}


/**
 * Item function struct defines the properties of a function
 * the identifier, declaration and block.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ItemFn {
    ident: Ident,
    decl: FnDecl,
    block: ExprBlock,
    span: Span,
}


/**
 * Function declaration struct contains information about the
 * functions input arguments and output type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    inputs: Vec<Argument>,
    output: Type,
    span: Span,
}


/**
 * Argument struct contains an identifier and a type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    ident: Ident,
    ty: Type,
    span: Span,
}


/**
 * Type enum currently only supports i32 and bool.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32(Int32),
    Bool(Bool)
}


/**
 * 32 bit signed integer type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Int32  {
    span: Span,
}


/**
 * Boolean type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Bool {
    span: Span,
}
