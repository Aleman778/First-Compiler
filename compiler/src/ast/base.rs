
/***************************************************************************
 * Base AST sub module contains the base structures such as the file
 * and the item contents of a file e.g. functions and their respective
 * declarations. This module also 
 ***************************************************************************/


/**
 * Requires parts from other parts in the ast module.
 */
use crate::ast::{
    Span,
    atom::Ident,
    expr::Block,
};


/**
 * File struct is the root of the AST in a source file.
 * The file structure contains a vector of items. Currently
 * only function items are supported, but can easily be
 * extended to support any item such as structs, type alias etc.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct File<'a> {
    items: Vec<Item<'a>>,
    span: Span<'a>
}


/**
 * Items enum contains all types of items that appear in a file.
 * This currently only supports item functions.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Item<'a> {
    ItemFn(ItemFn<'a>),
}


/**
 * Item function struct defines the properties of a function
 * the identifier, declaration and block.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct ItemFn<'a> {
    ident: Ident<'a>,
    decl: FnDecl<'a>,
    block: Block<'a>,
    span: Span<'a>,
}


/**
 * Function declaration struct contains information about the
 * functions input arguments and output type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl<'a> {
    inputs: Vec<Argument<'a>>,
    output: Type<'a>,
    span: Span<'a>,
}


/**
 * Argument struct contains an identifier and a type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Argument<'a> {
    ident: Ident<'a>,
    ty: Type<'a>,
    span: Span<'a>,
}


/**
 * Type enum currently only supports i32 and bool.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Int32(Int32<'a>),
    Bool(Bool<'a>)
}


/**
 * 32 bit signed integer type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Int32 <'a> {
    span: Span<'a>,
}


/**
 * Boolean type.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Bool<'a> {
    span: Span<'a>,
}
