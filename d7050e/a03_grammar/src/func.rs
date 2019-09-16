use crate::expr::{Expr, Type};

/**
 * Function declaration struct contains information
 * about a function i.e. its name, arguments and return type.
 */
#[derive(Debug, PartialEq)]
pub struct FuncDecl {
    ident: Expr,
    arguments: Vec<FuncArg>,
    ret_type: Option<Type>,
}


/**
 * Function argument struct contains information
 * about an argument i.e. its name and type.
 */
#[derive(Debug, PartialEq)]
pub struct FuncArg {
    ident: Expr,
    ty: Type
}
