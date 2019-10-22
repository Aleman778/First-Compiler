
/***************************************************************************
 * Primitive data type implementation for boolean
 ***************************************************************************/


use crate::ast::span::Span;
// use crate::interp::value::Val;

/**
 * Defines the boolean value.
 */
#[derive(Debug, Clone)]
pub struct BoolVal {
    pub val: bool,
    pub span: Span,
}


/**
 * Implementation of boolean value to provide operations.
 */
impl BoolVal {
    
}
