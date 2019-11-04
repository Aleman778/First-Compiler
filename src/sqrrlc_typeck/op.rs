
/***************************************************************************
 * Type checker implementation for operators
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    ty::Type,
    op::*,
};
use crate::sqrrlc_typeck::{
    error::*,
};


/**
 * Type checking implementation of binary operators.
 * Returns optionally an unimplemented error, if successfull None is returned instead.
 */
impl BinOp {
    pub fn check_type<'a>(&self, left: &'a Type, right: &'a Type) -> Option<TypeError> {
        match self {
            BinOp::Add{span} => self.compare(span, left, right, &Type::Int32{span: Span::new_empty()}),
            _ => None,
        }
    }

    fn compare(&self, span: &Span, left: &Type, right: &Type, expected: &Type) -> Option<TypeError> {
        if left != expected || right != expected {
            Some(TypeError::new(span.clone(), ErrorKind::BinOpNotImplemented(
                self.clone(), left.clone(), right.clone())))
        } else {
            None
        }        
    }
}
