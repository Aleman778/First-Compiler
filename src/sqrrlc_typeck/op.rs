
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
    /**
     * Check the types of the left and right expression agains the implemented
     * types supported for this operator.
     */
    pub fn check_type<'a>(&self, left: &'a Type, right: &'a Type) -> Option<TypeError> {
        let int_ty = Type::Int32{span: Span::new_empty()};
        let bool_ty = Type::Bool{span: Span::new_empty()};
        match self {
            BinOp::Add{span} => self.compare(span, left, right, &int_ty),
            BinOp::Sub{span} => self.compare(span, left, right, &int_ty),
            BinOp::Mul{span} => self.compare(span, left, right, &int_ty),
            BinOp::Div{span} => self.compare(span, left, right, &int_ty),
            BinOp::Pow{span} => self.compare(span, left, right, &int_ty),
            BinOp::Mod{span} => self.compare(span, left, right, &int_ty),
            BinOp::And{span} => self.compare(span, left, right, &int_ty),
            BinOp::Or{span} => self.compare(span, left, right, &int_ty),
            BinOp::Eq{span} => self.compare(span, left, right, &int_ty),
            BinOp::Ne{span} => self.compare(span, left, right, &int_ty),
            BinOp::Lt{span} => self.compare(span, left, right, &int_ty),
            BinOp::Gt{span} => self.compare(span, left, right, &int_ty),
            BinOp::Ge{span} => self.compare(span, left, right, &int_ty),
        }
    }
}
