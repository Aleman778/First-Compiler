
/***************************************************************************
 * Operator interpreter implementation calcluates both binary
 * and unary operations from interpreted values.
 ***************************************************************************/


use crate::ast::{
    span::Span,
    op::*,
};
use crate::interp::{
    value::Val,
    RuntimeError,
    IResult,
};


/**
 * Implementation of binary operator for binary expression evaluation
 */
impl BinOp {
    pub fn eval(&self, left: Val, right: Val, span: Span) -> IResult<Val> {
        let result = match self {
            BinOp::Add{span: _} => left.add(&right, span.clone()),
            BinOp::Sub{span: _} => left.sub(&right, span.clone()),
            BinOp::Div{span: _} => left.div(&right, span.clone()),
            BinOp::Mul{span: _} => left.mul(&right, span.clone()),
            BinOp::Pow{span: _} => left.pow(&right, span.clone()),
            BinOp::Mod{span: _} => left.modulo(&right, span.clone()),
            _ => unimplemented!(),
        };
        
        match result {
            Some(val) => Ok(val),
            None => Err(RuntimeError::binary_expr(span, self.clone(), right, left)),
        }
    }
}
