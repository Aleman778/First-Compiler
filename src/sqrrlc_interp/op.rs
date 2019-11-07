
/***************************************************************************
 * Operator interpreter implementation calcluates both binary
 * and unary operations from interpreted values.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    op::*,
};
use crate::sqrrlc_interp::{
    error::{RuntimeError, ErrorKind},
    value::Val,
    IResult,
};


/**
 * Implementation of binary operator for evaluating binary expressions.
 */
impl BinOp {
    pub fn eval(&self, left: Val, right: Val, span: Span) -> IResult<Val> {
        let left_type = left.get_type();
        let right_type = right.get_type();
        let span_clone = span.clone();
        let result = match self {
            BinOp::Add{span: _} => left.add(right, span_clone),
            BinOp::Sub{span: _} => left.sub(right, span_clone),
            BinOp::Div{span: _} => left.div(right, span_clone),
            BinOp::Mul{span: _} => left.mul(right, span_clone),
            BinOp::Pow{span: _} => left.pow(right, span_clone),
            BinOp::Mod{span: _} => left.r#mod(right, span_clone),
            BinOp::And{span: _} => left.and(right, span_clone),
            BinOp::Or{span: _}  => left.or(right, span_clone),
            BinOp::Eq{span: _}  => left.eq(right, span_clone),
            BinOp::Ne{span: _}  => left.ne(right, span_clone),
            BinOp::Lt{span: _}  => left.lt(right, span_clone),
            BinOp::Le{span: _}  => left.le(right, span_clone),
            BinOp::Gt{span: _}  => left.gt(right, span_clone),
            BinOp::Ge{span: _}  => left.ge(right, span_clone),
        };
        
        match result {
            Some(val) => Ok(val),
            None => Err(RuntimeError {
                span: span,
                kind: ErrorKind::BinaryExpr(self.clone(), left_type, right_type),
            }),
        }
    }
}


/**
 * Implementation of unary oprator for evaluating unary expressions.
 */
impl UnOp {
    pub fn eval(&self, right: Val, span: Span) -> IResult<Val> {
        let right_type = right.get_type();
        let span_clone = span.clone();
        let result = match self {
            UnOp::Neg{span: _}   => right.neg(span_clone),
            UnOp::Not{span: _}   => right.not(span_clone),
            UnOp::Deref{span: _} => right.deref(span_clone),
        };

        match result {
            Some(val) => Ok(val),
            None => Err(RuntimeError {
                span: span,
                kind: ErrorKind::UnaryExpr(self.clone(), right_type),
            }),
        }
    }
}
