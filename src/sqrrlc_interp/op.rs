
/***************************************************************************
 * Operator interpreter implementation calcluates both binary
 * and unary operations from interpreted values.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    op::*,
};
use crate::sqrrlc_interp::{
    env::RuntimeEnv,
    value::Val,
    IResult,
};


/**
 * Implementation of binary operator for evaluating binary expressions.
 */
impl BinOp {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv<'a>, left: Val, right: Val, combined_span: Span) -> IResult<Val> {
        let left_type = left.get_type();
        let right_type = right.get_type();
        let (span, result) = match self {
            BinOp::Add{span} => (span, left.add(right, combined_span)),
            BinOp::Sub{span} => (span, left.sub(right, combined_span)),
            BinOp::Div{span} => (span, left.div(right, combined_span)),
            BinOp::Mul{span} => (span, left.mul(right, combined_span)),
            BinOp::Pow{span} => (span, left.pow(right, combined_span)),
            BinOp::Mod{span} => (span, left.r#mod(right, combined_span)),
            BinOp::And{span} => (span, left.and(right, combined_span)),
            BinOp::Or{span}  => (span, left.or(right, combined_span)),
            BinOp::Eq{span}  => (span, left.eq(right, combined_span)),
            BinOp::Ne{span}  => (span, left.ne(right, combined_span)),
            BinOp::Lt{span}  => (span, left.lt(right, combined_span)),
            BinOp::Le{span}  => (span, left.le(right, combined_span)),
            BinOp::Gt{span}  => (span, left.gt(right, combined_span)),
            BinOp::Ge{span}  => (span, left.ge(right, combined_span)),
        };
        
        match result {
            Some(val) => Ok(val),
            None => {
                let mut err = struct_span_fatal!(
                    env.sess,
                    *span,
                    "cannot {} `{}` to `{}`",
                    self,
                    left_type,
                    right_type
                );
                err.span_label(
                    *span,
                    &format!("no implementation for `{} {} {}`", left_type, self.token(), right_type)
                );
                Err(err)
            },
        }
    }
}


/**
 * Implementation of unary oprator for evaluating unary expressions.
 */
impl UnOp {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv<'a>, right: Val, combined_span: Span) -> IResult<Val> {
        let right_type = right.get_type();
        let (span, result) = match self {
            UnOp::Neg{span}   => (span, right.neg(combined_span)),
            UnOp::Not{span}   => (span, right.not(combined_span)),
            UnOp::Deref{span} => (span, right.deref(combined_span, env)?),
        };

        match result {
            Some(val) => Ok(val),
            None => {
                let mut err = struct_span_fatal!(
                    env.sess,
                    *span,
                    "type `{}` cannot be {}",
                    right_type,
                    self,
                );
                err.span_label(
                    *span,
                    &format!("no implementation for `{}{}`", self.token(), right_type)
                );
                Err(err)
            }
        }
    }
}
