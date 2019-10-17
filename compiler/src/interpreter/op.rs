
/***************************************************************************
 * Operator interpreter implementation calcluates both binary
 * and unary operations from interpreted values.
 ***************************************************************************/


use std::ops;
use crate::ast::{
    span::Span,
    expr::*,
};
use crate::interpreter::{
    value::Val,
    RuntimeError,
    IResult,
    Eval,
};


impl BinOp {
    fn eval(&self, left: Val, right: Val, span: Span) -> IResult<Val> {
        match self {
            BinOp::Add => left.add(right),
        }
    }
}



/**
 * Implementation of add operator.
 */
impl ops::Add<Val> for Val {
    fn add(self, rhs: Val) -> Val {
        
    }
}
