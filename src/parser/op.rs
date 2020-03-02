//! Parser implementation for operators.


use crate::lexer::tokens::TokenKind;
use crate::parser::ParseCtxt;
use crate::span::Span;
use crate::ast::{BinOp, UnOp};
use TokenKind::*;


/**
 * Parses an binary operation using the next token in the token stream.
 */
pub fn parse_binop(ctx: &mut ParseCtxt) -> Option<BinOp> {
    let first = ctx.tokens.next()?;
    let second = ctx.tokens.peek();
    let span = first.to_span();
    let combined_span = Span::new(
        first.base, 
        first.len + second.map_or(0, |s| s.len));
    match (first.kind, second.map_or(Unknown, |t| t.kind)) {
        // Multiple token operators
        (Eq, Eq)     => Some(BinOp::Eq(combined_span)),
        (Not, Eq)    => Some(BinOp::Ne(combined_span)),
        (And, And)   => Some(BinOp::And(combined_span)),
        (Or, Or)     => Some(BinOp::Or(combined_span)),
        (Lt, Eq)     => Some(BinOp::Lt(combined_span)),
        (Gt, Eq)     => Some(BinOp::Gt(combined_span)),
        (Star, Star) => Some(BinOp::Pow(combined_span)),
        
        // Single token operators
        (Plus, _)    => Some(BinOp::Add(span)),
        (Minus, _)   => Some(BinOp::Sub(span)),
        (Star, _)    => Some(BinOp::Mul(span)),
        (Slash, _)   => Some(BinOp::Div(span)),
        (Lt, _)      => Some(BinOp::Lt(span)),
        (Gt, _)      => Some(BinOp::Gt(span)),
        (Percent, _) => Some(BinOp::Mod(span)),
        _            => None,
    }
}


pub fn parse_unop(ctx: &mut ParseCtxt) -> Option<UnOp> {
    let token = ctx.tokens.next()?;
    let span = token.to_span();
    match token.kind {
        Minus => Some(UnOp::Neg(span)),
        Not => Some(UnOp::Not(span)),
        Star => Some(UnOp::Deref(span)),
        _ => None
    }
}
