//! Parser implementation for operators.


use crate::lexer::tokens::{TokenKind, DUMMY_TOKEN};
use crate::parser::ParseCtxt;
use crate::span::Span;
use crate::ast::{BinOp, UnOp};
use TokenKind::*;


/**
 * Parses an binary operation using the next token in the token stream.
 */
pub fn parse_binop(ctx: &mut ParseCtxt) -> Option<BinOp> {
    ctx.tokens.peek3();
    let first = ctx.tokens.peeked[0].as_ref().unwrap_or(&DUMMY_TOKEN);
    let second = ctx.tokens.peeked[1].as_ref().unwrap_or(&DUMMY_TOKEN);
    let third = ctx.tokens.peeked[2].as_ref().unwrap_or(&DUMMY_TOKEN);
    
    let span1 = Span::new(first.base, first.len);
    let span2 = Span::new(first.base, first.len + second.len);
    let span3 = Span::new(first.base, first.len + second.len + third.len);
    
    let (binop, num_tokens) = match (first.kind, second.kind, third.kind) {
        // Operators with three tokens
        (Lt, Lt, Eq)     => (Some(BinOp::ShlEq(span3)), 3),
        (Gt, Gt, Eq)     => (Some(BinOp::ShrEq(span3)), 3),

        // Operators with two tokens
        (Star, Star, _)  => (Some(BinOp::Pow(span2)), 2),
        (And, And, _)    => (Some(BinOp::And(span2)), 2),
        (Or, Or, _)      => (Some(BinOp::Or(span2)), 2),
        (Lt, Lt, _)      => (Some(BinOp::Shl(span2)), 2),
        (Gt, Gt, _)      => (Some(BinOp::Shr(span2)), 2),
        (Lt, Eq, _)      => (Some(BinOp::Le(span2)), 2),
        (Gt, Eq, _)      => (Some(BinOp::Ge(span2)), 2),
        (Eq, Eq, _)      => (Some(BinOp::Eq(span2)), 2),
        (Not, Eq, _)     => (Some(BinOp::Ne(span2)), 2),
        (Plus, Eq, _)    => (Some(BinOp::AddEq(span2)), 2),
        (Minus, Eq, _)   => (Some(BinOp::SubEq(span2)), 2),
        (Star, Eq, _)    => (Some(BinOp::MulEq(span2)), 2),
        (Slash, Eq, _)   => (Some(BinOp::DivEq(span2)), 2),
        (Percent, Eq, _) => (Some(BinOp::ModEq(span2)), 2),
        (And, Eq, _)     => (Some(BinOp::BitAndEq(span2)), 2),
        (Or, Eq, _)      => (Some(BinOp::BitOrEq(span2)), 2),
        (Caret, Eq, _)   => (Some(BinOp::BitXorEq(span2)), 2),

        // Operators with one tokens
        (Lt, _, _)       => (Some(BinOp::Le(span1)), 1),
        (Gt, _, _)       => (Some(BinOp::Ge(span1)), 1),
        (Eq, _, _)       => (Some(BinOp::Eq(span1)), 1),
        (Plus, _, _)     => (Some(BinOp::Add(span1)), 1),
        (Minus, _, _)    => (Some(BinOp::Sub(span1)), 1),
        (Star, _, _)     => (Some(BinOp::Mul(span1)), 1),
        (Slash, _, _)    => (Some(BinOp::Div(span1)), 1),
        (Percent, _, _)  => (Some(BinOp::Mod(span1)), 1),
        (And, _, _)      => (Some(BinOp::BitAnd(span1)), 1),
        (Or, _, _)       => (Some(BinOp::BitOr(span1)), 1),
        (Caret, _, _)    => (Some(BinOp::BitXor(span1)), 1),
        _                => (None, 0),
    };
    ctx.tokens.consume(num_tokens);
    binop
}


/**
 * Parses a unary operation using the next token in the token stream.
 */
pub fn parse_unop(ctx: &mut ParseCtxt) -> Option<UnOp> {
    let token = ctx.tokens.peek();
    let span = token.to_span();
    let op = match token.kind {
        Minus => Some(UnOp::Neg(span)),
        Not   => Some(UnOp::Not(span)),
        Caret => Some(UnOp::Ptr(span)),
        Star  => Some(UnOp::Deref(span)),
        _     => None
    };

    if let Some(_) = op {
        ctx.tokens.next()?;
    }

    op
}

#[cfg(test)]
/// Unit testing of the different operator parsers.
mod tests {
    use crate::lexer::tokenize;
    
}
