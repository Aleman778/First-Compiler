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
    let first = ctx.tokens.first();
    let second = ctx.tokens.second();
    let third = ctx.tokens.third();
    let span1 = Span::new(first.base, first.len);
    let span2 = Span::new(first.base, first.len + second.len);
    let span3 = Span::new(first.base, first.len + second.len + third.len);
    let operator = |op, n| {
        ctx.consume(n);
        Some(op)    
    };
    match (first.kind, second.kind, third.kind) {
        // Operators with three tokens
        (Lt, Lt, Eq)     => operator(BinOp::ShlEq(span3), 3),
        (Gt, Gt, Eq)     => operator(BinOp::ShrEq(span3), 3),

        // Operators with two tokens
        (Star, Star, _)  => operator(BinOp::Pow(span2), 2),
        (And, And, _)    => operator(BinOp::And(span2), 2),
        (Or, Or, _)      => operator(BinOp::Or(span2), 2),
        (Lt, Lt, _)      => operator(BinOp::Shl(span2), 2),
        (Gt, Gt, _)      => operator(BinOp::Shr(span2), 2),
        (Lt, Eq, _)      => operator(BinOp::Le(span2), 2),
        (Gt, Eq, _)      => operator(BinOp::Ge(span2), 2),
        (Eq, Eq, _)      => operator(BinOp::Eq(span2), 2),
        (Not, Eq, _)     => operator(BinOp::Ne(span2), 2),
        (Plus, Eq, _)    => operator(BinOp::AddEq(span2), 2),
        (Minus, Eq, _)   => operator(BinOp::SubEq(span2), 2),
        (Star, Eq, _)    => operator(BinOp::MulEq(span2), 2),
        (Slash, Eq, _)   => operator(BinOp::DivEq(span2), 2),
        (Percent, Eq, _) => operator(BinOp::ModEq(span2), 2),
        (And, Eq, _)     => operator(BinOp::BitAndEq(span2), 2),
        (Or, Eq, _)      => operator(BinOp::BitOrEq(span2), 2),
        (Caret, Eq, _)   => operator(BinOp::BitXorEq(span2), 2),

        // Operators with one tokens
        (Lt, _, _)       => operator(BinOp::Le(span1), 1),
        (Gt, _, _)       => operator(BinOp::Ge(span1), 1),
        (Eq, _, _)       => operator(BinOp::Eq(span1), 1),
        (Plus, _, _)     => operator(BinOp::Add(span1), 1),
        (Minus, _, _)    => operator(BinOp::Sub(span1), 1),
        (Star, _, _)     => operator(BinOp::Mul(span1), 1),
        (Slash, _, _)    => operator(BinOp::Div(span1), 1),
        (Percent, _, _)  => operator(BinOp::Mod(span1), 1),
        (And, _, _)      => operator(BinOp::BitAnd(span1), 1),
        (Or, _, _)       => operator(BinOp::BitOr(span1), 1),
        (Caret, _, _)    => operator(BinOp::BitXor(span1), 1),
        _                => None
    }
}


/**
 * Parses a unary operation using the next token in the token stream.
 */
pub fn parse_unop(ctx: &mut ParseCtxt) -> Option<UnOp> {
    let token = ctx.tokens.peek().unwrap_or(&DUMMY_TOKEN);
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
