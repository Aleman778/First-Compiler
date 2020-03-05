//! Perser implementation for expressions.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::lit::*;
use crate::parser::op::*;
use crate::span::Span;
use crate::span::symbol::kw;
use crate::ast;
use TokenKind::*;


/**
 * Parses an expression using the provided token and parse context.
 */
pub fn parse_expr(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::Expr> {
    debug_assert!(ctx.tokens.peeked.len() == 0);
    let atom = match token.kind {
        // Ident => parse_ident(),
        // RawIdent => parse_raw_ident(),
        Literal { kind, suffix_start } =>
            parse_literal(ctx, token, kind, suffix_start),
        _ => {
            span_err!(ctx.sess, token.to_span(), "expected expression, found {}", token);
            return None;
        }
    };

    Some(ast::Expr {
        node_id: ast::NodeId(0),
        kind: atom?,
        span: token.to_span(),
    })
}



/**
 * Parses a literal expression using the current token.
 */
pub fn parse_literal(
    ctx: &mut ParseCtxt, 
    token: &Token, 
    kind: LitKind,
    suffix: usize
) -> Option<ast::ExprKind> {
    let literal = match kind {
        LitKind::Int { radix, empty } => 
            parse_int(ctx, token, radix, empty, suffix)?,

        LitKind::Float { radix, empty_exponent } => 
            parse_float(ctx, token, radix, empty_exponent, suffix)?,

        LitKind::Char { terminated } => 
            parse_character(ctx, token, terminated, suffix)?,

        LitKind::Byte { terminated } => 
            parse_byte(ctx, token, terminated, suffix)?,

        LitKind::Str { terminated } => 
            parse_string(ctx, token, terminated, suffix)?,

        LitKind::ByteStr { terminated } => 
            parse_byte_string(ctx, token, terminated, suffix)?,

        LitKind::RawStr { num_hashes, started, terminated } =>
            parse_raw_string(ctx, token, num_hashes, started, terminated, suffix)?,

        LitKind::RawByteStr { num_hashes, started, terminated } =>
            parse_raw_byte_string(ctx, token, num_hashes, started, terminated, suffix)?,
    };
    Some(ast::ExprKind::Lit(Box::new(literal)))
} 
