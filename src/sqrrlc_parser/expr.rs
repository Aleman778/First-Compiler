//! Perser implementation for expressions.


use crate::sqrrlc_lexer::tokens::*;
use crate::sqrrlc_parser::ParseCtxt;
use crate::sqrrlc_parser::lit::*;
use crate::sqrrlc::span::Span;
use crate::sqrrlc_ast::ast;
use TokenKind::*;


/**
 * Parses an expression using the next tokens in the token stream.
 */
pub fn parse_expr(ctx: &mut ParseCtxt) -> Option<ast::Expr> {
    let token = ctx.tokens.next()?;
    match token.kind {
        Literal { kind, suffix_start } => parse_literal(ctx, &token, kind, suffix_start),
        Unknown => {
            let span = Span::new(token.base, token.len);
            span_err!(ctx.sess, span, "unknown start of a token `{}`", ctx.file.get_source(span));
            None
        }
        _ => None
    }
}


/**
 * Parses a literal expression using the current token.
 */
pub fn parse_literal(
    ctx: &mut ParseCtxt, 
    token: &Token, 
    kind: LitKind, 
    suffix: usize
) -> Option<ast::Expr> {
    let literal = match kind {
        LitKind::Int { radix, empty } => 
            parse_int(ctx, token, radix, empty, suffix),

        LitKind::Float { radix, empty_exponent } => 
            parse_float(ctx, token, radix, empty_exponent, suffix),

        LitKind::Char { terminated } => 
            parse_character(ctx, token, terminated, suffix),

        LitKind::Byte { terminated } => 
            parse_byte(ctx, token, terminated, suffix),

        LitKind::Str { terminated } => 
            parse_string(ctx, token, terminated, suffix),

        LitKind::ByteStr { terminated } => 
            parse_byte_string(ctx, token, terminated, suffix),

        LitKind::RawStr { num_hashes, started, terminated } =>
            parse_raw_string(ctx, token, num_hashes, started, terminated, suffix),

        LitKind::RawByteStr { num_hashes, started, terminated } =>
            parse_raw_byte_string(ctx, token, num_hashes, started, terminated, suffix),
    };
    println!("{:#?}", literal);
    
    if let Some(lit) = literal {
        Some(ast::Expr {
            node_id: ast::NodeId(0),
            kind: ast::ExprKind::Lit(Box::new(lit)),
            span: Span::new(token.base, token.len),
        })
    } else {
        None
    }
} 
