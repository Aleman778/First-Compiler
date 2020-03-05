//! Perser implementation for expressions.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::lit::*;
use crate::parser::op::*;
use crate::span;
use crate::span::Span;
use crate::span::symbol::kw;
use crate::ast::op::Assoc;
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
    parse_expr_climb(ctx, token, 1)
}


/**
 * Parses a binary expression using precedence climbing.
 */
fn parse_expr_climb(
    ctx: &mut ParseCtxt, 
    token: &Token, 
    min_prec: u8
) -> Option<ast::Expr> {
    let base_pos = token.base;
    let expr_kind = match token.kind {
        // Ident => parse_ident(),
        // RawIdent => parse_raw_ident(),
        Literal { kind, suffix_start } =>
            parse_literal(ctx, token, kind, suffix_start)?,
        _ => {
            span_err!(ctx.sess, token.to_span(), "expected expression, found {}", token);
            return None;
        }
    };

    println!("base_pos: {}, cur_pos: {}", base_pos, ctx.tokens.cur_pos());
    let mut expr_lhs = ast::Expr {
        node_id: ast::NodeId(0),
        kind: expr_kind,
        span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
    };

    while let Some((binop, num_tokens)) = parse_binop(ctx, true) {
        let (prec, assoc) = binop.get_prec();
        if prec < min_prec {
            break;
        }

        let next_min_prec = match assoc {
            Assoc::Left => prec + 1,
            Assoc::Right => prec,
        };

        ctx.tokens.consume(num_tokens);
        let token = match ctx.tokens.next() {
            Some(token) => token,
            None => {
                span_err!(ctx.sess, 
                          Span::new(ctx.tokens.cur_pos(), 0), 
                          "unterminated binary expression");
                break;
            }
        };
            
        let expr_rhs = parse_expr_climb(ctx, &token, next_min_prec)?;
        let span = span::combine(&(expr_lhs).span, &(expr_rhs).span);

        let expr_kind = ast::ExprKind::Binary(
            binop, 
            Box::new(expr_lhs), 
            Box::new(expr_rhs)
        );

        expr_lhs = ast::Expr {
            node_id: ast::NodeId(0),
            kind: expr_kind,
            span: span,
        };
    }
    println!("{:#?}", expr_lhs);
    Some(expr_lhs)
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
