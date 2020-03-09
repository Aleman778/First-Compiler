//! Perser implementation for expressions.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::lit::*;
use crate::parser::op::*;
use crate::span;
use crate::span::Span;
use crate::span::symbol::{Symbol, kw};
use crate::ast::op::Assoc;
use crate::ast;
use TokenKind::*;


/**
 * Parses an expression using the provided token reference from the parse context.
 */
pub fn parse_expr(
    ctx: &mut ParseCtxt,
    token: &Token,
    min_prec: u8,
) -> Option<ast::Expr> {
    let base_pos = token.base;
    let expr_kind = match token.kind {
        Ident => {
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);

            match symbol {
                kw::Break => ast::ExprKind::Break,
                kw::Continue => ast::ExprKind ::Continue,

                kw::True => ast::ExprKind::Lit(Box::new(
                    ast::Lit { 
                        kind: ast::LitKind::Bool(true), 
                        span: token.to_span(),
                    }
                )),

                kw::False => ast::ExprKind::Lit(Box::new(
                    ast::Lit { 
                        kind: ast::LitKind::Bool(false), 
                        span: token.to_span(),
                    }
                )),

                kw::For    => parse_for(ctx)?,
                kw::If     => parse_if(ctx)?,
                kw::Return => parse_return(ctx)?,
                kw::While  => parse_while(ctx)?,
                
                _ => ast::ExprKind::Ident(Box::new(
                    ast::Ident { 
                        symbol, 
                        span: token.to_span() 
                    }
                ))
            }
        }

        RawIdent => {   
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);
            ast::ExprKind::Ident(Box::new(
                ast::Ident { 
                    symbol, 
                    span: token.to_span() 
                }
            ))
        }

        Literal { kind, suffix_start } => parse_literal(ctx, &token, kind, suffix_start)?,
        OpenParen => parse_parenthesized(ctx)?,
        OpenBrace => parse_block(ctx)?,
        OpenBracket => parse_array(ctx)?,

        _ => {
            span_err!(ctx.sess, token.to_span(), "expected expression, found {}", token);
            return None;
        }
    };

    // The parsed left hand expression.
    let mut expr_lhs = ast::Expr {
        node_id: ast::NodeId(0),
        kind: expr_kind,
        span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
    };

    // Parse optionally a binary expression using precedence climbing.
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
        let token = ctx.tokens.next()?;
        let expr_rhs = parse_expr(ctx, &token, next_min_prec)?;
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


    Some(expr_lhs)
}


/**
 * Parses an if statement using the next tokens in the parse context.
 */
pub fn parse_if(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);
        
    let token = ctx.tokens.next()?;
    let cond = Box::new(parse_expr(ctx, &token, 1)?);

    let token = ctx.tokens.next()?;
    let then_body = Box::new(parse_expr(ctx, &token, 1)?);

    let else_body = if let Some(kw) = parse_keyword(ctx) {
        if kw.index() == kw::Else.index() {
            ctx.tokens.next()?;
            let token = ctx.tokens.next()?;
            Some(Box::new(parse_expr(ctx, &token, 1)?))
        } else {
            span_err!(ctx.sess, 
                      token.to_span(), 
                      "expected keyword `else`, found `{}`",
                      ctx.sess.symbol_map.as_str(kw));
            return None;
        }
    } else {
        None
    };

    if else_body.is_some() {
        ctx.tokens.next()?;
    }
        
    Some(ast::ExprKind::If(cond, then_body, else_body))
}


/**
 * Parses a for loop using the next tokens in parse context.
 */
pub fn parse_for(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);

    let token = ctx.tokens.next()?;
    let ident = Box::new(parse_ident(ctx, &token)?);

    if let Some(kw) = parse_keyword(ctx) {
        if kw.index() != kw::In.index() {
            span_err!(ctx.sess, 
                      token.to_span(), 
                      "expected keyword `in`, found `{}`",
                      ctx.sess.symbol_map.as_str(kw));
            return None;
        } else {
            ctx.tokens.next()?;
        }
    } else {
        let token = ctx.tokens.peek();
        span_err!(ctx.sess, 
                  token.to_span(), 
                  "expected keyword `in`, found `{}`",
                  token);
        return None;
    }

    let token = ctx.tokens.next()?;
    let iter = Box::new(parse_expr(ctx, &token, 1)?);

    let token = ctx.tokens.next()?;
    let body = Box::new(parse_expr(ctx, &token, 1)?);

    Some(ast::ExprKind::For(ident, iter, body))
}


/**
 * Parses a while loop using the next tokens in parse context.
 */
pub fn parse_while(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);

    None
}


/**
 * Parses a function call using the next tokens in the parse context.
 */
pub fn parse_fn_call(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);

    None
}


/**
 * Parses a function return statement using the next tokens in the parse context.
 */
pub fn parse_return(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);

    None
}


/**
 * Parses an identifier using the provided token and context.
 */
pub fn parse_ident(ctx: &mut ParseCtxt, token: &Token) -> Option<ast::Ident> {
    debug_assert!(token.kind == Ident);

    match token.kind {
        Ident => {
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);

            if let kw::Invalid = symbol {
                span_err!(ctx.sess, token.to_span(), "identifier cannot be empty");
                None
            } else if symbol.index() > kw::START_INDEX + 1 && symbol.index() < kw::LAST_INDEX {
                span_err!(ctx.sess, token.to_span(), "expected identifier, found keyword `{}`", source);
                None
            } else {
                Some(ast::Ident { 
                    symbol, 
                    span: token.to_span() 
                })
            }
        }

        RawIdent => {
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);

            Some(ast::Ident { 
                    symbol, 
                    span: token.to_span() 
                }
            )
        }

        _ => {
            span_err!(ctx.sess, token.to_span(), "expected identifier, found `{}`", token);
            None
        }
    }
}



/**
 * Tries to parse a specific keyword and reports error if not found.
 * One token is peeked and should be consumed if needed outside of this function.
 */
pub fn parse_keyword(ctx: &mut ParseCtxt) -> Option<Symbol> {
    let token = ctx.tokens.peek();
    match token.kind {
        Ident => {
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);

            if let kw::Invalid = symbol {
                span_err!(ctx.sess, token.to_span(), "keyword cannot be empty");
                None
            } else if symbol.index() > kw::START_INDEX + 1 && symbol.index() < kw::LAST_INDEX {
                Some(symbol)
            } else {
                None
            }
        }

        _ => {
            None
        }
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


pub fn parse_parenthesized(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    None
}


pub fn parse_tuple(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    None
}


pub fn parse_block(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    None
}


pub fn parse_array(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    None
}
