//! Parser implementation for expressions.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::lit::*;
use crate::parser::op::*;
use crate::parser::stmt;
use crate::parser::utils;
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
    // Keep track of starting position, used for resulting expression span.
    let base_pos = token.base;

    // Starts by parsing atom starting with the provided token.
    let expr_kind = match token.kind {
        // Parse any expression starting with a keyword or identifier.
        Ident => {
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);

            match symbol {
                kw::Break => ast::ExprKind::Break,
                kw::Continue => ast::ExprKind::Continue,

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
                kw::Loop   => parse_loop(ctx)?,
                
                // Parse any expression starting only with an identifier.
                _ => parse_identifier(ctx, token)?
            }
        }

        // Parse any expression starting only with a raw identifier.
        RawIdent => parse_identifier(ctx, token)?,

        // Parse any literal expression using information from token and source code.
        Literal { kind, suffix_start } => parse_literal(ctx, &token, kind, suffix_start)?,

        // Parse parenthesized or tuple expression
        OpenParen => parse_parenthesized(ctx)?,

        // Parse an array expression.
        OpenBracket => {
            let exprs = utils::parse_many(ctx, Comma, CloseBracket, |ctx, t| parse_expr(ctx, t, 1))?;
            ast::ExprKind::Array(exprs)
        },

        // Parse a block expression.
        OpenBrace => {
            let token = ctx.tokens.next()?;
            let block = stmt::parse_block(ctx, &token)?;
            ast::ExprKind::Block(Box::new(block))
        }

        // Report error if failed to match any valid start of expression.
        _ => {
            unexpected_token_err!(ctx, token, ["expression"]);
            return None;
        }
    };

    // The parsed left hand expression.
    let mut expr_lhs = ast::Expr {
        node_id: ast::NodeId(0),
        kind: expr_kind,
        span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
    };

    // Some expressions that matches, we might want to combine it with lhs expr.
    match ctx.tokens.peek().kind {
        OpenParen => {
            if let ast::ExprKind::Ident(ident) = expr_lhs.kind {
                ctx.tokens.next()?;
                expr_lhs = ast::Expr {
                    node_id: ast::NodeId(0),
                    kind: parse_fn_call(ctx, ident)?,
                    span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
                }
            }
        }

        OpenBracket => {
            ctx.tokens.next()?;
            expr_lhs = ast::Expr {
                node_id: ast::NodeId(0),
                kind: parse_index(ctx, Box::new(expr_lhs))?,
                span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
            }
        }

        OpenBrace => {
            if let ast::ExprKind::Ident(ident) = expr_lhs.kind {
                ctx.tokens.next()?;
                expr_lhs = ast::Expr {
                    node_id: ast::NodeId(0),
                    kind: parse_struct(ctx, ident)?,
                    span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
                }
            }
        }

        _ => (),
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
            unexpected_token_err!(ctx, token, ["else"]);
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
    let ident = if let ast::ExprKind::Ident(ident) = parse_identifier(ctx, &token)? {
        ident
    } else {
        panic!("parsed identifier is not an identifier, what is going on?");
    };

    if let Some(kw) = parse_keyword(ctx) {
        if kw.index() != kw::In.index() {
            unexpected_token_err!(ctx, token, ["in"]);
            return None;
        } else {
            ctx.tokens.next()?;
        }
    } else {
        let token = ctx.tokens.peek();
        unexpected_token_err!(ctx, token, ["in"]);
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

    let token = ctx.tokens.next()?;
    let cond = Box::new(parse_expr(ctx, &token, 1)?);

    let token = ctx.tokens.next()?;
    let body = Box::new(parse_expr(ctx, &token, 1)?);

    Some(ast::ExprKind::While(cond, body))
}


/**
 * Parses a while loop using the next tokens in parse context.
 */
pub fn parse_loop(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);

    let token = ctx.tokens.next()?;
    let body = Box::new(parse_expr(ctx, &token, 1)?);

    Some(ast::ExprKind::Loop(body))
}


/**
 * Parses a function call using the next tokens in the parse context.
 */
pub fn parse_fn_call(ctx: &mut ParseCtxt, ident: ast::Ident) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == OpenParen);
    

    let exprs = utils::parse_many(ctx, Comma, CloseParen, |ctx, t| parse_expr(ctx, t, 1))?;
    Some(ast::ExprKind::Call(ident, exprs))
}


/**
 * Parses a function return statement using the next tokens in the parse context.
 */
pub fn parse_return(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Ident);

    let token = ctx.tokens.next()?;
    let expr = parse_expr(ctx, &token, 1).map(|expr| Box::new(expr));
    
    Some(ast::ExprKind::Return(expr))
}


/**
 * Parses an identifier expression using the provided token and context.
 * Identifier expressions includes `identifier`, `function calls`, etc.
 */
pub fn parse_identifier(ctx: &mut ParseCtxt, token: &Token) -> Option<ast::ExprKind> {
    debug_assert!(token.kind == Ident);
    let source = ctx.file.get_source(token.to_span());
    let symbol = ctx.sess.symbol_map.as_symbol(source);

    let ident = match token.kind {
        // For identifiers check validity i.e. it not a keyword or empty.
        Ident => {
            if let kw::Invalid = symbol {
                span_err!(ctx.sess, token.to_span(), "identifier cannot be empty");
                return None;
            } else if symbol.index() > kw::START_INDEX + 1 && symbol.index() < kw::LAST_INDEX {
                unexpected_token_err!(ctx, token, [Ident]);
                return None;
            } else {
                ast::Ident { 
                    symbol, 
                    span: token.to_span() 
                }
            }
        }

        // No checks neccessary for raw identifiers.
        RawIdent => {
            ast::Ident { 
                symbol, 
                span: token.to_span()
            }
        }

        // Nothing matched, expected either identifier or raw identifier.
        _ => {
            unexpected_token_err!(ctx, token, [Ident]);
            return None;
        }
    };

    Some(ast::ExprKind::Ident(ident))
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


/**
 * Parses a parenthesized or tuple expression using the next tokens in parse context.
 */
pub fn parse_parenthesized(ctx: &mut ParseCtxt) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == OpenParen);

    let token = ctx.tokens.next()?;
    let expr = Box::new(parse_expr(ctx, &token, 1)?);

    let token = ctx.tokens.peek();
    match token.kind {
        CloseParen => {
            ctx.tokens.next()?;
            Some(ast::ExprKind::Paren(expr))
        }
        Comma => {
            ctx.tokens.next()?;
            let mut exprs = utils::parse_many(ctx, Comma, CloseParen, |ctx, t| parse_expr(ctx, t, 1))?;
            exprs.insert(0, expr);
            Some(ast::ExprKind::Tuple(exprs))
        }
        _ => {   
            let token = ctx.tokens.next()?;
            unexpected_token_err!(ctx, token, [Comma, OpenParen]);
            None
        }
    }
}


/**
 * Parses a range expression using the next tokens in parse context.
 */
pub fn parse_range(ctx: &mut ParseCtxt, lhs_expr: Option<Box<ast::Expr>>) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == Dot);

    let token = ctx.tokens.next()?;
    if token.kind != Dot {
        unexpected_token_err!(ctx, token, [Dot]);
    }

    let mut token = ctx.tokens.next()?;
    let range_end = if token.kind == Eq {
        token = ctx.tokens.next()?;
        ast::RangeEnd::Included
    } else {
        ast::RangeEnd::Excluded
    };

    let token = ctx.tokens.peek();
    let rhs_expr = if utils::is_expr_start(&token) {
        let token = ctx.tokens.next()?;
        Some(Box::new(parse_expr(ctx, &token, 1)?))
    } else {
        None
    };

    Some(ast::ExprKind::Range(lhs_expr, rhs_expr, range_end))
}


/**
 * Parses an array index using the next tokens in parse context.
 */
pub fn parse_index(ctx: &mut ParseCtxt, lhs_expr: Box<ast::Expr>) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == OpenBracket);
    let token = ctx.tokens.next()?;
    let rhs_expr = Box::new(parse_expr(ctx, &token, 1)?);
    let token = ctx.tokens.next()?;
    match token.kind {
        CloseBracket => Some(ast::ExprKind::Index(lhs_expr, rhs_expr)),
        _ => {
            unexpected_token_err!(ctx, token, [CloseBracket]);
            None
        }
    }
}


/**
 * Parses a struct expression using the next tokens in the parse context and provided identifier.
 */
pub fn parse_struct(ctx: &mut ParseCtxt, ident: ast::Ident) -> Option<ast::ExprKind> {
    debug_assert!(ctx.tokens.prev == OpenBrace);

    let fields = utils::parse_many(ctx, Comma, CloseBrace, |ctx, t| parse_field(ctx, t))?;
    Some(ast::ExprKind::Struct(ident, fields))
}


/**
 * Parses a field ast node using the next tokens in the parse context and the provided token.
 */
pub fn parse_field(ctx: &mut ParseCtxt, token: &Token) -> Option<ast::Field> {
    let base_pos = token.base;
    let key = match parse_identifier(ctx, &token)? {
        ast::ExprKind::Ident(ident) => ident,
        _ => return None,
    };

    let token = ctx.tokens.next()?;
    if token.kind != Colon {
        unexpected_token_err!(ctx, token, [Colon]);
        return None;
    }
    
    let token = ctx.tokens.next()?;
    let value = Box::new(parse_expr(ctx, &token, 1)?);
    let span = Span::new(base_pos, token.base + token.len);
    
    Some(ast::Field { key, value, span })
}
