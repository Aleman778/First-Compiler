//! Parser implementation for items, e.g. functions, structs etc.


use crate::span::{Span, DUMMY_SPAN};
use crate::span::symbol::kw;
use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::{utils, stmt, ty};
use crate::ast;
use TokenKind::*;


/**
 * Parses an item using the provided context and token.
 */
pub fn parse_item(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::Item> {
    debug_assert!(token.kind == Ident || token.kind == RawIdent);
    let base_pos = token.base;
    let ident = utils::parse_identifier(ctx, token)?;
    
    let first = utils::next_token(ctx)?;
    let second = utils::next_token(ctx)?;
    if first.kind != Colon || second.kind != Colon {
        let span = Span::new(first.base, second.base - first.base + second.len);
        span_err!(ctx.sess, span, "expected `::` found `{}{}`", first, second);
        return None;
    }

    let mut viskind = ast::VisibilityKind::Visible;
    let mut keyword = utils::parse_keyword(ctx)?;
    ctx.tokens.consume(1);
    match keyword {
        kw::Hidden => {
            viskind = ast::VisibilityKind::Hidden;
            keyword = utils::parse_keyword(ctx)?;
            ctx.tokens.consume(1);
        }
        _ => ()
    };
    
    let kind = match keyword {
        kw::Fn => parse_fn(ctx)?,
        kw::Struct => parse_struct(ctx)?,
        kw::Enum => parse_enum(ctx)?,
        kw::Extern => {
            let keyword = utils::parse_keyword(ctx)?;
            match keyword {
                kw::Fn => parse_extern_fn(ctx)?,
                _ => {
                    unexpected_token_err!(ctx, token, ["fn"]);
                    return None;
                }
            }
        }
        _ => {
            unexpected_token_err!(ctx, token, ["fn", "struct", "enum", "extern"]);
            return None;
        }
    };

    let vis = ast::Visibility {
        kind: viskind,
        span: DUMMY_SPAN,
    };

    let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);
    Some(ast::Item {
        ident,
        node_id: ast::NodeId(0),
        kind,
        vis,
        span
    })
}


/**
 * Parses a function item and its body using the provided context.
 */
pub fn parse_fn(ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    let fn_sig = parse_fn_sig(ctx)?;
    let token = utils::next_token(ctx)?;
    let block = Box::new(stmt::parse_block(ctx, &token)?);

    Some(ast::ItemKind::Fn(fn_sig, block))
}


/**
 * Parses a foreign function interface using the provided context.
 */
pub fn parse_extern_fn(_ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}


/**
 * Parses a function signature using the provided context.
 */
pub fn parse_fn_sig(ctx: &mut ParseCtxt) -> Option<ast::FnSig> {
    let token = utils::next_token(ctx)?;
    let base_pos = token.base;
    
    if token.kind != OpenParen {
        unexpected_token_err!(ctx, token, [OpenParen]);
        return None;
    }

    let inputs = utils::parse_many(ctx, Comma, CloseParen, parse_arg)?;

    // Parse output type part `-> ty`
    let mut output = None;
    if ctx.tokens.peek().kind == Minus {
        ctx.tokens.consume(1);

        if ctx.tokens.peek().kind == Gt {
            ctx.tokens.consume(1);

            let token = utils::next_token(ctx)?;
            output = Some(Box::new(ty::parse_ty(ctx, &token)?));
        } else {
            unexpected_token_err!(ctx, token, [Gt]);
            return None;
        }
    }

    let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);
    Some(ast::FnSig { inputs, output, span })
}


/**
 * Parses a function argument using the provided token and context.
 */
pub fn parse_arg(ctx: &mut ParseCtxt, token: &Token) -> Option<ast::FnArg> {
    let base_pos = token.base;
    let ident = utils::parse_identifier(ctx, token)?;

    let token = utils::next_token(ctx)?;
    if token.kind != Colon {
        unexpected_token_err!(ctx, token, [Colon]);
        return None;
    }

    let token = utils::next_token(ctx)?;
    let ty = Box::new(ty::parse_ty(ctx, &token)?);

    let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);
    Some(ast::FnArg { ident, ty, span})
}


/**
 * Parses a struct item and its fields using the provided context.
 */
pub fn parse_struct(_ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}


/**
 * Parses a enum item and its fields using the provided context.
 */
pub fn parse_enum(_ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}
