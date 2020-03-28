//! Parser implementation for items, e.g. functions, structs etc.


use crate::span::{Span, DUMMY_SPAN};
use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::{utils, stmt};
use crate::ast;
use TokenKind::*;


/**
 * Parses an item using the provided context and token.
 */
pub fn parse_item(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::Item> {
    let base_pos = token.base;
    let ident = utils::parse_identifier(ctx, token)?;
    if !utils::parse_tokens(ctx, [Colon, Colon]) {
        return None;
    }

    let vis = ast::Visibility::Visible;

    let token = ctx.tokens.next()?;
    let keyword = utils::parse_keyword(ctx, &token)?;
    let kind = match keyword {
        kw::Fn => parse_fn(),
        kw::Struct => parse_struct(),
        kw::Enum => parse_enum(),
        kw::Extern => {
            let token = ctx.tokens.next()?;
            let keyword = utils::parse_keyword(ctx, &token)?;
            match keyword {
                kw::Fn => parse_extern_fn(),
                _ => {
                    unexpected_token_err!(ctx, token, [kw::Fn]);
                    return None;
                }
            }
        }
        _ => {
            unexpected_token_err!(ctx, token, [kw::Fn, kw::Struct, kw::Enum, kw::Extern]);
            return None;
        }
    };
    let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);

    Some(ast::Item {
        ident,
        node_id: NodeId(0),
        kind,
        vis,
        span
    })
}


/**
 * Parses a function item and its body using theprovided context.
 */
pub fn parse_fn(ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}


/**
 * Parses a foreign function interface using the provided context.
 */
pub fn parse_extern_fn(ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}


/**
 * Parses a function signature using the privded context.
 */
pub fn parse_fn_sig(ctx: &mut ParseCtxt) -> Option<ast::FnSig> {
    None
}


/**
 * Parses a struct item and its fields using the provided context.
 */
pub fn parse_struct(ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}


/**
 * Parses a enum item and its fields using the provided context.
 */
pub fn parse_enum(ctx: &mut ParseCtxt) -> Option<ast::ItemKind> {
    None
}
