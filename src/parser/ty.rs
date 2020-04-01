//! Parser implementation for types.

use crate::ast;
use crate::lexer::tokens::*;
use crate::span::Span;
use crate::span::symbol::sym;
use crate::parser::ParseCtxt;
use crate::parser::{utils, expr};
use TokenKind::*;



/**
 * Parses a type declaration there are different kinds of types.
 * Each kind of type is parsed individually.
 */
pub fn parse_ty(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::Ty> {
    debug_assert!(ctx.tokens.prev() == token.kind);

    let base_pos = token.base;
    let ty_kind = match token.kind {
        // Parses any type starting with an identifier, e.g. `i32`.
        Ident => {
            let source = ctx.file.get_source(token.to_span());
            let symbol = ctx.sess.symbol_map.as_symbol(source);

            match symbol {
                // Signed integer types.
                sym::i8    => ast::TyKind::Int(ast::IntTy::I8),
                sym::i16   => ast::TyKind::Int(ast::IntTy::I16),
                sym::i32   => ast::TyKind::Int(ast::IntTy::I32),
                sym::i64   => ast::TyKind::Int(ast::IntTy::I64),
                sym::i128  => ast::TyKind::Int(ast::IntTy::I128),
                sym::isize => ast::TyKind::Int(ast::IntTy::ISize),

                // Unsigned integer types.
                sym::u8    => ast::TyKind::UInt(ast::UIntTy::U8),
                sym::u16   => ast::TyKind::UInt(ast::UIntTy::U16),
                sym::u32   => ast::TyKind::UInt(ast::UIntTy::U32),
                sym::u64   => ast::TyKind::UInt(ast::UIntTy::U64),
                sym::u128  => ast::TyKind::UInt(ast::UIntTy::U128),
                sym::usize => ast::TyKind::UInt(ast::UIntTy::USize),

                // Float types.
                sym::f32 => ast::TyKind::Float(ast::FloatTy::F32),
                sym::f64 => ast::TyKind::Float(ast::FloatTy::F64),

                // Miscellaneous types.
                sym::str  => ast::TyKind::Str,
                sym::char => ast::TyKind::Char,
                sym::bool => ast::TyKind::Bool,

                _ => parse_custom_ty(ctx, token)?,
            }
        },

        // Parses custom types e.g. `MyStruct`.
        RawIdent => parse_custom_ty(ctx, token)?,

        // Parses array declarations e.g. `[N] i32`.
        OpenBracket => {
            let token = utils::next_token(ctx)?;
            let size_expr = if token.kind == CloseBracket {
                None
            } else {
                let expr = Box::new(expr::parse_expr(ctx, &token, 1)?);
                let token = utils::next_token(ctx)?;
                if token.kind != CloseBracket {
                    unexpected_token_err!(ctx, token, [CloseBracket]);
                    return None;
                }
                Some(expr)
            };

            let token = utils::next_token(ctx)?;
            let element_ty = Box::new(parse_ty(ctx, &token)?);

            ast::TyKind::Array(size_expr, element_ty)
        }

        // Parses none, tuple or function signature type,
        // e.g. `()`, `(str, i32)` or `(i32, i32) -> i32` respectively.
        OpenParen => {
            let token = utils::next_token(ctx)?;
            if token.kind == CloseParen {
                ast::TyKind::None
            } else {
                let inputs = utils::parse_many(ctx, Comma, CloseParen, parse_ty)?;

                if ctx.tokens.peek().kind == Minus {
                    ctx.tokens.consume(1);

                    if ctx.tokens.peek().kind == Gt {
                        ctx.tokens.consume(1);

                        let token = utils::next_token(ctx)?;
                        let output = Box::new(parse_ty(ctx, &token)?);
                        ast::TyKind::FnSig(inputs, output)
                    } else {
                        let token = utils::next_token(ctx)?;
                        unexpected_token_err!(ctx, token, [Minus]);
                        return None;
                    }
                } else {
                    ast::TyKind::Tuple(inputs)
                }
            }
        }

        // Pointer type e.g. `*MyStruct`.
        Star => {
            let token = utils::next_token(ctx)?;
            let ty = Box::new(parse_ty(ctx, &token)?);
            ast::TyKind::Pointer(ty)
        }

        // Polymorphic type e.g. `$T` or `$T/MyStruct`.
        Dollar => {
            let token = utils::next_token(ctx)?;
            let ident = utils::parse_identifier(ctx, &token)?;
            
            let ty = if ctx.tokens.peek().kind == Slash {
                ctx.tokens.consume(1);

                let token = utils::next_token(ctx)?;
                let kind = parse_custom_ty(ctx, &token)?;
                let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);
                Some(Box::new(ast::Ty { kind, span }))
            } else {
                None
            };

            ast::TyKind::Polymorphic(ident, ty)
        }

        // Failed to parse type report error.
        _ => {
            unexpected_token_err!(ctx, token, ["type"]);
            return None;
        }
    };

    let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);
    Some(ast::Ty { kind: ty_kind, span })
}


pub fn parse_custom_ty(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::TyKind> {
    let ident = utils::parse_identifier(ctx, token)?;
    Some(ast::TyKind::Custom(ident))
}
