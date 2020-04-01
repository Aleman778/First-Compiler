//! Parser utilit functions.


use crate::ast;
use crate::span::Span;
use crate::span::symbol::{Symbol, kw};
use crate::lexer::tokens::*;
use crate::parser::{expr, ty};
use crate::parser::ParseCtxt;
use TokenKind::*;


/**
 * Parses multiple ast nodes based on 
 */
pub fn parse_many<T, P>(
    ctx: &mut ParseCtxt,
    sep: TokenKind, 
    end: TokenKind,
    parser: P,
) -> Option<Vec<Box<T>>> 
where
    P: Fn(&mut ParseCtxt, &Token) -> Option<T>
{
    let mut exprs: Vec<Box<T>> = Vec::new();

    let token = next_token(ctx)?;
    if token.kind == end {
        return Some(exprs);
    } else {
        let expr = Box::new(parser(ctx, &token)?);
        exprs.push(expr);
    }
    
    loop {
        let token = next_token(ctx)?;
        if token.kind == sep {
            let token = next_token(ctx)?;
            let expr = Box::new(parser(ctx, &token)?);
            exprs.push(expr);
        } else if token.kind == end {
            break;
        } else {
            unexpected_token_err!(ctx, token, [sep, end]);
            return None;
        }
    }

    Some(exprs)
}


/**
 * Parses an identifier using the provided token and context.
 */
pub fn parse_identifier(ctx: &mut ParseCtxt, token: &Token) -> Option<ast::Ident> {
    debug_assert!(token.kind == Ident || token.kind == RawIdent);
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

    Some(ident)
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
 * Parses an initializer field ast node using the next tokens in the parse context and token.
 * It is possible to choose either to parse type, value or both at the same time.
 * Syntax: `ident: value`.
 */
pub fn parse_field(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::Field> {
    let base_pos = token.base;
    let key = parse_identifier(ctx, &token)?;

    let token = next_token(ctx)?;
    if token.kind != Colon {
        unexpected_token_err!(ctx, token, [Colon]);
        return None;
    }
    
    let token = next_token(ctx)?;
    let value = Box::new(expr::parse_expr(ctx, &token, 1)?);
    let span = Span::new(base_pos, token.base + token.len);
    Some(ast::Field { key, value, span })
}


/**
 * Parses a struct field ast node using the next tokens in the parse context and token.
 * It is possible to choose either to parse type, value or both at the same time.
 * Syntax: `ident: ty` or `ident: ty = value`.
 */
#[allow(dead_code)]
pub fn parse_struct_field(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::StructField> {
    let base_pos = token.base;
    let key = parse_identifier(ctx, &token)?;

    let token = next_token(ctx)?;
    if token.kind != Colon {
        unexpected_token_err!(ctx, token, [Colon]);
        return None;
    }

    let token = next_token(ctx)?;
    let ty = Box::new(ty::parse_ty(ctx, &token)?);
       
    let token = next_token(ctx)?;
    let value = if token.kind == Eq {
        let token = next_token(ctx)?;
        Some(Box::new(expr::parse_expr(ctx, &token, 1)?))
    } else {
        None
    };
    
    let span = Span::new(base_pos, token.base + token.len);
    Some(ast::StructField { key, ty, value, span })
}


/**
 * Parses an enum field ast node using the next tokens in the parse context and token.
 * It is possible to choose either to parse type, value or both at the same time.
 * Syntax: `ident = value`.
 */
#[allow(dead_code)]
pub fn parse_enum_field(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::EnumField> {
    let base_pos = token.base;
    let key = parse_identifier(ctx, &token)?;

    let token = next_token(ctx)?;
    let value = if token.kind == Eq {
        let token = next_token(ctx)?;
        Some(Box::new(expr::parse_expr(ctx, &token, 1)?))
    } else {
        None
    };

    let span = Span::new(base_pos, token.base + token.len);
    Some(ast::EnumField { key, value, span })
}


/**
 * Parses an binary operation using the next token in the token stream.
 * Parsing binops uses peeking for tokens but consumes the tokens that
 * are used, set peek to true for skipping the consumption of peeked tokens.
 */
pub fn parse_binop(ctx: &mut ParseCtxt, peek: bool) -> Option<(ast::BinOp, usize)> {
    ctx.tokens.peek3();
    let first = ctx.tokens.peeked[0].as_ref().unwrap_or(&DUMMY_TOKEN);
    let second = ctx.tokens.peeked[1].as_ref().unwrap_or(&DUMMY_TOKEN);
    let third = ctx.tokens.peeked[2].as_ref().unwrap_or(&DUMMY_TOKEN);
    
    let span1 = Span::new(first.base, first.len);
    let span2 = Span::new(first.base, first.len + second.len);
    let span3 = Span::new(first.base, first.len + second.len + third.len);
    
    let result = match (first.kind, second.kind, third.kind) {
        // Operators with three tokens
        (Lt, Lt, Eq)     => Some((ast::BinOp::ShlEq(span3), 3)),
        (Gt, Gt, Eq)     => Some((ast::BinOp::ShrEq(span3), 3)),

        // Operators with two tokens
        (Star, Star, _)  => Some((ast::BinOp::Pow(span2), 2)),
        (And, And, _)    => Some((ast::BinOp::And(span2), 2)),
        (Or, Or, _)      => Some((ast::BinOp::Or(span2), 2)),
        (Lt, Lt, _)      => Some((ast::BinOp::Shl(span2), 2)),
        (Gt, Gt, _)      => Some((ast::BinOp::Shr(span2), 2)),
        (Lt, Eq, _)      => Some((ast::BinOp::Le(span2), 2)),
        (Gt, Eq, _)      => Some((ast::BinOp::Ge(span2), 2)),
        (Eq, Eq, _)      => Some((ast::BinOp::Eq(span2), 2)),
        (Not, Eq, _)     => Some((ast::BinOp::Ne(span2), 2)),
        (Plus, Eq, _)    => Some((ast::BinOp::AddEq(span2), 2)),
        (Minus, Eq, _)   => Some((ast::BinOp::SubEq(span2), 2)),
        (Star, Eq, _)    => Some((ast::BinOp::MulEq(span2), 2)),
        (Slash, Eq, _)   => Some((ast::BinOp::DivEq(span2), 2)),
        (Percent, Eq, _) => Some((ast::BinOp::ModEq(span2), 2)),
        (And, Eq, _)     => Some((ast::BinOp::BitAndEq(span2), 2)),
        (Or, Eq, _)      => Some((ast::BinOp::BitOrEq(span2), 2)),
        (Caret, Eq, _)   => Some((ast::BinOp::BitXorEq(span2), 2)),

        // Operators with one tokens
        (Lt, _, _)       => Some((ast::BinOp::Le(span1), 1)),
        (Gt, _, _)       => Some((ast::BinOp::Ge(span1), 1)),
        (Eq, _, _)       => Some((ast::BinOp::Eq(span1), 1)),
        (Plus, _, _)     => Some((ast::BinOp::Add(span1), 1)),
        (Minus, _, _)    => Some((ast::BinOp::Sub(span1), 1)),
        (Star, _, _)     => Some((ast::BinOp::Mul(span1), 1)),
        (Slash, _, _)    => Some((ast::BinOp::Div(span1), 1)),
        (Percent, _, _)  => Some((ast::BinOp::Mod(span1), 1)),
        (And, _, _)      => Some((ast::BinOp::BitAnd(span1), 1)),
        (Or, _, _)       => Some((ast::BinOp::BitOr(span1), 1)),
        (Caret, _, _)    => Some((ast::BinOp::BitXor(span1), 1)),
        _                => None,
    };
    if !peek {
        if let Some((_, num_tokens)) = result {
            ctx.tokens.consume(num_tokens);
        }
    }
    result
}


/**
 * Strips the next tokens that are comments and
 * returns the next token that is not a comment.
 * Note: this function does not report errors when
 * lexer reached end of file, do manual check on 
 * option instead of try operator.
 */
pub fn next_non_comment_token(ctx: &mut ParseCtxt) -> Option<Token> { 
    while let Some(token) = ctx.tokens.next() {
        match token.kind {
            LineComment => (),

            BlockComment { terminated } => {
                if !terminated {
                    span_err!(ctx.sess, token.to_span(), "block comment is not terminated");
                }
            }

            _ => return Some(token),
        }
    }
    None
}


/**
 * Returns the next token in the lexed token stream.
 * Note: this function does report error when lexer 
 * reached end of file, so using try operator is fine! 
 */
pub fn next_token(ctx: &mut ParseCtxt) -> Option<Token> {
    let token = ctx.tokens.next();
    if token.is_none() {
        let span = Span::from_range(ctx.file.end_pos, ctx.file.end_pos);
        let mut err = struct_span_err!(ctx.sess, span, "reached end of file while parsing");
        err.span_label(span, "help: maybe missing closing curly brace: `}`");
        ctx.sess.emit(&mut err);
    }
    token
}


/**
 * Check if a given token is the start of an expression,
 * i.e. running parse_expr with this token does not 
 * immediately report error.
 */
pub fn is_expr_start(token: &Token) -> bool {
    match token.kind {
        Ident |
        RawIdent |
        OpenParen |
        OpenBracket |
        OpenBrace => true,

        _ => false,
    }
}
