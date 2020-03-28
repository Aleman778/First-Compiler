//! Parser utilit functions.


use crate::lexer::tokens::*;
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

    let token = ctx.tokens.next()?;
    if token.kind == end {
        return Some(exprs);
    } else {
        let expr = Box::new(parser(ctx, &token)?);
        exprs.push(expr);
    }
    
    loop {
        let token = ctx.tokens.next()?;
        if token.kind == sep {
            let token = ctx.tokens.next()?;
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
pub fn parse_identifier(ctx: &mut ParseCtxt, token: &Token) -> Option<Ident> {
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
 * Parses a sequence of tokens and reports error if any is missing or out of order.
 */
pub fn parse_tokens(ctx: &mut ParseCtxt, expected: &[Token]) -> bool {
    let mut span = ctx.tokens.peek().to_span();
    let mut found = Vec::new();
    for t in expected {
        let token = ctx.tokens.next()?;
        found.push(token);
        if t.kind != token.kind {
            span_err!(ctx.sess, t.to_span(), "expected `{:?}`, found `{:?}`", expected, found);
            return false;
        }
    }
    return true;
}


/**
 * Parses an initializer field ast node using the next tokens in the parse context and token.
 * It is possible to choose either to parse type, value or both at the same time.
 * Syntax: `ident: value`.
 */
pub fn parse_init_field(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::Field> {
    let base_pos = token.base;
    let key = parse_identifier(ctx, &token)?;

    let token = ctx.tokens.next()?;
    if token.kind != Colon {
        unexpected_token_err!(ctx, token, [Colon]);
        return None;
    }
    
    let token = ctx.tokens.next()?;
    let value = Box::new(expr::parse_expr(ctx, &token, 1)?);
    let span = Span::new(base_pos, token.base + token.len);
    Some(ast::Field { key, value, span })
}


/**
 * Parses a struct field ast node using the next tokens in the parse context and token.
 * It is possible to choose either to parse type, value or both at the same time.
 * Syntax: `ident: ty` or `ident: ty = value`.
 */
pub fn parse_struct_field(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::StructField> {
    let token = ctx.tokens.next()?;
    let base_pos = token.base;
    let key = parse_identifier(ctx, &token)?;

    let token = ctx.tokens.next()?;
    if token.kind != Colon {
        unexpected_token_err!(ctx, token, [Colon]);
        return None;
    }

    let token = ctx.tokens.next()?;
    let ty = Box::new(ty::parse_ty(ctx, &token, 1)?);
       
    let token = ctx.tokens.next()?;
    let value = if token.kind = Eq {
        let token = ctx.tokens.next()?;
        Some(Box::new(expr::parse_expr(ctx, &token, 1)?))
    } else {
        None
    };
    
    let kind = ast::FieldKind::Struct(ty, value);
    let span = Span::new(base_pos, token.base + token.len);
    Some(ast::Field { key, kind, span })
}


/**
 * Parses an enum field ast node using the next tokens in the parse context and token.
 * It is possible to choose either to parse type, value or both at the same time.
 * Syntax: `ident = value`.
 */
pub fn parse_enum_field(
    ctx: &mut ParseCtxt,
    token: &Token
) -> Option<ast::EnumField> {
let base_pos = token.base;
    let key = parse_identifier(ctx, &token)?;

    let token = ctx.tokens.next()?;
    let value = if token.kind == Eq {
        let token = ctx.tokens.next()?;
        Some(Box::new(expr::parse_expr(ctx, &token, 1)?));
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
pub fn parse_binop(ctx: &mut ParseCtxt, peek: bool) -> Option<(BinOp, usize)> {
    ctx.tokens.peek3();
    let first = ctx.tokens.peeked[0].as_ref().unwrap_or(&DUMMY_TOKEN);
    let second = ctx.tokens.peeked[1].as_ref().unwrap_or(&DUMMY_TOKEN);
    let third = ctx.tokens.peeked[2].as_ref().unwrap_or(&DUMMY_TOKEN);
    
    let span1 = Span::new(first.base, first.len);
    let span2 = Span::new(first.base, first.len + second.len);
    let span3 = Span::new(first.base, first.len + second.len + third.len);
    
    let result = match (first.kind, second.kind, third.kind) {
        // Operators with three tokens
        (Lt, Lt, Eq)     => Some((BinOp::ShlEq(span3), 3)),
        (Gt, Gt, Eq)     => Some((BinOp::ShrEq(span3), 3)),

        // Operators with two tokens
        (Star, Star, _)  => Some((BinOp::Pow(span2), 2)),
        (And, And, _)    => Some((BinOp::And(span2), 2)),
        (Or, Or, _)      => Some((BinOp::Or(span2), 2)),
        (Lt, Lt, _)      => Some((BinOp::Shl(span2), 2)),
        (Gt, Gt, _)      => Some((BinOp::Shr(span2), 2)),
        (Lt, Eq, _)      => Some((BinOp::Le(span2), 2)),
        (Gt, Eq, _)      => Some((BinOp::Ge(span2), 2)),
        (Eq, Eq, _)      => Some((BinOp::Eq(span2), 2)),
        (Not, Eq, _)     => Some((BinOp::Ne(span2), 2)),
        (Plus, Eq, _)    => Some((BinOp::AddEq(span2), 2)),
        (Minus, Eq, _)   => Some((BinOp::SubEq(span2), 2)),
        (Star, Eq, _)    => Some((BinOp::MulEq(span2), 2)),
        (Slash, Eq, _)   => Some((BinOp::DivEq(span2), 2)),
        (Percent, Eq, _) => Some((BinOp::ModEq(span2), 2)),
        (And, Eq, _)     => Some((BinOp::BitAndEq(span2), 2)),
        (Or, Eq, _)      => Some((BinOp::BitOrEq(span2), 2)),
        (Caret, Eq, _)   => Some((BinOp::BitXorEq(span2), 2)),

        // Operators with one tokens
        (Lt, _, _)       => Some((BinOp::Le(span1), 1)),
        (Gt, _, _)       => Some((BinOp::Ge(span1), 1)),
        (Eq, _, _)       => Some((BinOp::Eq(span1), 1)),
        (Plus, _, _)     => Some((BinOp::Add(span1), 1)),
        (Minus, _, _)    => Some((BinOp::Sub(span1), 1)),
        (Star, _, _)     => Some((BinOp::Mul(span1), 1)),
        (Slash, _, _)    => Some((BinOp::Div(span1), 1)),
        (Percent, _, _)  => Some((BinOp::Mod(span1), 1)),
        (And, _, _)      => Some((BinOp::BitAnd(span1), 1)),
        (Or, _, _)       => Some((BinOp::BitOr(span1), 1)),
        (Caret, _, _)    => Some((BinOp::BitXor(span1), 1)),
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
