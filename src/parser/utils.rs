//! Parser utilit functions.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::lit::*;
use crate::parser::op::*;
use crate::parser::stmt;
use crate::span;
use crate::span::Span;
use crate::span::symbol::{Symbol, kw};
use crate::ast::op::Assoc;
use crate::ast;
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
