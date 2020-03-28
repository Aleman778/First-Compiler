//! Parser implementation for statements.


use crate::span::{Span, DUMMY_SPAN};
use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::{expr, ty};
use crate::ast;
use TokenKind::*;


/**
 * Parses a statement using the provided token and parse context.
 */
pub fn parse_stmt(
    ctx: &mut ParseCtxt, 
    token: &Token
) -> Option<ast::Stmt> {
    let expr_lhs = expr::parse_expr(ctx, token, 1)?;
    match ctx.tokens.peek().kind {
        // Parses a variable declaration statement.
        Colon => {
            ctx.tokens.next()?;
            match ctx.tokens.peek().kind {
                Colon => parse_item(ctx, ),
                // No specified type, then infer the type.
                Eq => ast::Ty {
                    kind: ast::TyKind::Infer,
                    span: DUMMY_SPAN,
                },

                // Parses a specific type information.
                _ => ty::parse_ty(ctx, token)?
            }

            let token = ctx.tokens.next()?;
            if token.kind != Eq {
                unexpected_token_err!(ctx, token, [Eq]);
                return None;
            }
            
        }

        // Parses an expression statement ending with a semicolon.
        Semi => {

        }

        _ => {

        }
    }
}


/**
 * Parses a block statement using the provided token and parse context.
 */
pub fn parse_block(
    ctx: &mut ParseCtxt, 
    token: &Token
) -> Option<ast::Block> {
    debug_assert!(ctx.token.prev == OpenBrace);
    let base_pos = ctx.token.prev.base;
    let found_err = false;
    let mut block = ast::Block {
        stmts: Vec::new(),
        expr: None,
        span: DUMMY_SPAN,
    };

    loop {
        let token = ctx.tokens.next()?;
        match token.kind {
            CloseBrace => {
                block.span = Span::new(base_pos, token.base - base_pos + token.len);
                return Some(block);
            }

            _ => {
                if found_err {
                    continue;
                }
                
                if let Some(stmt) = parse_stmt(ctx, &token) {
                    if ctx.tokens.peek().kind == CloseBrace {
                        if let Expr(expr) = stmt {
                            block.expr = Some(expr);
                        } else {
                            block.stmts.push(Box::new(stmt));
                        }
                    } else {
                        block.stmts.push(Box::new(stmt));
                    }
                } else {
                    found_err = true;
                }
            }
        }
    }

    return None;
}
