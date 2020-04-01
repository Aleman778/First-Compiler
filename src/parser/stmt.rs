//! Parser implementation for statements.


use crate::span::{Span, DUMMY_SPAN};
use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::{utils, expr, item, ty};
use crate::ast;
use TokenKind::*;


/**
 * Parses a statement using the provided token and parse context.
 */
pub fn parse_stmt(
    ctx: &mut ParseCtxt, 
    token: &Token
) -> Option<ast::Stmt> {
    debug_assert!(ctx.tokens.prev() == token.kind);

    let base_pos = token.base;
    let stmt_kind = match ctx.tokens.peek().kind {
        // Parses a variable declaration statement.
        Colon => {
            let ident = utils::parse_identifier(ctx, token)?;
            
            let ty = match ctx.tokens.peek2().kind {
                Colon => {
                    let item = Box::new(item::parse_item(ctx, token)?);
                    let kind = ast::StmtKind::Item(item);
                    let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);

                    return Some(ast::Stmt {
                        kind,
                        span
                    });
                }

                // No specified type, then infer the type.
                Eq => {
                    ctx.tokens.consume(1);
                    Some(Box::new(ast::Ty {
                        kind: ast::TyKind::Infer,
                        span: DUMMY_SPAN,
                    }))
                }

                // Parses a specific type information.
                _ => {
                    ctx.tokens.consume(1);
                    let token = utils::next_token(ctx)?;
                    Some(Box::new(ty::parse_ty(ctx, &token)?))
                }
            };

            let init = if ctx.tokens.peek().kind == Eq {
                ctx.tokens.consume(1);
                let token = utils::next_token(ctx)?;
                Some(Box::new(expr::parse_expr(ctx, &token, 1)?))
            } else {
                None
            };

            let token = utils::next_token(ctx)?;
            if token.kind != Semi {
                unexpected_token_err!(ctx, token, [Semi]);
                return None;
            }

            let span = Span::new(base_pos, ctx.tokens.cur_pos() - base_pos);
            let local = Box::new(ast::Local {
                ident,
                ty,
                init,
                span
            });
            
            ast::StmtKind::Local(local)
        }
        
        // Parses an expression statement.
        _ => {
            let expr = Box::new(expr::parse_expr(ctx, &token, 1)?);

            match ctx.tokens.peek().kind {
                Semi => {
                    ctx.tokens.consume(1);
                    ast::StmtKind::Semi(expr)
                }
                _ => ast::StmtKind::Expr(expr)
            }
        }
    };

    Some(ast::Stmt {
        kind: stmt_kind,
        span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
    })
}


/**
 * Parses a block statement using the provided token and parse context.
 */
pub fn parse_block(
    ctx: &mut ParseCtxt, 
    token: &Token
) -> Option<ast::Block> {
    debug_assert!(ctx.tokens.prev() == OpenBrace);
    debug_assert!(token.kind == OpenBrace);
    let base_pos = token.base;

    let mut found_err = false;
    let mut block = ast::Block {
        stmts: Vec::new(),
        expr: None,
        span: DUMMY_SPAN,
    };

    loop {
        let token = utils::next_token(ctx)?;
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
                        if let ast::StmtKind::Expr(expr) = stmt.kind {
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
}
