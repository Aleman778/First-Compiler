//! Parser implementation for expressions.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::{utils, stmt, lit};
use crate::span::Span;
use crate::span::symbol::kw;
use crate::ast::op::Assoc;
use crate::span;
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

                kw::True => ast::ExprKind::Lit(
                    ast::Lit { 
                        kind: ast::LitKind::Bool(true), 
                        span: token.to_span(),
                    }
                ),

                kw::False => ast::ExprKind::Lit(
                    ast::Lit { 
                        kind: ast::LitKind::Bool(false), 
                        span: token.to_span(),
                    }
                ),

                kw::For    => parse_for(ctx)?,
                kw::If     => parse_if(ctx)?,
                kw::Return => parse_return(ctx)?,
                kw::While  => parse_while(ctx)?,
                kw::Loop   => parse_loop(ctx)?,
                
                // Parse any expression starting only with an identifier.
                _ => ast::ExprKind::Ident(utils::parse_identifier(ctx, token)?)
            }
        }

        // Parse any expression starting only with a raw identifier.
        RawIdent => ast::ExprKind::Ident(utils::parse_identifier(ctx, token)?),

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

        // Parse a range without lhs expression.
        Dot => parse_range(ctx, None)?,

        // Parse a negation unary expression.
        Minus => {
            let ntoken = ctx.tokens.next()?;
            let expr_rhs = parse_expr(ctx, &ntoken, min_prec)?;
            ast::ExprKind::Unary(ast::UnOp::Neg(token.to_span()), Box::new(expr_rhs))
        }

        // Parse a  expression.
        Not => {
            let ntoken = ctx.tokens.next()?;
            let expr_rhs = parse_expr(ctx, &ntoken, min_prec)?;
            ast::ExprKind::Unary(ast::UnOp::Not(token.to_span()), Box::new(expr_rhs))
        }

        // Parse a pointer expression.
        Star => {
            let token = ctx.tokens.next()?;
            let expr_rhs = parse_expr(ctx, &token, min_prec)?;
            ast::ExprKind::Pointer(Box::new(expr_rhs))
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
        // Parse a function call expression.
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

        // Parse an array index expression.
        OpenBracket => {
            ctx.tokens.next()?;
            expr_lhs = ast::Expr {
                node_id: ast::NodeId(0),
                kind: parse_index(ctx, Box::new(expr_lhs))?,
                span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
            }
        }

        // Parse a struct expression.
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

        // Parse either range or field expression.
        Dot => {
            let expr_kind = match ctx.tokens.peek().kind {
                // Parses a range expression.
                Dot => parse_range(ctx, Some(Box::new(expr_lhs)))?,

                // Parses a field expression.
                Ident |
                RawIdent => {
                    let token = ctx.tokens.next()?;
                    let ident = utils::parse_identifier(ctx, &token)?;
                    ast::ExprKind::Field(Box::new(expr_lhs), ident)
                }

                // Expects only `identifier` or `.`.
                _ => {
                    unexpected_token_err!(ctx, token, [Dot, Ident]);
                    return None;
                }
            };

            expr_lhs = ast::Expr {
                node_id: ast::NodeId(0),
                kind: expr_kind,
                span: Span::new(base_pos, ctx.tokens.cur_pos() - base_pos),
           }
        }

        _ => (),
    };

    // Parse optionally a binary expression using precedence climbing.
    while let Some((binop, num_tokens)) = utils::parse_binop(ctx, true) {
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

    let else_body = if let Some(kw) = utils::parse_keyword(ctx) {
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
    let ident = utils::parse_identifier(ctx, &token)?;

    if let Some(kw) = utils::parse_keyword(ctx) {
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
            lit::parse_int(ctx, token, radix, empty, suffix)?,

        LitKind::Float { radix, empty_exponent } => 
            lit::parse_float(ctx, token, radix, empty_exponent, suffix)?,

        LitKind::Char { terminated } => 
            lit::parse_character(ctx, token, terminated, suffix)?,

        LitKind::Byte { terminated } => 
            lit::parse_byte(ctx, token, terminated, suffix)?,

        LitKind::Str { terminated } => 
            lit::parse_string(ctx, token, terminated, suffix)?,

        LitKind::ByteStr { terminated } => 
            lit::parse_byte_string(ctx, token, terminated, suffix)?,

        LitKind::RawStr { num_hashes, started, terminated } =>
            lit::parse_raw_string(ctx, token, num_hashes, started, terminated, suffix)?,

        LitKind::RawByteStr { num_hashes, started, terminated } =>
            lit::parse_raw_byte_string(ctx, token, num_hashes, started, terminated, suffix)?,
    };
    Some(ast::ExprKind::Lit(literal))
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

    let token = ctx.tokens.next()?;
    let range_end = if token.kind == Eq {
        ctx.tokens.next()?;
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

    let fields = utils::parse_many(ctx, Comma, CloseBrace, |ctx, t| utils::parse_field(ctx, t))?;
    Some(ast::ExprKind::Struct(ident, fields))
}


#[cfg(test)]
/// Unit testing of the different expression parsers.
mod tests {
    use crate::lexer::tokenize;
    use crate::ast::map::AstMap;
    use crate::span::DUMMY_SPAN;
    use crate::span::symbol::SymbolMap;
    use crate::core::session::Session;
    use crate::core::source_map::Filename;
    use crate::parser::expr::parse_expr;
    use crate::parser::ParseCtxt;
    use crate::ast;
    use std::rc::Rc;
    use ast::ExprKind::*;
    use ast::LitKind::*;
    use ast::LitIntTy::*;

    macro_rules! test {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let mut sess = Session::new();
                let fname = Filename::Custom("test".to_string());
                let file = sess.source_map().insert_source_file(fname, $input.to_string());
                let tokens = tokenize(&file.source, file.start_pos.index());
                let mut ctx = ParseCtxt {
                    sess: &mut sess,
                    file: &file,
                    tokens: tokens,
                    ast_map: AstMap::new(),
                };

                let token = ctx.tokens.next().unwrap();
                let actual = parse_expr(&mut ctx, &token, 1);

                assert_eq!(actual, $expected.map(|e| *e));
            }
        }
    }


    macro_rules! ast {
        (expr: $kind:expr) => {
            Box::new(ast::Expr {
                node_id: ast::NodeId(0),
                kind: $kind,
                span: DUMMY_SPAN
            })
        };

        (lit: $kind:expr) => {
            Box::new(ast::Expr {
                node_id: ast::NodeId(0),
                kind: Lit(ast::Lit {
                    kind: $kind,
                    span: DUMMY_SPAN
                }),
                span: DUMMY_SPAN
            })
        };
    }


    // macro_rules! ident {
    //     ($ident: expr) => (
    //         unsafe {
    //             ast::Ident {
    //                 symbol: SYMBOLS.unwrap().as_symbol($ident),
    //                 span: DUMMY_SPAN,
    //             }
    //         }
    //     );
    // }
    

    // Testing array expression parser
    test!(parse_array, "[1, 3, 4]", Some(
        ast!(
            expr: Array(
                vec![
                    ast!(lit: Int(1, Unsuffixed)),
                    ast!(lit: Int(3, Unsuffixed)), 
                    ast!(lit: Int(4, Unsuffixed)),
                ]
            )
        )
    ));


    // Testing assignment expression parser
    // test!(parse_assign, "x = 5", Some(
    //     ast!(
    //         expr: Assign(
    //             ast!(expr: Ident(ident!("x"))),
    //             ast!(lit: Int(5, Unsuffixed))
    //         )
    //     )
    // ));
}
