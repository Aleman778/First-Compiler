//! Perser implementation for expressions.


use crate::sqrrlc_lexer::tokens::*;
use crate::sqrrlc_parser::Parser;
use crate::sqrrlc::span::Span;
use crate::sqrrlc_ast::ast;
use TokenKind::*;


impl<'a> Parser<'a> {
    /**
     * Parses an expression using the next tokens in the
     * token stream.
     */
    pub fn parse_expr(&mut self) -> Option<ast::Expr> {
        let token = self.tokens.next()?;
        match token.kind {
            Literal { kind, suffix_start } => self.parse_literal(&token, kind, suffix_start),
            Unknown => {
                let span = Span::new(token.base, token.len);
                span_err!(self.sess, span, "unknown start of a token `{}`", self.file.get_source(span));
                None
            }
            _ => None
        }
    }


    /**
     * Parses a literal expression using the current token 
     */
    pub fn parse_literal(&mut self, token: &Token, kind: LitKind, suffix: usize) -> Option<ast::Expr> {
        let literal = match kind {
            LitKind::Int { radix, empty } => self.parse_int(token, radix, empty, suffix),
            // Float   { radix, empty } => self.parse_float(radix, empty_exponent, suffix),
            // Char    { terminated }   => self.parse_character(terminated, suffix),
            // Byte    { terminated }   => self.parse_byte(terminated, suffix),
            // ByteStr { terminated }   => self.parse_byte_string(terminated, suffix),
            // Str     { terminated }   => self.parse_string(terminated, suffix),
            // RawStr { num_hashes, started, terminated } =>
                // self.parse_raw_string(terminated, suffix),
            // RawByteStr { num_hashes, started, terminated } =>
            // self.parse_raw_byte_string(num_hashesh, started, terminated, suffix)
            _ => None
        };

        if let Some(lit) = literal {
            
            Some(ast::Expr {
                node_id: ast::NodeId(0),
                kind: ast::ExprKind::Lit(Box::new(lit)),
                span: Span::new(token.base, token.len),
            })
        } else {
            None
        }
    }
} 
