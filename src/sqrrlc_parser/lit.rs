//! Parser implementation for literals.


use crate::sqrrlc::span::{symbol::sym, Span};
use crate::sqrrlc_parser::Parser;
use crate::sqrrlc_lexer::tokens::{Token, Radix};
use crate::sqrrlc_ast::ast::*;


impl<'a> Parser<'a> {
    pub fn parse_int(
        &mut self, 
        token: Token, 
        radix: Radix, 
        empty: bool, 
        suffix: usize
    ) -> Option<Lit> {
        if empty {
            let span = token.to_span();
            span_err!(self.sess, span, "no valid digits found for number");
            return None;
        }
        let suffix_span = Span::new(token.base + suffix, token.len - suffix);
        let lit_span = Span::new(token.base, suffix);
        let ty = if !suffix_span.is_empty() {
            let suffix = self.file.get_source(suffix_span);
            let symbol = self.sess.symbol_map.as_symbol(&suffix);
            match symbol {
                sym::i8    => LitIntTy::Signed(IntTy::I8),
                sym::i16   => LitIntTy::Signed(IntTy::I16),
                sym::i32   => LitIntTy::Signed(IntTy::I32),
                sym::i64   => LitIntTy::Signed(IntTy::I64),
                sym::i128  => LitIntTy::Signed(IntTy::I128),
                sym::isize => LitIntTy::Signed(IntTy::ISize),
                sym::u8    => LitIntTy::Unsigned(UIntTy::U8),
                sym::u16   => LitIntTy::Unsigned(UIntTy::U16),
                sym::u32   => LitIntTy::Unsigned(UIntTy::U32),
                sym::u64   => LitIntTy::Unsigned(UIntTy::U64),
                sym::u128  => LitIntTy::Unsigned(UIntTy::U128),
                sym::usize => LitIntTy::Unsigned(UIntTy::USize),
                _ => {
                    span_err!(self.sess, suffix_span, "invalid suffix expected e.g. i32, u16 etc.");
                    LitIntTy::Unsuffixed
                }
            }
        } else {
            LitIntTy::Unsuffixed
        };
        let input = self.file.get_source(lit_span);
        let (radix_offset, radix_value) = match radix {
            Radix::Binary      => (2, 2),
            Radix::Hexadecimal => (2, 16),
            Radix::Octal       => (2, 8),
            Radix::Decimal     => (0, 10),
        };
        let input = &input[radix_offset..];
        let mut value: u128 = 0;
        for c in input.bytes() {
            let span = Span::new(token.base + radix_offset, token.len - suffix - radix_offset);
            let x: u128 = match (c as char).to_digit(radix_value) {
                Some(x) => x as u128,
                None => {
                    span_err!(self.sess, span, "invalid digit for a base {} integer literal", radix_value);
                    continue;
                }
            };
            value = match value.checked_mul(radix_value as u128) {
                Some(x) => x,
                None => {
                    span_err!(self.sess, span, "integer literal is too large");
                    return None;
                }
            };
            value = match value.checked_add(x) {
                Some(x) => x,
                None => {
                    span_err!(self.sess, span, "integer literal is too large");
                    return None;
                }
            };
        }        
        println!("{}", value);
        let value = 0;
        Some(Lit { kind: LitKind::Int(value, ty), span: token.to_span(), })
    }
}
