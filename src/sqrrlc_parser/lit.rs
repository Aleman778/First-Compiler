//! Parser implementation for literals.


use crate::sqrrlc::span::{symbol::sym, Span};
use crate::sqrrlc_parser::Parser;
use crate::sqrrlc_lexer::tokens::{Token, Radix};
use crate::sqrrlc_ast::ast::*;


impl<'a> Parser<'a> {
    /**
     * Parses integer literal token and returns the literal ast node.
     */
    pub fn parse_int(
        &mut self, 
        token: &Token, 
        radix: Radix, 
        empty: bool, 
        suffix: usize
    ) -> Option<Lit> {
        if empty {
            span_err!(self.sess, token.to_span(), "no valid digits found for number");
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
                    span_err!(self.sess, suffix_span, "invalid suffix expected e.g. `i32`, `u16` etc.");
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
        for (i, c) in input.bytes().enumerate() {
            if c == b'_' {
                continue;
            }
            let x: u128 = match (c as char).to_digit(radix_value) {
                Some(x) => x as u128,
                None => {
                    let span = Span::new(token.base + radix_offset + i, 1);
                    span_err!(self.sess, span, "invalid digit for a base {} integer literal", radix_value);
                    continue;
                }
            };
            value = match value.checked_mul(radix as u128) {
                Some(x) => x,
                None => {
                    
                    span_err!(self.sess, lit_span, "integer literal is too large");
                    return None;
                }
            };
            value = match value.checked_add(x) {
                Some(x) => x,
                None => {
                    span_err!(self.sess, lit_span, "integer literal is too large");
                    return None;
                }
            };
        }
        println!("{}", value);
        Some(Lit { kind: LitKind::Int(value, ty), span: token.to_span(), })
    }


    /**
     * Parses floating-point literal and returns literal ast node.
     */
    pub fn parse_float(
        &mut self,
        token: &Token,
        radix: Radix,
        empty_exponent: bool,
        suffix: usize
    ) -> Option<Lit> {
        if empty_exponent {
            span_err!(self.span, token.to_span(), "expected at least one digit in exponent");
            return None;
        }
        let suffix_span = Span::new(token.base + suffix, token.len - suffix);
        let lit_span = Span::new(token.base, suffix);
        let ty = if !suffix.span.is_empty() {
            let suffix = self.file.get_source(suffix_span);
            let symbol = self.sess.symbol.as_symbol(&suffix);
            match symbol {
                sym::f32 => LitFloatTy::Suffixed(FloatTy::F32),
                sym::f64 => LitFloatTy::Suffixed(FloatTy::F64),
                _ => {
                    span_err!(self.sess, suffix_span, "invalid suffix expected `f32` or `f64`");
                    LitFloatTy::Unsuffixed
                }
            }
        } else {
            LitFloatTy::Unsuffixed
        };
        let input = self.file.get_source(lit_span);
        let (radix_offset, radix_value) = match radix {
            Radix::Binary => {
                span_err!(self.sess, token.to_span(), "binary float literal is not supported");
                return None;
            },
            Radix::Hexadecimal => {
                span_err!(self.sess, token.to_span(), "hexadecimal float literal is not supported");
                return None;
            }
            Radix::Octal => {
                span_err!(self.sess, token.to_span(), "octal float literal is not supported");
            }
            _ => ()
        };
        let point = input.find(".").unwrap();
        let value: f64 = 0.0;
        for (i, c) in input.bytes().enumerate() {
            if c == b'_' {
                continue;
            }
            let x: u128 = match (c as char).to_digit(radix_value) {
                Some(x) => x as u128,
                None => {
                    let span = Span::new(token.base + radix_offset + i, 1);
                    span_err!(self.sess, span, "invalid digit for a base {} integer literal", radix_value);
                    continue;
                }
            };
            value *= 10.0;
            value += 
        }
        println!("{}", value);
        Some(Lit {kind: LitKind::Float(value, ty), span: token.to_span(), })
    }
}


fn eat_digits(input: &[u8]) -> (&[u8], &[u8]) {
    
}
