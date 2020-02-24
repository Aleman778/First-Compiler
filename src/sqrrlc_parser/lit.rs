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
        fn eat_digits(s: &[u8]) -> (Vec<u8>, &[u8]) {
            let mut i = 0;
            let mut digits = Vec::new();
            while i < s.len() && ((b'0' <= s[i] && s[i] <= b'9') || s[i] == b'_') {
                if s[i] != b'_' {
                    digits.push(s[i]);
                }
                i += 1;
            }
            (digits, &s[i..])
        }

        fn parse_exponent(s: &[u8]) -> (f64, &[u8]) {
            let (sign, s) = if s[0] == b'+' {
                (1_f64, &s[1..])
            } else if s[0] == b'-' {
                (-1_f64, &s[1..])
            } else {
                (1_f64, s)
            };
            let (exponent, s) = eat_digits(s);
            return (sign*parse_integral(&exponent), s)
        }

        fn parse_integral(s: &[u8]) -> f64 {
            let mut result: f64 = 0_f64;
            for d in s {
                result = result * 10_f64 + (d - b'0') as f64;
            }
            return result;
        }

        fn parse_fraction(s: &[u8]) -> f64 {
            let mut numerator: f64 = 0_f64;
            let mut denominator: f64 = 1_f64;
            for d in s {
                numerator = numerator * 10_f64 + (d - b'0') as f64;
                denominator = denominator * 10_f64;
            }
            return numerator / denominator;
        }

        if empty_exponent {
            span_err!(self.sess, token.to_span(), "expected at least one digit in exponent");
            return None;
        }
        let suffix_span = Span::new(token.base + suffix, token.len - suffix);
        let lit_span = Span::new(token.base, suffix);
        let ty = if !suffix_span.is_empty() {
            let suffix = self.file.get_source(suffix_span);
            let symbol = self.sess.symbol_map.as_symbol(&suffix);
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
        match radix {
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
                return None;
            }
            _ => ()
        };
        let input = self.file.get_source(lit_span);
        let (integral, input) = eat_digits(input.as_bytes());
        let mut value: f64 = parse_integral(&integral);
        match input.first() {
            None => {
                span_err!(self.sess, token.to_span(), "expected `e`, `E` or `.`, found nothing");
                return None;
            }
            Some(b'e') | Some(b'E') => {
                let (exponent, input) = parse_exponent(&input[1..]);
                debug_assert!(input.is_empty());
                if integral.is_empty() {
                    span_err!(self.sess, token.to_span(), "expected at least one digit before exponent");
                    return None;
                }
                value = parse_integral(&integral);
                value *= 10_f64.powf(exponent);
            }
            Some(b'.') => {
                let (fraction, input) = eat_digits(&input[1..]);
                if integral.is_empty() && fraction.is_empty() {
                    span_err!(self.sess, 
                              token.to_span(), 
                              "expected at least one digit before or after decimal point");
                    return None;
                }
                value += parse_fraction(&fraction);
                
                match input.first() {
                    Some(b'e') | Some(b'E') => {
                        let (exponent, input) = parse_exponent(&input[1..]);
                        debug_assert!(input.is_empty());
                        value *= 10_f64.powf(exponent);
                    },
                    _ => debug_assert!(input.is_empty())
                }
                
            }
            Some(c) => {
                span_err!(self.sess, token.to_span(), "expected `e`, `E` or `.`, found `{}`", c);
                return None;
            }
        }
        println!("{:?}", value);
        // println!("{}", value);
        // Some(Lit {kind: LitKind::Float(value, ty), span: token.to_span(), })
        None
    }

}
