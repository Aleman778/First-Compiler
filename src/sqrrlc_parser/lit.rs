//! Parser implementation for literals.


use crate::sqrrlc::span::{symbol::sym, Span};
use crate::sqrrlc_parser::ParseCtxt;
use crate::sqrrlc_lexer::tokens::{Token, Radix};
use crate::sqrrlc_ast::ast::*;


/**
 * Parses integer literal token.
 */
pub fn parse_int(
    ctx: &mut ParseCtxt, 
    token: &Token, 
    radix: Radix, 
    empty: bool, 
    suffix: usize
) -> Option<Lit> {
    if empty {
        span_err!(ctx.sess, token.to_span(), "no valid digits found for number");
        return None;
    }
    let suffix_span = Span::new(token.base + suffix, token.len - suffix);
    let lit_span = Span::new(token.base, suffix);
    let ty = if !suffix_span.is_empty() {
        let suffix = ctx.file.get_source(suffix_span);
        let symbol = ctx.sess.symbol_map.as_symbol(&suffix);
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
                span_err!(ctx.sess, suffix_span, "invalid suffix expected e.g. `i32`, `u16` etc.");
                LitIntTy::Unsuffixed
            }
        }
    } else {
        LitIntTy::Unsuffixed
    };
    let input = ctx.file.get_source(lit_span);
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
                span_err!(ctx.sess, span, "invalid digit for a base {} integer literal", radix_value);
                continue;
            }
        };
        value = match value.checked_mul(radix as u128) {
            Some(x) => x,
            None => {
                span_err!(ctx.sess, lit_span, "integer literal is too large");
                return None;
            }
        };
        value = match value.checked_add(x) {
            Some(x) => x,
            None => {
                span_err!(ctx.sess, lit_span, "integer literal is too large");
                return None;
            }
        };
    }
    println!("{}", value);
    Some(Lit { kind: LitKind::Int(value, ty), span: token.to_span() })
}


/**
 * Parses floating-point literal token.
 */
pub fn parse_float(
    ctx: &mut ParseCtxt,
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
        span_err!(ctx.sess, token.to_span(), "expected at least one digit in exponent");
        return None;
    }
    let suffix_span = Span::new(token.base + suffix, token.len - suffix);
    let lit_span = Span::new(token.base, suffix);
    let ty = if !suffix_span.is_empty() {
        let suffix = ctx.file.get_source(suffix_span);
        let symbol = ctx.sess.symbol_map.as_symbol(&suffix);
        match symbol {
            sym::f32 => LitFloatTy::Suffixed(FloatTy::F32),
            sym::f64 => LitFloatTy::Suffixed(FloatTy::F64),
            _ => {
                span_err!(ctx.sess, suffix_span, "invalid suffix expected `f32` or `f64`");
                LitFloatTy::Unsuffixed
            }
        }
    } else {
        LitFloatTy::Unsuffixed
    };
    match radix {
        Radix::Binary => {
            span_err!(ctx.sess, token.to_span(), "binary float literal is not supported");
            return None;
        },
        Radix::Hexadecimal => {
            span_err!(ctx.sess, token.to_span(), "hexadecimal float literal is not supported");
            return None;
        }
        Radix::Octal => {
            span_err!(ctx.sess, token.to_span(), "octal float literal is not supported");
            return None;
        }
        _ => ()
    };
    let input = ctx.file.get_source(lit_span);
    let (integral, input) = eat_digits(input.as_bytes());
    let mut value: f64 = parse_integral(&integral);
    match input.first() {
        None => {
            span_err!(ctx.sess, token.to_span(), "expected `e`, `E` or `.`, found nothing");
            return None;
        }
        Some(b'e') | Some(b'E') => {
            let (exponent, input) = parse_exponent(&input[1..]);
            debug_assert!(input.is_empty());
            if integral.is_empty() {
                span_err!(ctx.sess, token.to_span(), "expected at least one digit before exponent");
                return None;
            }
            value = parse_integral(&integral);
            value *= 10_f64.powf(exponent);
        }
        Some(b'.') => {
            let (fraction, input) = eat_digits(&input[1..]);
            if integral.is_empty() && fraction.is_empty() {
                span_err!(ctx.sess, 
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
            span_err!(ctx.sess, token.to_span(), "expected `e`, `E` or `.`, found `{}`", c);
            return None;
        }
    }
    println!("{:?}", value);
    Some(Lit { kind: LitKind::Float(value, ty), span: token.to_span() })
}


/**
 * Parses a character literal token.
 */
pub fn parse_character(
    ctx: &mut ParseCtxt,
    token: &Token,
    terminated: bool,
    suffix: usize
) -> Option<Lit> {
    if !terminated {
        span_err!(ctx.sess, Span::new(token.base + token.len, 1), "unterminated character literal");
        return None;
    }
    if suffix != token.len {
        invalid_suffix_err!(ctx, token, suffix, "character");
    }
    let span = Span::new(token.base, suffix);
    let mut chars = ctx.file.get_source(span).chars();
    assert!(chars.next() == Some('\''));

    let value: char = chars.next()?;
    match chars.next() {
        Some(c) => {
            if c != '\'' {
                span_err!(ctx.sess, span, "character literal may only contain one codepoint");
                return None;
            }
        }
        None => {
            return None;
        }
    }
    Some(Lit { kind: LitKind::Char(value), span })
}


/**
 * Parses a byte literal token.
 */
pub fn parse_byte(
    ctx: &mut ParseCtxt,
    token: &Token,
    terminated: bool,
    suffix: usize
) -> Option<Lit> {
    if !terminated {
        span_err!(ctx.sess, Span::new(token.base + token.len, 1), "unterminated byte literal");
        return None;
    }
    if suffix != token.len {
        invalid_suffix_err!(ctx, token, suffix, "byte");
    }
    let span = Span::new(token.base, suffix);
    let mut chars = ctx.file.get_source(span).chars();
    assert!(chars.next() == Some('b'));
    assert!(chars.next() == Some('\''));

    let value: u8 = chars.next()? as u8;
    match chars.next() {
        Some(c) => {
            if c != '\'' {
                span_err!(ctx.sess, span, "byte literal may only contain one codepoint");
                return None;
            }
        }
        None => {
            return None;
        }
    }
    Some(Lit { kind: LitKind::Byte(value), span })
}


/**
 * Parses a string literal token.
 */
pub fn parse_string(
    ctx: &mut ParseCtxt,
    token: &Token,
    terminated: bool,
    suffix: usize
) -> Option<Lit> {
    if !terminated {
        span_err!(ctx.sess, Span::new(token.base + token.len, 1), "unterminated string literal");
        return None;
    }
    if suffix != token.len {
        invalid_suffix_err!(ctx, token, suffix, "string");
    }

    let span = token.to_span();
    let input = ctx.file.get_source(span);
    assert!(&input[0..1] == "\"");

    let string = &input[1..suffix - 1];
    let symbol = ctx.sess.symbol_map.as_symbol(string);
    Some(Lit { kind: LitKind::Str(symbol, StrKind::Normal), span })
}


/**
 * Parses a byte string literal token.
 */
pub fn parse_byte_string(
    ctx: &mut ParseCtxt,
    token: &Token,
    terminated: bool,
    suffix: usize
) -> Option<Lit> {
    if !terminated {
        span_err!(ctx.sess, Span::new(token.base + token.len, 1), "unterminated byte string literal");
        return None;
    }
    if suffix != token.len {
        invalid_suffix_err!(ctx, token, suffix, "byte string");
    }

    let span = Span::new(token.base, suffix);
    let input = ctx.file.get_source(span);
    assert!(&input[0..2] == "b\"");
    let bytes = input[2..suffix - 1].as_bytes();
    Some(Lit { kind: LitKind::ByteStr(bytes.to_vec(), StrKind::Normal), span })
}


/**
 * Parses a raw string literal token.
 */
pub fn parse_raw_string(
    ctx: &mut ParseCtxt,
    token: &Token,
    num_hashes: usize,
    started: bool,
    terminated: bool,
    suffix: usize
) -> Option<Lit> {
    if !terminated {
        span_err!(ctx.sess, Span::new(token.base + token.len, 1), "unterminated raw string literal");
        return None;
    }
    if suffix != token.len {
        invalid_suffix_err!(ctx, token, suffix, "raw string");
    }
    let span = Span::new(token.base, suffix);
    let input = ctx.file.get_source(span);
    assert!(&input[0..1] == "r");
    assert!(&input[1..2 + num_hashes] == &"#".repeat(num_hashes));
    assert!(&input[1 + num_hashes..2 + num_hashes] == "\"");
    let string = &input[2 + num_hashes..suffix - num_hashes - 2];
    let symbol = ctx.sess.symbol_map.as_symbol(string);
    Some(Lit { kind: LitKind::Str(symbol, StrKind::Raw(num_hashes as u16)), span })
}


/**
 * Parses a raw byte string token.
 */
pub fn parse_raw_byte_string(
    ctx: &mut ParseCtxt,
    token: &Token,
    num_hashes: usize,
    started: bool,
    terminated: bool,
    suffix: usize
) -> Option<Lit> {
    if !terminated {
        span_err!(ctx.sess, Span::new(token.base + token.len, 1), "unterminated raw byte string literal");
        return None;
    }
    if suffix != token.len {
        invalid_suffix_err!(ctx, token, suffix, "raw byte string");
    }
    let span = Span::new(token.base, suffix);
    let input = ctx.file.get_source(span);
    let bytes = input[2 + num_hashes..suffix - num_hashes - 2].as_bytes();
    Some(Lit { kind: LitKind::ByteStr(bytes.to_vec(), StrKind::Raw(num_hashes as u16)), span })
}


