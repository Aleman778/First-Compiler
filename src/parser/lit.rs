//! Parser implementation for literals.


use std::str::Chars;
use crate::core::span::{symbol::sym, Span};
use crate::parser::ParseCtxt;
use crate::lexer::tokens::{Token, Radix};
use crate::ast::*;


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
        value = match value.checked_mul(radix_value as u128) {
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
    println!("{:#?}", token);
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
    
    let mut value: char = chars.next()?;
    if value == '\\' {
        value = parse_escape_character(ctx, token, &mut chars, false)?;
    }

    if chars.next()? != '\'' {
        span_err!(ctx.sess, span, "byte literal may only contain one codepoint");
        return None;
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

    let mut value: u8 = chars.next()? as u8;
    if value == b'\\' {
        value = parse_escape_character(ctx, token, &mut chars, true)? as u8;
    }

    if chars.next()? != '\'' {
        span_err!(ctx.sess, span, "byte literal may only contain one codepoint");
        return None;
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
    _started: bool,
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
    _started: bool,
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


/**
 * Parses an escape character from either char or byte literal token.
 */
fn parse_escape_character(
    ctx: &mut ParseCtxt,
    token: &Token,
    chars: &mut Chars,
    byte: bool,
) -> Option<char> {
    match chars.next()? {
        // ASCII escape character
        'x' => {
            let value: u8 = match chars.next()?.to_digit(7) {
                Some(x) => x as u8,
                None => {
                    span_err!(ctx.sess, 
                              token.to_span(),
                              "escape character out of range, expected [\0x00 - \0x7F]");
                    return None;
                }
            }; 
            let value = if let Some(c) = chars.next() {
                match c.to_digit(16) {
                    Some(x) => value * 16 + x as u8,
                    None => {
                        span_err!(ctx.sess, 
                                  token.to_span(),
                                  "escape character out of range, expected [\0x00 - \0x7F]");
                        return None;
                    }
                }
            } else {
                value
            };
            Some(value as char)
        },
 
        'n'  => Some('\n'),
        't'  => Some('\t'),
        'r'  => Some('\r'),
        '\\' => Some('\\'),
        '0'  => Some('\0'),
        
        // Unicode escape character
        'u'  => {
            if byte {
                span_err!(ctx.sess, 
                          token.to_span(),
                          "unicode escape characters cannot be used as bytes or in byte string");
                return None;
            }

            assert!(chars.next() == Some('{'));
            let mut value: u32 = 0;
            let mut c = chars.next()?;
            if !c.is_digit(16) {
                span_err!(ctx.sess, 
                          token.to_span(), 
                          "empty unicode escape, expected at least one hex digit");
                return None;
            }

            loop {
                value = value * 16 + c. to_digit(16)?;
                c = chars.next()?;
                if !c.is_digit(16) {
                    break;
                }
            }

            if value > 0x10FFFF {
                span_err!(ctx.sess,
                          token.to_span(),
                          "invalid unicode escape character, expected at most 10FFFF");
                return None;
            }
            assert!(c == '}');
            std::char::from_u32(value)
        },

        // Quote sscape characters.
        '\'' => Some('\''),
        '"'  => Some('"'),
        _ => {
            span_err!(ctx.sess, token.to_span(), "invalid escape character");
            None
        }
    }
}



#[cfg(test)]
/// Unit testing of the different literal parsers.
mod tests {
    use crate::lexer::tokenize;
    use crate::ast::*;
    use crate::ast::map::AstMap;
    use crate::core::session::Session;
    use crate::core::source_map::Filename;
    use crate::parser::expr::parse_expr;
    use crate::parser::ParseCtxt;


    macro_rules! test {
        ($func:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $func() {
                let mut sess = Session::new();
                let fname = Filename::Custom("test".to_string());
                let file = sess.source_map().insert_source_file(fname, $input.to_string());
                let tokens = tokenize(&file.source, file.start_pos.index());
                let mut ctx = ParseCtxt {
                    sess: &mut sess,
                    file: &file,
                    tokens: tokens.peekable(),
                    ast_map: AstMap::new(),
                };
                let actual = if let Some(expr) = parse_expr(&mut ctx) {
                    if let ExprKind::Lit(lit) = expr.kind {
                        Some((*lit).kind)
                    } else {
                        None
                    }
                } else {
                    None
                };
                assert_eq!(actual, $expected);
            }
        };
    }

    // Testing decimal integer of different types
    test!(parse_int_dec_usuffixed, "42", 
          Some(LitKind::Int(42, LitIntTy::Unsuffixed)));

    test!(parse_int_dec_unsigned, "16u8", 
          Some(LitKind::Int(16, LitIntTy::Unsigned(UIntTy::U8))));

    test!(parse_int_dec_signed, "534__34_24__i64", 
          Some(LitKind::Int(5343424, LitIntTy::Signed(IntTy::I64))));

    // Testing binary integers of different types
    test!(parse_int_bin_unsuffixed, "0b101010111110", 
          Some(LitKind::Int(2750, LitIntTy::Unsuffixed)));

    test!(parse_int_bin_unsigned, "0b11__00u16", 
          Some(LitKind::Int(12, LitIntTy::Unsigned(UIntTy::U16))));

    test!(parse_int_bin_signed, "0b11110000isize", 
          Some(LitKind::Int(240, LitIntTy::Signed(IntTy::ISize))));

    // Testing octal integers of different types
    test!(parse_int_oct_unsuffixed, "0o325", 
          Some(LitKind::Int(213, LitIntTy::Unsuffixed)));

    test!(parse_int_oct_unsigned, "0o123__456__u128", 
          Some(LitKind::Int(42798, LitIntTy::Unsigned(UIntTy::U128))));

    test!(parse_int_oct_signed, "0o534________i16", 
          Some(LitKind::Int(348, LitIntTy::Signed(IntTy::I16))));

    // Testing floating-point values of different types and notations
    test!(parse_float_fraction, "129.521",
          Some(LitKind::Float(129.521, LitFloatTy::Unsuffixed)));

    test!(parse_float_exponent, "3___2e-4__f32",
          Some(LitKind::Float(0.0032, LitFloatTy::Suffixed(FloatTy::F32))));

    test!(parse_flaot_both, "420.50E+2_f64",
          Some(LitKind::Float(42050.0, LitFloatTy::Suffixed(FloatTy::F64))));

    test!(parse_float_empty_fraction, "112.",
          Some(LitKind::Float(112.0, LitFloatTy::Unsuffixed)));
    
    // Testing characters
    test!(parse_character, "'a'", 
          Some(LitKind::Char('a')));

    test!(parse_single_quote_character, r"'\''", 
          Some(LitKind::Char('\'')));

    test!(parse_newline_character, r"'\n'", 
          Some(LitKind::Char('\n')));

    test!(parse_ascii_escape_character, r"'\x37'", 
          Some(LitKind::Char('7')));

    test!(parse_unicode_escape_character, r"\u{534}", 
          Some(LitKind::Char('\u{534}')));

    // Testing bytes
    test!(parse_character_byte, "b'a'", 
          Some(LitKind::Byte(97u8)));

    test!(parse_single_quote_character_byte, r"b'\''", 
          Some(LitKind::Byte(39u8)));

    test!(parse_newline_character_byte, r"b'\n'",
          Some(LitKind::Byte(10u8)));

    test!(parse_ascii_escape_character_byte, r"b'\x37'",
          Some(LitKind::Byte(55u8)));

    test!(parse_unicode_escape_character_byte, r"\u{534}", None);
}
