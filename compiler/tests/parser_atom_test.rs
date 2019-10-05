
/***************************************************************************
 * Tests for the atom parser
 ***************************************************************************/


use crate::compiler::{
    ast::atom::*,
    ast::Span,
    parser::Parser,
};
use nom_locate::LocatedSpanEx;


/**
 * Easily define span information to test whats left after parsing.
 */
fn span<'a>(offset: usize, frag: &'a str) -> Span<'a> {
    LocatedSpanEx{
        offset: offset,
        line: 1,
        fragment: frag,
        extra: (),
    }
}


// #[test]
// fn parse_fn_call() {
    // assert_eq!(FnCall::parse(Span::new("my_function_10(100, 32)")).unwrap().1,
               // FnCall{ident: "my_function_10", span: span(0, "my_function_10")});
// }


#[test]
fn parse_ident() {
    assert_eq!(Ident::parse(Span::new("my_function_10")).unwrap().1,
               Ident{to_string: "my_function_10", span: span(0, "my_function_10")});
    
    assert_eq!(Ident::parse(Span::new("__WINDOWS10__")).unwrap().1,
               Ident{to_string: "__WINDOWS10__", span: span(0, "__WINDOWS10__")});

    assert!(Ident::parse(Span::new("123Testing")).is_err());
}


#[test]
fn parse_lit_int() {
    assert_eq!(LitInt::parse(Span::new("242")).unwrap(),
               (span(3, ""), LitInt{value: 242, span: span(0, "242")}));
    
    assert_eq!(LitInt::parse(Span::new("242abc")).unwrap(),
               (span(3, "abc"), LitInt{value: 242, span: span(0, "242")}));
    
    assert!(LitInt::parse(Span::new("1111111111111111")).is_err());
}


#[test]
fn parse_lit_bool() {
    assert_eq!(LitBool::parse(Span::new("true")).unwrap(),
               (span(4, ""), LitBool{value: true, span: span(0, "true")}));
    
    assert_eq!(LitBool::parse(Span::new("false")).unwrap(),
               (span(5, ""), LitBool{value: false, span: span(0, "false")}));
    
    assert_eq!(LitBool::parse(Span::new("false123")).unwrap(),
               (span(5, "123"), LitBool{value: false, span: span(0, "false")}));
    
    assert!(LitBool::parse(Span::new("123true")).is_err());
}
