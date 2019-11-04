
/***************************************************************************
 * Tests for literal parsers
 ***************************************************************************/


use utilities::span::*;
use compiler::{
    ast::lit::*,
    parser::Parser,
};


#[test]
fn parse_lit_int() {
    assert_eq!(LitInt::parse(input("242")).unwrap(),
               (output(3, ""), LitInt{value: 242, span: span(0, "242")}));
    
    assert_eq!(LitInt::parse(input("242abc")).unwrap(),
               (output(3, "abc"), LitInt{value: 242, span: span(0, "242")}));
    
    assert!(LitInt::parse(input("1111111111111111")).is_err());
}


#[test]
fn parse_lit_bool() {
    assert_eq!(LitBool::parse(input("true")).unwrap(),
               (output(4, ""), LitBool{value: true, span: span(0, "true")}));
    
    assert_eq!(LitBool::parse(input("false")).unwrap(),
               (output(5, ""), LitBool{value: false, span: span(0, "false")}));
    
    assert_eq!(LitBool::parse(input("false123")).unwrap(),
               (output(5, "123"), LitBool{value: false, span: span(0, "false")}));
    
    assert!(LitBool::parse(input("123true")).is_err());
}
