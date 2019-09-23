
/***************************************************************************
 * Tests for the atom parser
 ***************************************************************************/


use compiler::{
    ast::{Span, env::Env, atom::*},
    parser::Parser,
};
use nom_locate::LocatedSpanEx;


fn span<'a>(offset: usize, line: u32, frag: &'a str, env: &'a Env<'a>) -> Span<'a> {
    LocatedSpanEx{
        offset: offset,
        line: line,
        fragment: frag,
        extra: env,
    }
}




// fn parse_ident() {
//     assert_eq!(Ident::parse(Span::new("my_function_10")).unwrap().1,
//                Ident{to_string: "my_function_10", span: span(0, 1, "my_function_10")});
    
//     assert_eq!(Ident::parse(Span::new("__WINDOWS10__")).unwrap().1,
//                Ident{to_string: "__WINDOWS10__", span: span(0, 1, "__WINDOWS10__")});

//     assert!(Ident::parse(Span::new("123Testing")).is_err());
// }


#[test]
fn parse_val() {
    assert_eq!(Val::parse(Span::new_extra("234", &Env::new())).unwrap().1,
               Val::Num(LitInt{value:234, span: span(0, 1, "234", &Env::new())}));
    
    assert_eq!(Val::parse(Span::new_extra("234false", &Env::new())).unwrap(),
               (span(3, 1, "false", &Env::new()), Val::Num(LitInt{value:234, span: span(0, 1, "234", &Env::new())})));

    assert_eq!(Val::parse(Span::new_extra("true123", &Env::new())).unwrap(),
               (span(4, 1, "123", &Env::new()), Val::Bool(LitBool{value:true, span: span(0, 1, "true", &Env::new())})));
    
    assert_eq!(Val::parse(Span::new_extra("false123", &Env::new())).unwrap(),
               (span(5, 1, "123", &Env::new()), Val::Bool(LitBool{value:false, span: span(0, 1, "false", &Env::new())})));
}

#[test]
fn parse_lit_int() {
    assert_eq!(LitInt::parse(Span::new_extra("242", &Env::new())).unwrap(),
               (span(3, 1, "", &Env::new()), LitInt{value: 242, span: span(0, 1, "242", &Env::new())}));
    
    assert_eq!(LitInt::parse(Span::new_extra("242abc", &Env::new())).unwrap(),
               (span(3, 1, "abc", &Env::new()), LitInt{value: 242, span: span(0, 1, "242", &Env::new())}));
    
    assert!(LitInt::parse(Span::new_extra("1111111111111111", &Env::new())).is_err());
}


#[test]
fn parse_lit_bool() {
    assert_eq!(LitBool::parse(Span::new_extra("true", &Env::new())).unwrap(),
               (span(4, 1, "", &Env::new()), LitBool{value: true, span: span(0, 1, "true", &Env::new())}));
    
    assert_eq!(LitBool::parse(Span::new_extra("false", &Env::new())).unwrap(),
               (span(5, 1, "", &Env::new()), LitBool{value: false, span: span(0, 1, "false", &Env::new())}));
    
    assert_eq!(LitBool::parse(Span::new_extra("false123", &Env::new())).unwrap(),
               (span(5, 1, "123", &Env::new()), LitBool{value: false, span: span(0, 1, "false", &Env::new())}));
    
    assert!(LitBool::parse(Span::new_extra("123true", &Env::new())).is_err());
}
