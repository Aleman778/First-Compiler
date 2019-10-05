
/***************************************************************************
 * Tests for expression parser
 ***************************************************************************/


use crate::compiler::{
    ast::atom::*,
    ast::expr::*,
    ast::Span,
    parser::Parser,
};
use nom_locate::LocatedSpanEx;

/*
/**
 * Easily define span information to test whats left after parsing.
 */
fn span<'a>(offset: usize, frag: &'a str) -> Span<'a> {
    LocatedSpanEx {
        offset: offset,
        line: 1,
        fragment: frag,
        extra: (),
    }
}


#[test]
fn parse_binop() {
    assert_eq!(
        BinOp::parse(Span::new("1 + 2")).unwrap().1,
        BinOp {
            left: Box::new(Atom::Num(LitInt{value: 1, span: span(0, "1")})),
            op: Op::Add,
            right: Box::new(Expr::Atom(Atom::Num(LitInt{value: 2, span: span(4, "2")}))),
            span: span(0, "1 + 2"),
        }
    );


    assert_eq!(
        BinOp::parse(Span::new("true || false")).unwrap().1,
        BinOp {
            left: Box::new(Atom::Bool(LitBool{value: true, span: span(0, "true")})),
            op: Op::Or,
            right: Box::new(Expr::Atom(Atom::Bool(LitBool{value: false, span: span(9, "false")}))),
            span: span(0, "true || false"),
        }
    );
}


#[test]
fn parse_unop() {
    assert_eq!(
        UnOp::parse(Span::new("-42")).unwrap().1,
        UnOp {
            op: Op::Sub,
            right: Box::new(Expr::Atom(Atom::Num(LitInt{value: 42, span: span(1, "42")}))),
            span: span(0, "-42"),
        }
    );

    assert_eq!(
        UnOp::parse(Span::new("!false")).unwrap().1,
        UnOp {
            op: Op::Not,
            right: Box::new(Expr::Atom(Atom::Bool(LitBool{value: false, span: span(1, "false")}))),
            span: span(0, "!false"),
        }
    );
}


#[test]
fn parse_local() {
    assert_eq!(
        Local::parse(Span::new("let mut a: i32 = 5;")).unwrap().1,
        Local {
            mutable: true,
            ident: Ident{to_string: "a", span: span(8, "a")},
            ty: Type::Int32,
            init: Atom::Num(LitInt(5)),
            span: span(0, "let mut a: i32 = 5;"),
        }
    );
}


#[test]
fn parse_assign() {
    assert_eq!(
        Assign::parse(Span::new("x = x + 5")).unwrap().1,
        Assign {
            ident: Ident{to_string: "x", span(0, "x")},
            expr: BinOp{left: Box::new(Ident}
        }
    );
}


#[test]
fn parse_block() {

}


#[test]
fn parse_if() {
    
}


#[test]
fn parse_while() {

}


#[test]
fn parse_return() {

}


#[test]
fn parse_break() {

}


#[test]
fn parse_continue() {
    
}


#[test]
fn parse_atom() {
    assert_eq!(Atom::parse(Span::new("234")).unwrap().1,
               Atom::Num(LitInt{value:234, span: span(0, "234")}));
    
    assert_eq!(Atom::parse(Span::new("234false")).unwrap(),
               (span(3, "false"), Atom::Num(LitInt{value:234, span: span(0, "234")})));

    assert_eq!(Atom::parse(Span::new("true123")).unwrap(),
               (span(4, "123"), Atom::Bool(LitBool{value:true, span: span(0, "true")})));
    
    assert_eq!(Atom::parse(Span::new("false123")).unwrap(),
               (span(5, "123"), Atom::Bool(LitBool{value:false, span: span(0, "false")})));
}
