
/***************************************************************************
 * Tests for expression parsers
 ***************************************************************************/


extern crate compiler;


mod helper;
use helper::span::*;
use helper::ast::*;
use crate::compiler::{
    ast::{
        op::*,
        lit::*,
         nexpr::*,
    },
    parser::Parser,
};


#[test]
fn parse_binop() {
    assert_eq!(
        ExprBinary::parse(input("1 + 2")).unwrap().1,
        ExprBinary {
            left: Box::new(Expr::Lit(expr_lit_int(1, span(0, "1")))),
            op: BinOp::Add,
            right: Box::new(Expr::Lit(expr_lit_int(2, span(4, "2")))),
            span: span(0, "1 + 2"),
        }
    );


    assert_eq!(
        ExprBinary::parse(input("true || false")).unwrap().1,
        ExprBinary {
            left: Box::new(Expr::Lit(ExprLit {
                lit: Lit::Bool(LitBool{value: true, span: span(0, "true")}),
                span: span(0, "true"),
            })),
            op: BinOp::Or,
            right: Box::new(Expr::Lit(ExprLit {
                lit: Lit::Bool(LitBool{value: true, span: span(0, "true")}),
                span: span(0, "true"),
            })),
            span: span(0, "true || false"),
        }
    );
}


#[test]
fn parse_unop() {
    assert_eq!(
        ExprUnary::parse(input("-42")).unwrap().1,
        ExprUnary {
            op: UnOp::Sub,
            right: Box::new(Expr::Atom(Atom::Num(LitInt{value: 42, span: span(1, "42")}))),
            span: span(0, "-42"),
        }
    );

    assert_eq!(
        ExprUnary::parse(input("!false")).unwrap().1,
        ExprUnary {
            op: UnOp::Not,
            right: Box::new(Expr::Atom(Atom::Bool(LitBool{value: false, span: span(1, "false")}))),
            span: span(0, "!false"),
        }
    );
}


#[test]
fn parse_local() {
    assert_eq!(
        Local::parse(input("let mut a: i32 = 5;")).unwrap().1,
        Local {
            mutable: true,
            ident: ExprIdent{to_string: "a", span: span(8, "a")},
            ty: Type::Int32,
            init: Atom::Num(LitInt(5)),
            span: span(0, "let mut a: i32 = 5;"),
        }
    );
}


#[test]
fn parse_assign() {
    assert_eq!(
        Assign::parse(input("x = x + 5")).unwrap().1,
        Assign {
            ident: ExprIdent{to_string: "x", span: span(0, "x")},
            expr: BinOp {
                left: Box::new(Expr::Ident(ExprIdent {
                    to_string: "x",
                    span: span(0, "x")
                })),
                op: BinOp::Add,
                right: Box::new(Expr::Lit(ExprLit{
                    lit: Lit::Int(LitInt{value: 5, span: span(8, "5")}),
                    span: span(),
                })),
                span: span(4, "x + 5"),
            },
            span: span("x = x + 5"),
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
fn parse_fn_call() {
    assert_eq!(FnCall::parse(Span::new("my_function_10(100, 32)")).unwrap().1,
               FnCall{ident: "my_function_10", span: span(0, "my_function_10")});
}


#[test]
fn parse_ident() {
    assert_eq!(ExprIdent::parse(ParseSpan::new("my_function_10")).unwrap().1,
               ExprIdent {
                   to_string: "my_function_10".to_string(),
                   span: span(0, "my_function_10")
               }
    );
    
    assert_eq!(ExprIdent::parse(ParseSpan::new("__WINDOWS10__")).unwrap().1,
               ExprIdent {
                   to_string: "__WINDOWS10__".to_string(),
                   span: span(0, "__WINDOWS10__")
               }
    );

    assert!(ExprIdent::parse(ParseSpan::new("123Testing")).is_err());
}


#[test]
fn parse_lit() {
    assert_eq!(ExprLit::parse(input("234")).unwrap().1,
               ExprLit {
                   lit: Lit::Int(LitInt{value:234, span: span(0, "234")}),
                   span: span()
               }
    );
    
    assert_eq!(ExprLit::parse(input("234false")).unwrap(),
               (output(3, "false"), ExprLit {
                   lit: Lit::Int(LitInt{value:234, span: span(0, "234")}),
                   span: span()
               })
    );

    assert_eq!(ExprLit::parse(input("true")).unwrap().1,
               ExprLit {
                   lit: Lit::Bool(LitBool{value:true, span: span(0, "true")}),
                   span: span(),
               }
    );
    
    assert_eq!(ExprLit::parse(input("false123")).unwrap(),
               (output(5, "123"), ExprBool{
                   lit: Lit::Bool(LitBool{value:false, span: span(0, "false")}),
                   span: span(),
               })
    );
}
