
/***************************************************************************
 * Tests for expression parsers
 ***************************************************************************/


use utilities::span::*;
use utilities::ast::*;
use compiler::{
    ast::{op::*, lit::*, expr::*, base::Type},
    parser::Parser,
    parser::error::convert_error,
};
use nom::Err;


#[test]
fn parse_assign() {
    assert_eq!(
        ExprAssign::parse(input("x = x + 5;")).unwrap().1,
        ExprAssign {
            ident: ExprIdent{to_string: "x".to_string(), span: span(0, "x")},
            expr: Box::new(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Ident(ExprIdent {
                    to_string: "x".to_string(),
                    span: span(4, "x"),
                })),
                op: BinOp::Add{span: span(6, "+")},
                right: Box::new(Expr::Lit(expr_lit_int(5, span(8, "5")))),
                span: span(4, "x + 5"),
            })),
            span: span(0, "x = x + 5;"),
        }
    );
}


#[test]
fn parse_binary() {
    assert_eq!(
        ExprBinary::parse(input("1 + 2")).unwrap().1,
        Expr::Binary(ExprBinary {
            left: Box::new(Expr::Lit(expr_lit_int(1, span(0, "1")))),
            op: BinOp::Add{span: span(2, "+")},
            right: Box::new(Expr::Lit(expr_lit_int(2, span(4, "2")))),
            span: span(0, "1 + 2"),
        })
    );


    assert_eq!(
        ExprBinary::parse(input("true || false")).unwrap().1,
        Expr::Binary(ExprBinary {
            left: Box::new(Expr::Lit(ExprLit {
                lit: Lit::Bool(LitBool{value: true, span: span(0, "true")}),
                span: span(0, "true"),
            })),
            op: BinOp::Or{span: span(5, "||")},
            right: Box::new(Expr::Lit(ExprLit {
                lit: Lit::Bool(LitBool{value: true, span: span(0, "true")}),
                span: span(0, "true"),
            })),
            span: span(0, "true || false"),
        })
    );
}


#[test]
fn parse_block() {
    assert_eq!(
        ExprBlock::parse(input("{ { a } { b } }")).unwrap().1,
        ExprBlock {
            stmts: vec![
                Expr::Block(ExprBlock {
                    stmts: vec![
                        Expr::Return(ExprReturn {
                            expr: Box::new(Some(Expr::Ident(ExprIdent {
                                to_string: "a".to_string(),
                                span: span(4, "a"),
                            }))),
                            span: span(4, "a"),
                        }),
                    ],
                    span: span(2, "{ a }"),
                }),
                Expr::Block(ExprBlock {
                    stmts: vec![
                        Expr::Return(ExprReturn {
                            expr: Box::new(Some(Expr::Ident(ExprIdent {
                                to_string: "a".to_string(),
                                span: span(10, "b"),
                            }))),
                            span: span(10, "b"),
                        }),
                    ],
                    span: span(8, "{ b }"),
                }),
            ],
            span: span(0, "{ { a } { b } }"),
        }
    );
}


#[test]
fn parse_break() {
    assert_eq!(
        ExprBreak::parse(input("break;")).unwrap().1,
        ExprBreak {
            span: span(0, "break;"),
        }                
    );
}


#[test]
fn parse_call() {
    assert_eq!(
        ExprCall::parse(input("my_function_10(100, 32);")).unwrap().1,
        ExprCall {
            ident: ExprIdent {
                to_string: "my_function_10".to_string(),
                span: span(0, "my_function_10"),
            },
            args: vec![
                Expr::Lit(expr_lit_int(100, span(15, "100"))),
                Expr::Lit(expr_lit_int(32, span(20, "32"))),
            ],
            span: span(0, "my_function_10(100, 32);"),
        }
    );
}


#[test]
fn parse_continue() {
    assert_eq!(
        ExprContinue::parse(input("continue;")).unwrap().1,
        ExprContinue {
            span: span(0, "continue;"),
        }                
    );    
}


#[test]
fn parse_ident() {
    assert_eq!(
        ExprIdent::parse(input("my_function_10")).unwrap().1,
        ExprIdent {
            to_string: "my_function_10".to_string(),
            span: span(0, "my_function_10"),
        }
    );
    
    assert_eq!(
        ExprIdent::parse(input("__WINDOWS10__")).unwrap().1,
        ExprIdent {
            to_string: "__WINDOWS10__".to_string(),
            span: span(0, "__WINDOWS10__"),
        }
    );

    assert!(ExprIdent::parse(input("123Testing")).is_err());
}


#[test]
fn parse_if() {
    assert_eq!(
        ExprIf::parse(input("if a > 5 { true } else { false }")).unwrap().1,
        ExprIf {
            cond: Box::new(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Ident(ExprIdent {
                    to_string: "a".to_string(),
                    span: span(3, "a"),
                })),
                op: BinOp::Gt{span: span(5, ">")},
                right: Box::new(Expr::Lit(expr_lit_int(5, span(7, "5")))),
                span: span(3, "a > 5"),
            })),
            then_block: ExprBlock{
                stmts: vec![
                    Expr::Return(ExprReturn {
                        expr: Box::new(Some(Expr::Lit(expr_lit_bool(true, span(9, "true"))))),
                        span: span(9, "true"),
                    }),
                ],
                span: span(9, "{ true }"),
            },
            else_block: ExprBlock{
                stmts: vec![
                    Expr::Return(ExprReturn {
                        expr: Box::new(Some(Expr::Lit(expr_lit_bool(false, span(23, "false"))))),
                        span: span(23, "false"),
                    }),
                ],
                span: span(9, "{ true }"),
            },
            span: span(0, "if a > 5 { true } else { false }"),
        }
    );
}


#[test]
fn parse_lit() {
    assert_eq!(
        ExprLit::parse(input("234")).unwrap().1,
        expr_lit_int(234, span(0, "234"))
    );
    
    assert_eq!(
        ExprLit::parse(input("234false")).unwrap(),
        (output(3, "false"), expr_lit_int(234, span(0, "234")))
    );

    assert_eq!(
        ExprLit::parse(input("true")).unwrap().1,
        expr_lit_bool(true, span(0, "true"))
    );
    
    assert_eq!(
        ExprLit::parse(input("false123")).unwrap(),
               (output(5, "123"), expr_lit_bool(false, span(0, "false")))
    );
}


#[test]
fn parse_local() {
    assert_eq!(
        ExprLocal::parse(input("let mut a: i32 = 5;")).unwrap().1,
        ExprLocal {
            mutable: true,
            ident: ExprIdent{to_string: "a".to_string(), span: span(8, "a")},
            ty: Type::Int32{span: span(11, "i32")},
            init: Box::new(Expr::Lit(expr_lit_int(5, span(17, "5")))),
            span: span(0, "let mut a: i32 = 5;"),
        }
    );
}


#[test]
fn parse_paren() {
    
}


#[test]
fn parse_return() {

}


#[test]
fn parse_unop() {
    assert_eq!(
        ExprUnary::parse(input("-42")).unwrap().1,
        ExprUnary {
            op: UnOp::Neg{span: span(0, "-")},
            right: Box::new(Expr::Lit(expr_lit_int(42, span(1, "42")))),
            span: span(0, "-42"),
        }
    );

    assert_eq!(
        ExprUnary::parse(input("!false")).unwrap().1,
        ExprUnary {
            op: UnOp::Not{span: span(0, "!")},
            right: Box::new(Expr::Lit(expr_lit_bool(false, span(1, "false")))),
            span: span(0, "!false"),
        }
    );
}


#[test]
fn parse_while() {

}
