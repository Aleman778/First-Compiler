
/***************************************************************************
 * Tests for base parsers
 ***************************************************************************/


use utilities::span::*;
use sqrrl::sqrrlc_ast::{
    stmt::*,
    expr::*,
    base::*,
    ty::*,
};
use sqrrl::sqrrlc_parser::Parser;


#[test]
fn parse_fn_item() {
    assert_eq!(
        FnItem::parse(input("fn hello_world(id: i32) -> bool {  }  ")).unwrap().1,
        FnItem {
            ident: ExprIdent {
                to_string: "hello_world".to_string(),
                span: span(3, "hello_world"),
            },
            decl: FnDecl {
                inputs: vec![
                    Argument {
                        mutable: false,
                        ident: ExprIdent {
                            to_string: "id".to_string(),
                            span: span(15, "id"),
                        },
                        ty: Ty{kind: TyKind::Int(IntTy::I32), span: span(19, "i32")},
                        span: span(15, "id: i32"),
                    },
                ],
                output: Ty{kind: TyKind::Bool, span: span(27, "bool")},
                span: span(14, "(id: i32) -> bool"),
            },
            block: Block {
                stmts: vec![],
                span: span(32, "{  }"),
            },
            span: span(0, "fn hello_world(id: i32) -> bool {  }"),
        }
    )
}


#[test]
fn parse_fn_decl() {
    assert_eq!(
        FnDecl::parse(input("(val: i32, b: bool) -> i32  ")).unwrap().1,
        FnDecl {
            inputs: vec![
                Argument {
                    mutable: false,
                    ident: ExprIdent {
                        to_string: "val".to_string(),
                        span: span(1, "val"),
                    },
                    ty: Ty{kind: TyKind::Int(IntTy::I32), span: span(6, "i32")},
                    span: span(1, "val: i32"),
                },
                Argument {
                    mutable: false,
                    ident: ExprIdent {
                        to_string: "b".to_string(),
                        span: span(11, "b"),
                    },
                    ty: Ty{kind: TyKind::Bool, span: span(14, "bool")},
                    span: span(11, "b: bool"),
                },
            ],
            output: Ty{kind: TyKind::Int(IntTy::I32), span: span(23, "i32")},
            span: span(0, "(val: i32, b: bool) -> i32"),
        }
    )
}


#[test]
fn parse_argument() {
    assert_eq!(
        Argument::parse(input("  a:i32  ")).unwrap().1,
        Argument {
            mutable: false,
            ident: ExprIdent {
                to_string: "a".to_string(),
                span: span(2, "a"),
            },
            ty: Ty{kind: TyKind::Int(IntTy::I32), span: span(4, "i32")},
            span: span(2, "a:i32"),
        }       
    );

    
    assert_eq!(
        Argument::parse(input("  mut  b  :  bool  ")).unwrap().1,
        Argument {
            mutable: true,
            ident: ExprIdent {
                to_string: "b".to_string(),
                span: span(7, "b"),
            },
            ty: Ty{kind: TyKind::Bool, span: span(13, "bool")},
            span: span(2, "mut  b  :  bool"),
        }       
    );
}


#[test]
fn parse_type() {
    assert_eq!(Ty::parse(input("  i32  ")).unwrap().1,
               Ty{kind: TyKind::Int(IntTy::I32), span: span(2, "i32")});
    assert_eq!(Ty::parse(input("  bool  ")).unwrap().1, 
               Ty{kind: TyKind::Bool, span: span(2, "bool")});
}
