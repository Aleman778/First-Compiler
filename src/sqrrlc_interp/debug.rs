
/***************************************************************************
 * Defines rust (foreign) functions for debugging interpreted programs.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    expr::*,
    base::*,
    lit::*,
    ty::*,
};
use crate::sqrrlc_interp::{
    env::RuntimeEnv,
};


pub fn debug_functions() -> File {
    File {
        items: vec![
            Item::ForeignMod(
                ForeignModItem {
                    abi: Some(
                        LitStr {
                            value: "Rust".to_string(),
                            span: Span::new_empty(),
                        },
                    ),
                    items: vec![
                        ForeignFnItem {
                            ident: ExprIdent {
                                to_string: "trace".to_string(),
                                span: Span::new_empty(),
                            },
                            decl: FnDecl {
                                inputs: vec![],
                                output: Ty {
                                    kind: TyKind::None,
                                    span: Span::new_empty(),
                                },
                                span: Span::new_empty(),
                            },
                            span: Span::new_empty(),
                        },
                        ForeignFnItem {
                            ident: ExprIdent {
                                to_string: "print_int".to_string(),
                                span: Span::new_empty(),
                            },
                            decl: FnDecl {
                                inputs: vec![
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "val".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Int(
                                                IntTy::I32,
                                            ),
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                ],
                                output: Ty {
                                    kind: TyKind::None,
                                    span: Span::new_empty(),
                                },
                                span: Span::new_empty(),
                            },
                            span: Span::new_empty(),
                        },
                        ForeignFnItem {
                            ident: ExprIdent {
                                to_string: "print_bool".to_string(),
                                span: Span::new_empty(),
                            },
                            decl: FnDecl {
                                inputs: vec![
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "val".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Bool,
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                ],
                                output: Ty {
                                    kind: TyKind::None,
                                    span: Span::new_empty(),
                                },
                                span: Span::new_empty(),
                            },
                            span: Span::new_empty(),
                        },
                        ForeignFnItem {
                            ident: ExprIdent {
                                to_string: "assert".to_string(),
                                span: Span::new_empty(),
                            },
                            decl: FnDecl {
                                inputs: vec![
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "val".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Bool,
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                ],
                                output: Ty {
                                    kind: TyKind::None,
                                    span: Span::new_empty(),
                                },
                                span: Span::new_empty(),
                            },
                            span: Span::new_empty(),
                        },
                        ForeignFnItem {
                            ident: ExprIdent {
                                to_string: "assert_eq_int".to_string(),
                                span: Span::new_empty(),
                            },
                            decl: FnDecl {
                                inputs: vec![
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "left".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Int(
                                                IntTy::I32,
                                            ),
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "right".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Int(
                                                IntTy::I32,
                                            ),
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                ],
                                output: Ty {
                                    kind: TyKind::None,
                                    span: Span::new_empty(),
                                },
                                span: Span::new_empty(),
                            },
                            span: Span::new_empty(),
                        },
                        ForeignFnItem {
                            ident: ExprIdent {
                                to_string: "assert_eq_bool".to_string(),
                                span: Span::new_empty(),
                            },
                            decl: FnDecl {
                                inputs: vec![
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "left".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Bool,
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                    Argument {
                                        mutable: false,
                                        ident: ExprIdent {
                                            to_string: "right".to_string(),
                                            span: Span::new_empty(),
                                        },
                                        ty: Ty {
                                            kind: TyKind::Bool,
                                            span: Span::new_empty(),
                                        },
                                        span: Span::new_empty(),
                                    },
                                ],
                                output: Ty {
                                    kind: TyKind::None,
                                    span: Span::new_empty(),
                                },
                                span: Span::new_empty(),
                            },
                            span: Span::new_empty(),
                        },
                    ],
                    span: Span::new_empty(),
                },
            ),
        ],
        span: Span::new_empty(),
    }
}

/**
 * Prints the environment
 */
pub fn trace(env: &mut RuntimeEnv) {
    println!("{:#?}", env);
}


/**
 * Prints the given integer.
 */
pub fn print_int(val: i32) {
    println!("{}", val);
}


/**
 * Prints the given boolean.
 */
pub fn print_bool(val: bool) {
    println!("{}", val);
}


/**
 * Simple equals assertion.
 */
pub fn assert(val: bool) {
    assert!(val);
}

/**
 * Simple equals assertion.
 */
pub fn assert_eq_int(left: i32, right: i32) {
    assert_eq!(left, right);
}


/**
 * Simple equals assertion.
 */
pub fn assert_eq_bool(left: bool, right: bool) {
    assert_eq!(left, right);
}
