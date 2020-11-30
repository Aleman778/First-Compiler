use crate::ast::*;
use crate::interp::InterpContext;

// FIXME(alexander): everything here is temporary!!1
pub fn get_intrinsic_ast_items() -> Item {
    Item::ForeignMod(
        ForeignModItem {
            abi: Some("intrinsic".to_string()),
            items: vec![
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            to_string: "trace".to_string(),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![],
                            output: Ty {
                                kind: TyKind::None,
                                span: Span::new(),
                            },
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            to_string: "print_int".to_string(),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "val".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Int,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                            ],
                            output: Ty {
                                kind: TyKind::None,
                                span: Span::new(),
                            },
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            to_string: "print_bool".to_string(),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "val".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Bool,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                            ],
                            output: Ty {
                                kind: TyKind::None,
                                span: Span::new(),
                            },
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            to_string: "assert".to_string(),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "val".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Bool,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                            ],
                            output: Ty {
                                kind: TyKind::None,
                                span: Span::new(),
                            },
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            to_string: "assert_eq_int".to_string(),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "left".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Int,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "right".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Int,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                            ],
                            output: Ty {
                                kind: TyKind::None,
                                span: Span::new(),
                            },
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            to_string: "assert_eq_bool".to_string(),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "left".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Bool,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        to_string: "right".to_string(),
                                        span: Span::new(),
                                    },
                                    ty: Ty {
                                        kind: TyKind::Bool,
                                        span: Span::new(),
                                    },
                                    span: Span::new(),
                                },
                            ],
                            output: Ty {
                                kind: TyKind::None,
                                span: Span::new(),
                            },
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
            ],
            span: Span::new(),
        },
    )
}

/**
 * Prints the environment
 */
pub fn trace(ic: &mut InterpContext) {
    println!("{:#?}", ic);
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
