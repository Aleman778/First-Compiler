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
                            sym: intern_string("trace"),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![],
                            output: Ty::default(),
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            sym: intern_string("print_int"),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("val"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Int, Span::new()),
                                    span: Span::new(),
                                },
                            ],
                            output: Ty::default(),
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            sym: intern_string("print_bool"),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("val"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Bool, Span::new()),
                                    span: Span::new(),
                                },
                            ],
                            output: Ty::default(),
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            sym: intern_string("assert"),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("val"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Bool, Span::new()),
                                    span: Span::new(),
                                },
                            ],
                            output: Ty::default(),
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            sym: intern_string("assert_eq_int"),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("left"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Int, Span::new()),
                                    span: Span::new(),
                                },
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("right"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Int, Span::new()),
                                    span: Span::new(),
                                },
                            ],
                            output: Ty::default(),
                            span: Span::new(),
                        },
                        span: Span::new(),
                    }
                ),
                
                Item::ForeignFn(
                    ForeignFnItem {
                        ident: ExprIdent {
                            sym: intern_string("assert_eq_bool"),
                            span: Span::new(),
                        },
                        decl: FnDecl {
                            inputs: vec![
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("left"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Bool, Span::new()),
                                    span: Span::new(),
                                },
                                Argument {
                                    mutable: false,
                                    ident: ExprIdent {
                                        sym: intern_string("right"),
                                        span: Span::new(),
                                    },
                                    ty: Ty::new(TyKind::Bool, Span::new()),
                                    span: Span::new(),
                                },
                            ],
                            output: Ty::default(),
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
#[no_mangle]
pub extern "C" fn print_int(val: i32) {
    println!("{}", val);
}

/**
 * Prints the given boolean.
 */
#[no_mangle]
pub extern "C" fn print_bool(val: bool) {
    println!("{}", val);
}

/**
 * Simple equals assertion.
 */
#[no_mangle]
pub extern "C" fn assert(val: bool) {
    assert!(val);
}

/**
 * Simple equals assertion.
 */
#[no_mangle]
pub extern "C" fn assert_eq_int(left: i32, right: i32) {
    assert_eq!(left, right);
}

/**
 * Simple equals assertion.
 */
#[no_mangle]
pub extern "C" fn assert_eq_bool(left: bool, right: bool) {
    assert_eq!(left, right);
}
