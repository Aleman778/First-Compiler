
/***************************************************************************
 * Defines rust (foreign) functions for debugging interpreted programs.
 ***************************************************************************/

use log::info;
use std::io::Write;
use crate::sqrrlc_ast::{
    span::Span,
    expr::ExprIdent,
    ty::*,
    base::*,
};
use crate::sqrrlc_interp::{
    env::RuntimeEnv,
};


/**
 * Returns vector of debug foreign functions.
 */
pub fn debug_functions() -> Vec<Item> {
    vec![
        Item::ForeignFn(ForeignFnItem {
            ident: ExprIdent {
                to_string: "trace".to_string(),
                span: Span::new_empty(),
            },
            decl: FnDecl {
                inputs: Vec::new(),
                output: Ty::new(),
                span: Span::new_empty(),
            },
            span: Span::new_empty(),
        }),
        Item::ForeignFn(ForeignFnItem {
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
                            kind: TyKind::Int(IntTy::I32),
                            span: Span::new_empty(),
                        },
                        span: Span::new_empty(),
                    }
                ],
                output: Ty::new(),
                span: Span::new_empty(),
            },
            span: Span::new_empty(),
        }),
        Item::ForeignFn(ForeignFnItem {
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
                    }
                ],
                output: Ty::new(),
                span: Span::new_empty(),
            },
            span: Span::new_empty(),
        }),
    ]
}


/**
 * Prints the environment
 */
pub fn trace(env: &mut RuntimeEnv) {
    let trace = format!("{:#?}", env);
    let mut dest = env.sess.writable_out();
    match dest.write(trace.as_bytes()) {
        Err(e) => info!("Failed to print the trace, err: {}", e),
        Ok(_) => { },
    }
}


/**
 * Prints the given integer.
 */
pub fn print_int(env: &mut RuntimeEnv, val: i32) {
    let mut dest = env.sess.writable_out();
    match write!(dest, "{}", val) {
        Err(e) => info!("Failed to print int, err: {}", e),
        Ok(_) => { },
    }
}


/**
 * Prints the given boolean.
 */
pub fn print_bool(env: &mut RuntimeEnv, val: bool) {
    let mut dest = env.sess.writable_out();
    match write!(dest, "{}", val) {
        Err(e) => info!("Failed to print bool, err: {}", e),
        Ok(_) => { },
    }
}
