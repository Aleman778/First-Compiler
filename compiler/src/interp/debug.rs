
/***************************************************************************
 * Defines rust (foreign) functions for debugging interpreted programs.
 ***************************************************************************/


use crate::interp::{
    env::RuntimeEnv,
};
use crate::ast::{
    span::Span,
    expr::ExprIdent,
    ty::Type,
    base::*,
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
                output: None,
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
                        ident: ExprIdent {
                            to_string: "val".to_string(),
                            span: Span::new_empty(),
                        },
                        ty: Type::Int32{
                            span: Span::new_empty(),
                        },
                        span: Span::new_empty(),
                    }
                ],
                output: None,
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
                        ident: ExprIdent {
                            to_string: "val".to_string(),
                            span: Span::new_empty(),
                        },
                        ty: Type::Bool{
                            span: Span::new_empty(),
                        },
                        span: Span::new_empty(),
                    }
                ],
                output: None,
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
    println!("{:#?}", env)
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
