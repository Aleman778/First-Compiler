
/***************************************************************************
 * Abstract Syntax Tree helper is used to simply creations of 
 * some AST structures that are often used to simplify test code.
 ***************************************************************************/


use compiler::ast::{
    expr::ExprLit,
    span::Span,
    lit::*
};


/**
 * Generates ExprLit for i32 values.
 */
pub fn expr_lit_int(val: i32, span: Span) -> ExprLit {
    ExprLit {
        lit: Lit::Int(LitInt {
            value: val,
            span: span
        }),
        span: span,
    }
}


/**
 * Generates ExprLit for bool values
 */
pub fn expr_lit_bool(val: bool, span: Span) -> ExprLit {
    ExprLit {
        lit: Lit::Bool(LitBool {
            value: val,
            span: span
        }),
        span: span,
    }
}
