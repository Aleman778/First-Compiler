#![allow(dead_code)]

/***************************************************************************
 * Statement AST sub module defines all the different statements in
 * the sqrrl language e.g. let-bindings, expressions. Usually these
 * ends with a semicolon token except for inexplicit returns.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    base::Item,
    expr::{Expr, ExprIdent},
    ty::Ty,
};

