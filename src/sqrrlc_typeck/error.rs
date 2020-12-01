
/***************************************************************************
 * The type checker will return Type Errors is violations of the
 * type rules are found. The errors are stored in the environment.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    span::Span,
    op::*,
    ty::*,
};

