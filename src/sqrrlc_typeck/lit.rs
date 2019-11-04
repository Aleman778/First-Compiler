
/***************************************************************************
 * Type checker implementation for literals
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::Type,
    lit::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    TypeChecker,
};


/**
 * General type checking implementation for literals.
 */
impl TypeChecker for Lit {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        match self {
            Lit::Int(lit) => lit.check_type(env),
            Lit::Bool(lit) => lit.check_type(env),
        }
    }
}


/**
 * Type checking implementation for literal integers.
 */
impl TypeChecker for LitInt {
    fn check_type(&self, _env: &mut TypeEnv) -> Type {
        Type::Int32{span: self.span.clone()}
    }
}


/**
 * Type checking implementation for literal boolean.
 */
impl TypeChecker for LitBool {
    fn check_type(&self, _env: &mut TypeEnv) -> Type {
        Type::Bool{span: self.span.clone()}
    }
}
