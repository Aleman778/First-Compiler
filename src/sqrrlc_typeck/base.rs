
/***************************************************************************
 * Type checker implementation for base AST nodes.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::Type,
    base::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    error::TypeError,
    TypeChecker,
};


impl TypeChecker for File {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}
