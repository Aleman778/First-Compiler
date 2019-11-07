
/***************************************************************************
 * Type checker implementation for base AST nodes.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::Type,
    base::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    TypeChecker,
};


/**
 * Check the entire file for type errors.
 */
impl TypeChecker for File {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        for item in &self.items {
            item.check_type(env);
        }
        Type::None
    }
}


/**
 * Check this function item for type erros.
 */
impl TypeChecker for Item {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        match self {
            Item::Fn(func) => { func.check_type(env); },
            _ => { },
        };
        Type::None
    }
}


/**
 * Check the contents of this function for type errors.
 */
impl TypeChecker for FnItem {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        self.block.check_type(env);
        Type::None
    }
}
