
/***************************************************************************
 * Type checker implementation for expressions.
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::Type,
    expr::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    error::TypeError,
    TypeChecker,
};


/**
 * Genreal type checker implementation for expressions.
 */
impl TypeChecker for Expr {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        match self {
            Expr::Assign(expr) => expr.check_type(env),
            Expr::Binary(expr) => expr.check_type(env),
            Expr::Block(expr)  => expr.check_type(env),
            Expr::Call(expr)   => expr.check_type(env),
            Expr::Ident(expr)  => expr.check_type(env),
            Expr::If(expr)     => expr.check_type(env),
            Expr::Lit(expr)    => expr.check_type(env),
            Expr::Local(expr)  => expr.check_type(env),
            Expr::Paren(expr)  => expr.check_type(env),
            Expr::Return(expr) => expr.check_type(env),
            Expr::Unary(expr)  => expr.check_type(env),
            Expr::While(expr)  => expr.check_type(env),
            _ => Type::None,
        }
    }
}


/**
 * Type checker implementation for assignment expressions.
 */
impl TypeChecker for ExprAssign {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let prev = self.ident.check_type(env);
        let new = self.expr.check_type(env);
        if prev != new {
            env.err(TypeError::mismatched_types(&prev, &new));
        }
        Type::None
    }
}


/**
 * Type checker implementation for binary expressions.
 */
impl TypeChecker for ExprBinary {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let left = self.left.check_type(env);
        let right = self.right.check_type(env);
        let ret = left.clone();
        match self.op.check_type(&left, &right) {
            Some(error) => env.err(error),
            None => {
                if right != left {
                    env.err(TypeError::mismatched_types(&left, &right));
                }
            }
        };
        ret
    }
}


/**
 * Type checker implementation for block expressions.
 */
impl TypeChecker for ExprBlock {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for function call expressions.
 */
impl TypeChecker for ExprCall {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for identifier expressions.
 */
impl TypeChecker for ExprIdent {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for if expressions.
 */
impl TypeChecker for ExprIf {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for literal expressions.
 */
impl TypeChecker for ExprLit {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        self.lit.check_type(env)
    }
}


/**
 * Type checker implementation for let binding expressions.
 */
impl TypeChecker for ExprLocal {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for parenthesized expressions.
 */
impl TypeChecker for ExprParen {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for return expressions.
 */
impl TypeChecker for ExprReturn {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for unary expressions.
 */
impl TypeChecker for ExprUnary {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}


/**
 * Type checker implementation for while expressions.
 */
impl TypeChecker for ExprWhile {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        Type::None
    }
}
