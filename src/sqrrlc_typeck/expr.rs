
/***************************************************************************
 * Type checker implementation for expressions.
 ***************************************************************************/


use std::cmp::min;
use crate::sqrrlc_ast::{
    span::Span,
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
        self.op.check_type(&left, &right, env);
        left.clone()
    }
}


/**
 * Type checker implementation for block expressions.
 */
impl TypeChecker for ExprBlock {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        env.next_scope();
        for i in 0..self.stmts.len() {
            let ret_ty = self.stmts[i].check_type(env);
            if ret_ty != Type::None {
                if i == self.stmts.len() - 1 {
                    env.check_return_type(ret_ty);
                } else {
                    env.err(TypeError::mismatched_types(&Type::None, &ret_ty));
                }
            }
        }
        env.pop_scope();
        Type::None
    }
}


/**
 * Type checker implementation for function call expressions.
 */
impl TypeChecker for ExprCall {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let types = env.get_types(&self.ident);
        if types.len() > 0 {
            for i in 0..min(types.len() - 1, self.args.len()) {
                let arg = self.args[i].check_type(env);
                if arg != types[i] {
                    env.err(TypeError::mismatched_types(&types[i], &arg));
                }
            }
        }
        Type::None
    }
}


/**
 * Type checker implementation for identifier expressions.
 */
impl TypeChecker for ExprIdent {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let types = env.get_types(&self);
        if types.len() == 1 {
            types[0].clone()
        } else {
            Type::None
        }
    }
}


/**
 * Type checker implementation for if expressions.
 */
impl TypeChecker for ExprIf {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let cond_expected = Type::Bool{span: Span::new_empty()};
        let cond_type = (*self.cond).check_type(env);
        if cond_type != cond_expected {
            env.err(TypeError::mismatched_types(&cond_expected, &cond_type));
        }
        self.then_block.check_type(env);
        match &self.else_block {
            Some(block) => { block.check_type(env); },
            None => { },
        };
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
        let init_ty = self.init.check_type(env);
        if init_ty != self.ty {
            env.err(TypeError::mismatched_types(&self.ty, &init_ty));
        }
        Type::None
    }
}


/**
 * Type checker implementation for parenthesized expressions.
 */
impl TypeChecker for ExprParen {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        (*self.expr).check_type(env)
    }
}


/**
 * Type checker implementation for return expressions.
 */
impl TypeChecker for ExprReturn {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        match &*self.expr {
            Some(expr) => {
                let ret_ty = expr.check_type(env);
                env.check_return_type(ret_ty);
                Type::None
            },
            None => Type::None,
        }
    }
}


/**
 * Type checker implementation for unary expressions.
 */
impl TypeChecker for ExprUnary {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let ty = self.right.check_type(env);
        self.op.check_type(&ty, env);
        ty
    }
}


/**
 * Type checker implementation for while expressions.
 */
impl TypeChecker for ExprWhile {
    fn check_type(&self, env: &mut TypeEnv) -> Type {
        let cond_expected = Type::Bool{span: Span::new_empty()};
        let cond_type = self.cond.check_type(env);
        if cond_expected != cond_type {
            env.err(TypeError::mismatched_types(&cond_expected, &cond_type));
        }
        self.block.check_type(env);
        Type::None
    }
}
