
/***************************************************************************
 * Type checker implementation for operators
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::Type,
    op::*,
};
use crate::sqrrlc_typeck::{
    env::TypeEnv,
    error::*,
};


/**
 * Type checking implementation of binary operators.
 */
impl BinOp {
    /**
     * Check the types of the left and right expression agains the implemented
     * types supported for this operator. Reports an error if this operator
     * does not implement the given types.
     */
    pub fn check_type(&self, lhs_ty: &Type, rhs_ty: &Type, env: &mut TypeEnv) {
        let (span, ok) = match self {
            BinOp::Add{span} => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Sub{span} => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Mul{span} => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Div{span} => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Pow{span} => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Mod{span} => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::And{span} => (span.clone(), lhs_ty.check_bool(rhs_ty)),
            BinOp::Or{span}  => (span.clone(), lhs_ty.check_bool(rhs_ty)),
            BinOp::Eq{span}  => (span.clone(), lhs_ty == rhs_ty),
            BinOp::Ne{span}  => (span.clone(), lhs_ty == rhs_ty),
            BinOp::Lt{span}  => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Le{span}  => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Gt{span}  => (span.clone(), lhs_ty.check_int(rhs_ty)),
            BinOp::Ge{span}  => (span.clone(), lhs_ty.check_int(rhs_ty)),
        };
        if !ok {
            env.err(TypeError::new(span, ErrorKind::BinOpNotImplemented(
                self.clone(), lhs_ty.clone(), rhs_ty.clone())));
        }
    }
}


/**
 * Type checking implementation for unary operators.
 * 
 */
impl UnOp {
    /**
     * Check the type given agains the implemented types supported for this operator.
     * Reports an error if this operator is not implemented for the given type.
     */
    pub fn check_type(&self, ty: &Type, env: &mut TypeEnv) {
        let (span, ok) = match self {
            UnOp::Neg{span} => (span.clone(), ty.is_i32()),
            UnOp::Not{span} => (span.clone(), ty.is_bool()),
            UnOp::Deref{span} => (span.clone(), ty.get_ref().is_some()),
        };
        if !ok {
            env.err(TypeError::new(span, ErrorKind::UnOpNotImplemented(self.clone(), ty.clone())));
        }
    }
}



/**
 * Check type operator implementations.
 */
impl Type {
    /**
     * Checks if this is an integer type and if the rhs type
     * is also the same type of integer.
     */
    fn check_int(&self, rhs_ty: &Type) -> bool {
        match self {
            Type::Int32{span: _} => rhs_ty.is_i32(),
            _ => false,
        }
    }


    /**
     * Checks if this is a boolean type and that the rhs type is
     * also a boolean.
     */
    fn check_bool(&self, rhs_ty: &Type) -> bool {
        match self {
            Type::Bool{span: _} => rhs_ty.is_bool(),
            _ => false,
        }
    }
}
