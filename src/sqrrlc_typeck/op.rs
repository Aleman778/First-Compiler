
/***************************************************************************
 * Type checker implementation for operators
 ***************************************************************************/


use crate::sqrrlc_ast::{
    ty::*,
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
    pub fn check_type(&self, lhs_ty: &Ty, rhs_ty: &Ty, env: &mut TypeEnv) {
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
    pub fn check_type(&self, ty: &Ty, env: &mut TypeEnv) {
        let (span, ok) = match self {
            UnOp::Neg{span} => (span.clone(), ty.is_int()),
            UnOp::Not{span} => (span.clone(), ty.kind == TyKind::Bool),
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
impl Ty {
    /**
     * Checks if this is an integer type and if the rhs type
     * is also the same type of integer.
     */
    fn check_int(&self, rhs_ty: &Ty) -> bool {
        match &self.kind {
            TyKind::Int(int) =>
                match int {
                    IntTy::I32 => rhs_ty.is_i32(),
                    IntTy::I64 => rhs_ty.is_i64(),
                },
            _ => false,
        }
    }


    /**
     * Checks if this is a boolean type and that the rhs type is
     * also a boolean.
     */
    fn check_bool(&self, rhs_ty: &Ty) -> bool {
        match self.kind {
            TyKind::Bool => rhs_ty.is_bool(),
            _ => false,
        }
    }


    /**
     * Check if this type is an integer type.
     */
    fn is_int(&self) -> bool{
        match &self.kind {
            TyKind::Int(_) => true,
            _ => false,
        }
    }
}
