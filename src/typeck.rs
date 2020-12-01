use crate::ast::*;

/**
 * Type context is used to hold information used by the type cheker.
 */
pub struct TyCtxt<'tcx> {
    pub sess: &'tcx Session,
    pub sym: &'tcx mut SymbolTable,
}

impl<'tcx> TyCtxt<'tcx> {
    /**
     * Creates a new type context with the given
     * compiler session and symbol table to use.
     */
    pub fn new(sess: &'tcx Session, sym: &'tcx mut SymbolTable) -> Self {
        TyCtxt{sess, sym}
    }
}

/**
 * The type checker trait should be implemented for every AST node
 * that handles types and can generate a type error.
 */
pub trait TypeChecker {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty;
}

/**
 * Check the entire file for type errors.
 */
impl TypeChecker for File {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        for item in &self.items {
            item.check_type(tcx);
        }
        Ty::new()
    }
}

/**
 * Check this function item for type erros.
 */
impl TypeChecker for Item {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Item::Fn(func) => { func.check_type(tcx); },
            _ => { },
        };
        Ty::new()
    }
}

/**
 * Check the contents of this function for type errors.
 */
impl TypeChecker for FnItem {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let ret_ty = self.block.check_type(tcx);
        match tcx.sym.find_fn_symbol(&self.ident) {
            Some(fn_sym) => if fn_sym.output != ret_ty {
                if ret_ty.is_none() {
                    let mut err = struct_span_err!(tcx.sess, fn_sym.output.span, "mismatched types");
                    err.span_label(fn_sym.output.span, &format!("expected {} found ()", fn_sym.output));
                    if ret_ty.span.is_empty() {
                        err.span_label(
                            self.ident.span,
                            "implicitly returns `()` as its body has no tail or `return` expression"
                        );
                    } else {
                        err.span_label(ret_ty.span, "return type is not ()");
                    }
                    tcx.sess.emit(&err);
                } else {
                    mismatched_return_type_err!(tcx.sess, fn_sym, ret_ty);
                }
            },
            None => { }
        }

        Ty::new()
    }
}

/**
 * Type checker implementation for block statements.
 */
impl TypeChecker for Block {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        tcx.sym.next_scope();
        let mut ret_ty = Ty::new();
        for i in 0..self.stmts.len() {
            ret_ty = self.stmts[i].check_type(tcx);
            let is_return = match self.stmts[i] {
                Stmt::Semi(ref expr) => match expr {
                    Expr::Return(_) => true,
                    _ => false,
                },
                Stmt::Expr(_) => true,
                _ => false,
            };
            if !ret_ty.is_none() && !is_return {
                if i < self.stmts.len() - 1 {
                    mismatched_types_err!(tcx.sess, ret_ty.span, TyKind::None, &ret_ty);
                }
            }
        }
        tcx.sym.prev_scope();
        ret_ty.span = self.span.clone();
        ret_ty
    }
}

/**
 * Type checker for statements.
 */
impl TypeChecker for Stmt {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Stmt::Local(local) => local.check_type(tcx),
            Stmt::Item(item) => item.check_type(tcx),
            Stmt::Semi(expr) => {
                let ret_ty = expr.check_type(tcx);
                if let Expr::Return(_) = expr {
                    ret_ty
                } else {
                    Ty::new()
                }
            },
            Stmt::Expr(expr) => expr.check_type(tcx),
        }
    }
}

/**
 * Type checker implementation for let binding expressions.
 */
impl TypeChecker for Local {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match &*self.init {
            Some(init) => {
                let init_ty = init.check_type(tcx);
                let mut symbol = VarSymbol::new();
                symbol.ty = self.ty.clone();
                tcx.sym.push_symbol(&self.ident, Symbol::Var(symbol));
                if init_ty != self.ty {
                    mismatched_types_err!(tcx.sess, init_ty.span, self.ty, init_ty);
                }
            },
            None => { },
        };
        Ty::new()
    }
}

/**
 * Type checking implementation of binary operators.
 */
impl BinOp {
    /**
     * Check the types of the left and right expression agains the implemented
     * types supported for this operator. Reports an error if this operator
     * does not implement the given types.
     */
    pub fn check_type(&self, lhs_ty: &Ty, rhs_ty: &Ty, tcx: &mut TyCtxt) {
        let (span, ok) = match self {
            BinOp::Add{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Sub{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Mul{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Div{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Pow{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Mod{span} => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::And{span} => (span, lhs_ty.check_bool(rhs_ty)),
            BinOp::Or{span}  => (span, lhs_ty.check_bool(rhs_ty)),
            BinOp::Eq{span}  => (span, lhs_ty == rhs_ty),
            BinOp::Ne{span}  => (span, lhs_ty == rhs_ty),
            BinOp::Lt{span}  => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Le{span}  => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Gt{span}  => (span, lhs_ty.check_int(rhs_ty)),
            BinOp::Ge{span}  => (span, lhs_ty.check_int(rhs_ty)),
        };
        if !ok {
            let mut err = struct_span_err!(tcx.sess, *span, "cannot {} `{}` to `{}`", self, lhs_ty, rhs_ty);
            err.span_label(*span, &format!("no implementation for `{} {} {}`", lhs_ty, self.token(), rhs_ty));
            tcx.sess.emit(&err);
        }
    }

    /**
     * Infer the type that is resulted from the operation.
     * Special case for using comparators, needs to convert type to bool.
     */
    pub fn infer_type(&self, lhs_ty: &Ty, rhs_ty: &Ty) -> Ty {
        match self {
            BinOp::Eq{span: _} |
            BinOp::Ne{span: _} |
            BinOp::Lt{span: _} |
            BinOp::Le{span: _} |
            BinOp::Gt{span: _} |
            BinOp::Ge{span: _} => Ty {
                kind: TyKind::Bool,
                span: Span::from_bounds(
                    lhs_ty.span.start,
                    rhs_ty.span.end,
                    lhs_ty.span.loc,
                ),
            },
            _ => Ty {
                kind: lhs_ty.kind.clone(),
                span: Span::from_bounds(
                    lhs_ty.span.start,
                    rhs_ty.span.end,
                    lhs_ty.span.loc
                ),
            }
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
    pub fn check_type(&self, mut ty: Ty, tcx: &mut TyCtxt) -> Ty {
        let (span, ok) = match self {
            UnOp::Neg{span} => (span, ty.is_int()),
            UnOp::Not{span} => (span, ty.kind == TyKind::Bool),
            UnOp::Deref{span} => match ty.get_ref() {
                Some(r) => {
                    ty = (*r.elem).clone();
                    (span, true)
                },
                None => (span, false),
            }
        };
        if !ok {
            let mut err = struct_span_err!(tcx.sess, *span, "type `{}` cannot be {}", ty, self);
            err.span_label(*span, &format!("no implementation for `{}{}`", self.token(), ty));
            tcx.sess.emit(&err);
        }
        ty
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

/**
 * Genreal type checker implementation for expressions.
 */
impl TypeChecker for Expr {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Expr::Assign(expr)    => expr.check_type(tcx),
            Expr::Binary(expr)    => expr.check_type(tcx),
            Expr::Block(expr)     => expr.check_type(tcx),
            Expr::Call(expr)      => expr.check_type(tcx),
            Expr::Ident(expr)     => expr.check_type(tcx),
            Expr::If(expr)        => expr.check_type(tcx),
            Expr::Lit(expr)       => expr.check_type(tcx),
            Expr::Paren(expr)     => expr.check_type(tcx),
            Expr::Reference(expr) => expr.check_type(tcx),
            Expr::Return(expr)    => expr.check_type(tcx),
            Expr::Unary(expr)     => expr.check_type(tcx),
            Expr::While(expr)     => expr.check_type(tcx),
            _ => Ty::new(),
        }
    }
}

/**
 * Type checker implementation for assignment expressions.
 */
impl TypeChecker for ExprAssign {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let prev = self.left.check_type(tcx);
        let new = self.right.check_type(tcx);
        if prev != new {
            mismatched_types_err!(tcx.sess, new.span, prev, new);
        }
        Ty::new()
    }
}

/**
 * Type checker implementation for binary expressions.
 */
impl TypeChecker for ExprBinary {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let left = self.left.check_type(tcx);
        let right = self.right.check_type(tcx);
        self.op.check_type(&left, &right, tcx);
        self.op.infer_type(&left, &right)
    }
}

/**
 * Type checker implementation for block expressions.
 */
impl TypeChecker for ExprBlock {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        self.block.check_type(tcx)
    }
}

/**
 * Type checker implementation for function call expressions.
 */
impl TypeChecker for ExprCall {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match tcx.sym.find_fn_symbol(&self.ident) {
            Some(fn_sym) => {
                let fn_inputs = fn_sym.inputs.clone();
                let fn_output = fn_sym.output.clone();
                let mut arg_tys = Vec::new();
                for i in 0..min(self.args.len(), fn_inputs.len()) {
                    let ty = self.args[i].check_type(tcx);
                    arg_tys.push(ty);
                }
                for i in 0..min(arg_tys.len(), fn_inputs.len()) {
                    if fn_inputs[i] != arg_tys[i] {
                        mismatched_types_err!(tcx.sess, arg_tys[i].span, fn_inputs[i], arg_tys[i]);
                    }
                }
                let mut out_ty = fn_output.clone();
                out_ty.span = self.span;
                out_ty
            },
            None => Ty::new(),
        }
    }
}

/**
 * Type checker implementation for identifier expressions.
 */
impl TypeChecker for ExprIdent {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match tcx.sym.find_var_symbol(&self) {
            Some(var) => {
                let mut id_ty = var.ty.clone();
                id_ty.span = self.span;
                id_ty
            },
            None => Ty::new(),
        }
    }
}

/**
 * Type checker implementation for if expressions.
 */
impl TypeChecker for ExprIf {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let cond_type = (*self.cond).check_type(tcx);
        if cond_type.kind != TyKind::Bool {
            mismatched_types_err!(tcx.sess, cond_type.span, TyKind::Bool, cond_type);
        }
        let then_ty = self.then_block.check_type(tcx);
        match &self.else_block {
            Some(block) => {
                let else_ty = block.check_type(tcx);
                if then_ty != else_ty {
                    mismatched_types_err!(tcx.sess, else_ty.span, then_ty, else_ty);
                }
            },
            None => { },
        };
        then_ty
    }
}

/**
 * Type checker implementation for literal expressions.
 */
impl TypeChecker for ExprLit {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        self.lit.check_type(tcx)
    }
}

/**
 * Type checker implementation for parenthesized expressions.
 */
impl TypeChecker for ExprParen {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        (*self.expr).check_type(tcx)
    }
}

/**
 * Type checker implementation for return expressions.
 */
impl TypeChecker for ExprReturn {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match &*self.expr {
            Some(expr) => {
                let ret_ty = expr.check_type(tcx);
                match tcx.sym.find_current_fn_symbol() {
                    Some(fn_sym) => if fn_sym.output != ret_ty {
                        mismatched_return_type_err!(tcx.sess, fn_sym, ret_ty);
                        fn_sym.output.clone()
                    } else {
                        ret_ty
                    }
                    None => ret_ty
                }
            }
            None => {
                let mut ret_ty = Ty::new();
                ret_ty.span = self.span;
                ret_ty
            }
        }
    }
}

/**
 * Type checker implementation for reference expressions.
 */
impl TypeChecker for ExprReference {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let ty = (*self.expr).check_type(tcx);
        if ty.is_none() {
            let mut err = struct_span_err!(tcx.sess, self.span, "cannot reference none type");
            err.span_label(ty.span, "expected a typed value here, got nothing");
            tcx.sess.emit(&err);
            Ty::new()
        } else {
            Ty {
                span: self.span,
                kind: TyKind::Ref(TypeRef {
                    mutable: self.mutable,
                    elem: Box::new(ty),
                }),
            }
        }
    }
}

/**
 * Type checker implementation for unary expressions.
 */
impl TypeChecker for ExprUnary {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let ty = self.right.check_type(tcx);
        self.op.check_type(ty, tcx)
    }
}

/**
 * Type checker implementation for while expressions.
 */
impl TypeChecker for ExprWhile {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        let cond_type = self.cond.check_type(tcx);
        if cond_type.kind != TyKind::Bool {
            mismatched_types_err!(tcx.sess, cond_type.span, TyKind::Bool, cond_type);
        }
        self.block.check_type(tcx);
        Ty::new()
    }
}

/**
 * General type checking implementation for literals.
 */
impl TypeChecker for Lit {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty {
        match self {
            Lit::Int(lit) => lit.check_type(tcx),
            Lit::Bool(lit) => lit.check_type(tcx),
            Lit::Str(lit) => {
                let err = struct_span_fatal!(tcx.sess,
                                             lit.span,
                                             "unsupported literal type by the type checker");
                tcx.sess.emit(&err);
                Ty::new()
            }
        }
    }
}

/**
 * Type checking implementation for literal integers.
 */
impl TypeChecker for LitInt {
    fn check_type(&self, _tcx: &mut TyCtxt) -> Ty {
        Ty {
            kind: TyKind::Int(IntTy::I32),
            span: self.span,
        }
    }
}

/**
 * Type checking implementation for literal boolean.
 */
impl TypeChecker for LitBool {
    fn check_type(&self, _tcx: &mut TyCtxt) -> Ty {
        Ty {
            kind: TyKind::Bool,
            span: self.span,
        }
    }
}
