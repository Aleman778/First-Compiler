
/***************************************************************************
 * Environment used by the Type Checker
 ***************************************************************************/


use crate::sqrrlc::{
    symbol::table::SymbolTable,
};
use crate::sqrrlc_ast::{
    ty::Ty,
};   
use crate::sqrrlc_typeck::{
    error::TypeError,
};


/**
 * Type environment holds the symbol table and a vector of typing errors.
 */
pub struct TypeEnv {
    /// The symbol table currently checking in.
    pub table: SymbolTable,
    
    /// Error stack holds all errors encountered during type checking.
    pub errors: Vec<TypeError>,
}


/**
 * Implementation of the type environment.
 */
impl TypeEnv {
    /**
     * Creates a new empry type checker environment.
     * Takes in the symbol table representing the global scope.
     */
    pub fn new(table: SymbolTable) -> Self {
        TypeEnv {
            table: table,
            errors: Vec::new(),
        }
    }


    /**
     * Go to the next scope in the current children scopes.
     */
    pub fn next_scope(&mut self) {
        self.table.next_scope();
    }


    /**
     * Pop the current scope and go the parent scope.
     */
    pub fn prev_scope(&mut self) {
        self.table.prev_scope();
    }


    /**
     * Check the return type agains the return type of the current function.
     */
    pub fn check_ret_ty(&mut self, ret_ty: Ty) {
        let ident = self.table.current_id();
        match self.table.find_fn_symbol(&ident) {
            Some(fn_sym) => {
                let fn_ret_ty = fn_sym.output.clone();
                if fn_ret_ty != ret_ty {
                    self.err(TypeError::mismatched_types(fn_ret_ty.kind, &ret_ty));
                }
            },
            None => { },
        };
    }
    

    /**
     * Pushes an error onto the stack of errors.
     */
    pub fn err(&mut self, error: TypeError) {
        self.errors.push(error);
    }
}
