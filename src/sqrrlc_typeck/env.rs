
/***************************************************************************
 * Environment used by the Type Checker
 ***************************************************************************/


use crate::sqrrlc::{
    error::convert_error,
    symbol::table::SymbolTable,
    symbol::*,
};
use crate::sqrrlc_ast::{
    expr::ExprIdent,
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
    table: SymbolTable,

    /// Error stack holds all errors encountered during type checking.
    errors: Vec<TypeError>,
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
     * Returns list of types from the given identifier in the current symbol table.
     */
    pub fn find_symbol(&mut self, ident: &ExprIdent) -> Option<&Symbol> {
        match self.table.find_symbol(ident) {
            Ok(symbol) => Some(symbol),
            Err(_) => None,
        }
    }


    /**
     * Tries to find a variable symbol with the given identifier.
     */
    pub fn find_var_symbol(&mut self, ident: &ExprIdent) -> Option<&VarSymbol> {
        match self.find_symbol(ident) {
            Some(symbol) => match symbol {
                Symbol::Var(var) => Some(&var),
                _ => None,
            },
            None => None,
        }
    }


    /**
     * Tries to find a function symbol with the given identifier.
     */
    pub fn find_fn_symbol(&mut self, ident: &ExprIdent) -> Option<&FnSymbol> {
        match self.table.find_fn_symbol(ident) {
            Ok(fn_sym) => Some(fn_sym),
            Err(_) => None
        }
    }


    /**
     * Check the return type agains the return type of the current function.
     */
    pub fn check_ret_ty(&mut self, ret_ty: Ty) {
        let ident = self.table.current_id();
        match self.table.find_fn_symbol(&ident) {
            Ok(fn_sym) => {
                let fn_ret_ty = fn_sym.output.clone();
                if fn_ret_ty != ret_ty {
                    self.err(TypeError::mismatched_types(fn_ret_ty.kind, &ret_ty));
                }
            },
            Err(_) => { },
        };
    }
    

    /**
     * Pushes an error onto the stack of errors.
     */
    pub fn err(&mut self, error: TypeError) {
        self.errors.push(error);
    }


    /**
     * Notify the type checker that the process is done.
     * Errors are reported here.
     */
    pub fn done(&self, source: &str) {
        for error in &self.errors {
            let desc = error.kind.description();
            let expl = error.kind.explanation();
            println!("{}", convert_error(desc.as_str(), &error.span, source, &expl));
        }
    }
}
