
/***************************************************************************
 * Environment used by the Type Checker
 ***************************************************************************/


use crate::sqrrlc::{
    symbol_table::SymbolTable,
    error::convert_error,
};
use crate::sqrrlc_ast::{
    expr::ExprIdent,
    ty::Type,
};
use crate::sqrrlc_typeck::{
    error::TypeError,
};


/**
 * Type environment holds the symbol table and a vector of typing errors.
 */
pub struct TypeEnv {
    /// The symbol table currently checking in.
    current: SymbolTable,

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
            current: table,
            errors: Vec::new(),
        }
    }


    /**
     * Go to the next scope in the current children scopes.
     */
    pub fn next_scope(&mut self) {
        match self.current.next_table() {
            Some(table) => self.current = table,
            None => { },
        };
    }


    /**
     * Pop the current scope and go the parent scope.
     */
    pub fn pop_scope(&mut self) {
        match self.current.parent_table() {
            Some(table) => self.current = table,
            None => { },
        };
    }


    /**
     * Returns list of types from the given identifier in the current symbol table.
     */
    pub fn get_types(&mut self, ident: &ExprIdent) -> Vec<Type> {
        match self.current.find_symbol(ident) {
            Ok(symbol) => symbol.types,
            Err(_) => Vec::new(),
        }
    }


    /**
     * Checks if the given type is the same as the current functions return type.
     */
    pub fn check_return_type(&mut self, ty: Type) {
        let ident = self.current.get_ident();
        let fn_types = self.get_types(&ident);
        match fn_types.last() {
            Some(fn_ty_ref) => {
                let fn_return_ty = fn_ty_ref.clone();
                if ty != fn_return_ty {
                    self.err(TypeError::mismatched_types(&ty, fn_ty_ref));
                }
            },
            None => {},
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
