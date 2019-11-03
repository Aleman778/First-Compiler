
/***************************************************************************
 * Environment used by the Type Checker
 ***************************************************************************/


use crate::error::convert_error;
use crate::ast::span::Span;
use crate::type_checker::{
    symbol_table::SymbolTable,
    error::TypeError,
};


/**
 * Type environment holds the symbol table and a vector of typing errors.
 */
pub struct TypeEnv {
    /// The global symbol table is where items are stored.
    global: SymbolTable,

    /// Error stack holds all errors encountered during type checking.
    errors: Vec<TypeError>,
}


/**
 * Implementation of the type environment.
 */
impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            global: SymbolTable::new(Span::new_empty()),
            errors: Vec::new(),
        }
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
