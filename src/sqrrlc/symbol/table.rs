
/***************************************************************************
 * Symbol table sub module defines the data structure that stores symbols
 * according with scoping information.
 ***************************************************************************/


use std::fmt;
use std::collections::HashMap;
use crate::sqrrlc::symbol::{Symbol, FnSymbol};
use crate::sqrrlc_ast::{
    expr::ExprIdent,
    span::Span,
};
use crate::sqrrlc::{
    error::*,
    Result,
};


/**
 * Symbol table is a data structure used for storing
 * information about symbols in a program. The table is
 * divided up into scopes that are stored linearly in the
 * same order they occure in the source code.
 */
#[derive(Debug)]
pub struct SymbolTable {
    /// The list of scopes in this table.
    scopes: Vec<Scope>,

    /// Pointer to the current scope in the list.
    curr_ptr: usize,

    /// Pointer to the next scope in the list to process.
    next_ptr: usize,
}


/**
 * The scope struct defines a part of the symbol table that is 
 * valid for this given scope. Scopes are stored within the
 * Symbol Table struct.
 */
pub struct Scope {
    /// Maps identifiers to symbols.
    symbols: HashMap<String, Symbol>,
    
    /// The identifier used by this scope e.g. function name.
    ident: ExprIdent,

    /// The span location of this scope.
    span: Span,

    /// Pointer to the previous scope in the symbol table.
    prev_ptr: usize,
}


/**
 * Implementation of a symbol table.
 */
impl SymbolTable {
    /**
     * Creates a new empty symbol table.
     * The global scope is initialized by default.
     */
    pub fn new(span: Span) -> Self {
        SymbolTable {
            scopes: vec![Scope::new(span)],
            curr_ptr: 0,
            next_ptr: 1,
        }
    }
    

    /**
     * Push a new symbol table as a child to this table.
     */
    pub fn push_scope(&mut self, mut scope: Scope) {
        scope.prev_ptr = self.curr_ptr;
        self.scopes.push(scope);
        self.next_scope();
    }


    /**
     * Push a symbol with the identifier to the current scope.
     */
    pub fn push_symbol(&mut self, ident: &ExprIdent, symbol: Symbol){
        self.scopes[self.curr_ptr].push_symbol(ident, symbol);
    }

    
    /**
     * Returns the next scope in this symbol table.
     */
    pub fn next_scope(&mut self) {
        self.curr_ptr = self.next_ptr;
        self.next_ptr += 1;
    }


    /**
     * Returns the previous scope visited.
     */
    pub fn prev_scope(&mut self) {
        self.curr_ptr = self.scopes[self.curr_ptr].prev_ptr;
    }
    

    /**
     * Find a symbol from the given identifier.
     */
    pub fn find_symbol(&mut self, ident: &ExprIdent) -> Result<&Symbol> {
        let mut ptr = self.curr_ptr;
        loop {
            let scope = &self.scopes[ptr];
            match scope.find_symbol(ident) {
                Some(symbol) => return Ok(symbol),
                None => ptr = scope.prev_ptr,
            };
            if ptr == 0 {
                break;
            }
        }
        Err(
            Error {
                span: ident.span.clone(),
                kind: ErrorKind::OutOfScope(ident.to_string.clone(), &["symbol"])
            }
        )
    }


    /**
     * Find a function symbol in the global scope.
     * There is no need to check other scopes since functions
     * cannot be defined inside another function or scope.
     */
    pub fn find_fn_symbol(&self, ident: &ExprIdent) -> Result<&FnSymbol> {
        match self.scopes[0].find_symbol(ident) {
            Some(sym) => match sym {
                Symbol::Fn(func) => Ok(&func),
                _ => Err(
                    Error {
                        span: ident.span.clone(),
                        kind: ErrorKind::OutOfScope(ident.to_string.clone(), &["function"]),
                    }
                ),
            },
            _ => Err(
                Error {
                    span: ident.span.clone(),
                    kind: ErrorKind::OutOfScope(ident.to_string.clone(), &["function"]),
                }
            ),
        }
    }


    /**
     * Returns the identifier of the current scope.
     */
    pub fn current_id(&self) -> &ExprIdent {
        &self.scopes[self.curr_ptr].ident
    }


    /**
     * Resets the pointers used for iterating through the table scopes.
     */
    pub fn reset_ptr(&mut self) {
        self.curr_ptr = 0;
        self.next_ptr = 1;
    }
}


/**
 * Implementation of the symbol table.
 */
impl Scope {
    /**
     * Creates a new empty symbol table, without an identifier.
     */
    pub fn new(span: Span) -> Self {
        let span_clone = span.clone();
        Scope::with_ident(span, ExprIdent {
            to_string: "".to_string(),
            span: span_clone,
        })
    }


    /**
     * Creates a new empty symbol table with spcific identifier.
     */
    pub fn with_ident(span: Span, ident: ExprIdent) -> Self {
        Scope {
            symbols: HashMap::new(),
            ident: ident,
            span: span,
            prev_ptr: 0,
        }
    }


    /**
     * Push a new symbol entry to this table.
     */
    pub fn push_symbol(&mut self, ident: &ExprIdent, sym: Symbol) {
        self.symbols.insert(ident.to_string.clone(), sym);
    }


    /**
     * Find a symbol from the given identifier.
     */
    pub fn find_symbol(&self, ident: &ExprIdent) -> Option<&Symbol> {
        self.symbols.get(&ident.to_string)
    }
}


/**
 * Debug formatting for scope.
 */
impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("symbols", &self.symbols)
            .field("identifier", &self.ident.to_string)
            .field("span", &self.span)
            .field("prev_ptr", &self.prev_ptr)
            .finish()
    }
}
