
/***************************************************************************
 * Symbol Tables are used to define symbols, where an entry in the
 * table has the symbol name and type, symbols tables can be structured
 * according to the scoping of the program.
 ***************************************************************************/


use std::collections::HashMap;
use crate::sqrrlc_ast::{
    span::Span,
    ty::Type,
    expr::ExprIdent,
};
use crate::sqrrlc::{
    error::*,
    Result,
};


/**
 * Symbol table represents the data storage for symbols, where the identifiers
 * are mapped to a symbol. The symbol table is using a tree structure to
 * handle scoping. 
 */
#[derive(Debug, Clone)]
pub struct SymbolTable {
    parent: Option<Box<SymbolTable>>,
    children: Vec<SymbolTable>,
    symbols: HashMap<String, Symbol>,
    span: Span,
    next: usize,
}


/**
 * Implementation of the symbol table.
 */
impl SymbolTable {
    /**
     * Creates a new empty symbol table.
     */
    pub fn new(span: Span) -> Self {
        SymbolTable {
            parent: None,
            children: Vec::new(),
            symbols: HashMap::new(),
            span: span,
            next: 0,
        }
    }


    /**
     * Push a new symbol table as a child to this table.
     */
    pub fn push_table(&mut self, mut table: SymbolTable) {
        self.children.push(table);
        table.parent = Some(Box::new(self.clone()));
    }


    /**
     * Push a new symbol entry to this table.
     */
    pub fn push_symbol(&mut self, ident: String, sym: Symbol) {
        self.symbols.insert(ident, sym);
    }


    /**
     * Find a symbol from the given identifier.
     */
    pub fn find_symbol(&mut self, ident: &'static ExprIdent) -> Result<Symbol> {
        match self.symbols.get(&ident.to_string) {
            Some(sym) => Ok(sym.clone()),
            None => match self.parent.clone() {
                Some(mut parent) => parent.find_symbol(ident),
                None => Err(Error {
                    span: ident.span.clone(),
                    kind: ErrorKind::OutOfScope(ident.to_string.as_str(), &["symbol"])
                }),
            }
        }
    }

    
    /**
     * Returns the next symbol table in the list.
     */
    pub fn next_table(&mut self) -> SymbolTable {
        let table = self.children[self.next].clone();
        self.next += 1;
        return table;
    }


    /**
     * Returns the parent symbol table.
     */
    pub fn parent_table(&self) -> Option<SymbolTable> {
        match &self.parent {
            Some(parent) => Some(*parent.clone()),
            None => None,
        }
    }
}


/**
 * Symbol is a simple entry in the symbol table.
 * Each symbol has a kind and entries in the table.
 */
#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub types: Vec<Type>,
}


/**
 * Implementation of the symbol struct.
 */
impl Symbol {
    /**
     * Creates a new empty symbol of specific kind.
     */
    pub fn new(kind: SymbolKind) -> Self {
        Symbol {
            kind: kind,
            types: Vec::new(),
        }
    }


    /**
     * Push a new type on to the list of types.
     */
    pub fn push_type(&mut self, ty: Type) {
        self.types.push(ty);
    }
}


/**
 * The kind of symbol represented by the symbol struct.
 * This enum defines the different types of symbol types.
 */
#[derive(Debug, Clone)]
pub enum SymbolKind {
    Function,
    Variable,
}
