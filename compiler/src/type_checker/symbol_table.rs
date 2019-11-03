
/***************************************************************************
 * Symbol Tables are used to define symbols, where an entry in the
 * table has the symbol name and type, symbols tables can be structured
 * according to the scoping of the program.
 ***************************************************************************/


use std::collections::HashMap;
use crate::ast::{
    span::Span,
    ty::Type,
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
 * The kind of symbol represented by the symbol struct.
 * This enum defines the different types of symbol types.
 */
#[derive(Debug, Clone)]
pub enum SymbolKind {
    Function,
    Variable,
}
