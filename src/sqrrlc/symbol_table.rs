
/***************************************************************************
 * Symbol Tables are used to define symbols, where an entry in the
 * table has the symbol name and type, symbols tables can be structured
 * according to the scoping of the program.
 ***************************************************************************/


use std::collections::HashMap;
use crate::sqrrlc_ast::{
    expr::{Expr, ExprBlock, ExprIdent, ExprLocal},
    span::Span,
    ty::Type,
    base::*,
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
    ident: ExprIdent,
    span: Span,
    next: usize,
}


/**
 * Implementation of the symbol table.
 */
impl SymbolTable {
    /**
     * Creates a new empty symbol table with spcific identifier.
     */
    pub fn new_ident(span: Span, ident: ExprIdent) -> Self {
        SymbolTable {
            parent: None,
            children: Vec::new(),
            symbols: HashMap::new(),
            ident: ident,
            span: span,
            next: 0,
        }
    }


    /**
     * Creates a new empty symbol table, without an identifier.
     */
    pub fn new(span: Span) -> Self {
        let span_clone = span.clone();
        SymbolTable::new_ident(span, ExprIdent {
            to_string: "".to_string(),
            span: span_clone,
        })
    }


    /**
     * Push a new symbol table as a child to this table.
     */
    pub fn push_table(&mut self, mut table: SymbolTable) {
        table.parent = Some(Box::new(self.clone()));
        self.children.push(table);
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
    pub fn find_symbol(&mut self, ident: &ExprIdent) -> Result<Symbol> {
        match self.symbols.get(&ident.to_string) {
            Some(sym) => Ok(sym.clone()),
            None => match self.parent.clone() {
                Some(mut parent) => parent.find_symbol(ident),
                None => Err(Error {
                    span: ident.span.clone(),
                    kind: ErrorKind::OutOfScope(ident.to_string.clone(), &["symbol"])
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


    pub fn get_ident(&self) -> ExprIdent {
        return self.ident.clone();
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


/***************************************************************************
 * Symbol table generators
 ***************************************************************************/


/**
 * Generate symbol table trait.
 */
pub trait GenSymbolTable {
    fn gen_sym_table(&self, current: &mut SymbolTable);
}


/**
 * Generate symbol table for a file AST node.
 */
impl GenSymbolTable for File {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        for item in &self.items {
            item.gen_sym_table(current);
        }
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for Item {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        match self {
            Item::Fn(func) => func.gen_sym_table(current),
            Item::ForeignFn(func) => func.gen_sym_table(current),
        };
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for FnItem {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        let mut new_table = SymbolTable::new_ident(self.span.clone(), self.ident.clone());
        let mut symbol = Symbol::new(SymbolKind::Function);
        self.decl.gen_sym_table(&mut new_table, &mut symbol);
        current.push_symbol(self.ident.to_string.clone(), symbol);
        self.block.gen_sym_table(&mut new_table);
        current.push_table(new_table);
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for ForeignFnItem {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        let mut new_table = SymbolTable::new_ident(self.span.clone(), self.ident.clone());
        let mut symbol = Symbol::new(SymbolKind::Function);
        self.decl.gen_sym_table(&mut new_table, &mut symbol);
        current.push_symbol(self.ident.to_string.clone(), symbol);
        current.push_table(new_table);
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl FnDecl {
    fn gen_sym_table(&self, current: &mut SymbolTable, fn_symbol: &mut Symbol) {
        for arg in &self.inputs {
            let mut arg_symbol = Symbol::new(SymbolKind::Variable);
            arg_symbol.push_type(arg.ty.clone());
            current.push_symbol(arg.ident.to_string.clone(), arg_symbol);
            fn_symbol.push_type(arg.ty.clone());
        }
        let output = match &self.output {
            Some(ty) => ty.clone(),
            None => Type::None,
        };
        fn_symbol.push_type(output);
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for Expr {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        match self {
            Expr::Block(expr) => {
                let mut new_table = SymbolTable::new(expr.span.clone(), );
                expr.gen_sym_table(&mut new_table);
                current.push_table(new_table);
            },
            Expr::Local(expr) => expr.gen_sym_table(current),
            _ => {},
        };
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for ExprBlock {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        for expr in &self.stmts {
            expr.gen_sym_table(current);
        }
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for ExprLocal {
    fn gen_sym_table(&self, current: &mut SymbolTable) {
        let mut symbol = Symbol::new(SymbolKind::Variable);
        symbol.push_type(self.ty.clone());
        current.push_symbol(self.ident.to_string.clone(), symbol);
    }
}
