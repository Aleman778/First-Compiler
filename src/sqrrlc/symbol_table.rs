
/***************************************************************************
 * Symbol Tables are used to define symbols, where an entry in the
 * table has the symbol name and type, symbols tables can be structured
 * according to the scoping of the program.
 ***************************************************************************/


use std::fmt;
use std::rc::{Weak, Rc};
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
 * Symbol table is a data structure used for storing
 * information about symbols in a program. The table is
 * divided up into scopes that are stored linearly in the
 * same order they occure in the source code.
 */
struct SymbolTable {
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
#[derive(Clone)]
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
 * Symbol is a simple entry in the symbol table.
 * Each symbol has a kind and entries in the table.
 */
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The kind of symbol e.g. function, variable etc.
    pub kind: SymbolKind,

    /// The list of types associated with this symbol (used for type checking).
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


impl SymbolTable {
    /**
     * Creates a new empty symbol table.
     * The global scope is initialized by default.
     */
    pub fn new(Span: span) -> Self {
        SymbolTable {
            scopes: vec![Scope::new()],
        }
    }
    

    /**
     * Push a new symbol table as a child to this table.
     */
    pub fn push_scope(&mut self, mut table: SymbolTable) {
        table.parent = self.as_weak();
        let rc_table = Rc::new(table);
        self.children.push(rc_table);
    }

    
    /**
     * Returns the next scope in this symbol table.
     */
    pub fn next_scope(&mut self) -> Option<SymbolTable> {
    }


    /**
     * Returns the parent scope, if current scope is 
     */
    pub fn parent_scope(&self) -> Option<SymbolTable> {
    }
}


/**
 * Implementation of the symbol table.
 */
impl Scope {
    /**
     * Creates a new empty symbol table with spcific identifier.
     */
    pub fn new_ident(span: Span) -> Self {
        SymbolTable {
            parent: Weak::new(),
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
            None => match self.parent_table() {
                Some(mut parent) => parent.find_symbol(ident),
                None => Err(Error {
                    span: ident.span.clone(),
                    kind: ErrorKind::OutOfScope(ident.to_string.clone(), &["symbol"])
                }),
            }
        }
    }


    /**
     * Returns the identifier of this symbol table, thie identifier
     * is empty if none was specified in the constructor.
     */
    pub fn get_ident(&self) -> ExprIdent {
        return self.ident.clone();
    }
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
        let mut symbol = Symbol::new(SymbolKind::Function);
        self.decl.gen_sym_table(current, &mut symbol);
        current.push_symbol(self.ident.to_string.clone(), symbol);
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


/**
 * Debug formatting for improving the printing.
 */
impl fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Symbol Table")
            .field("children", &self.children)
            .field("symbols", &self.symbols)
            .field("ident", &self.ident.to_string)
            .field("span", &self.span)
            .field("next", &self.next)
            .finish()
    }
}
