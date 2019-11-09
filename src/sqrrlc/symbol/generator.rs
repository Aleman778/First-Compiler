
/***************************************************************************
 * Symbol table generator
 ***************************************************************************/

use crate::sqrrlc::{
    symbol::table::{SymbolTable, Scope},
    symbol::*,
};
use crate::sqrrlc_ast::{
    expr::{Expr, ExprBlock, ExprLocal},
    base::*,
};


/**
 * Geneerates a symbol table from the given file AST.
 */
pub fn gen_sym_table(file: &File) -> SymbolTable {
    let mut table = SymbolTable::new(file.span.clone());
    file.gen_sym_table(&mut table);
    table.reset_ptr();
    table
}


/**
 * Generate symbol table trait.
 */
trait GenSymbolTable {
    fn gen_sym_table(&self, table: &mut SymbolTable);
}


/**
 * Generate symbol table for a file AST node.
 */
impl GenSymbolTable for File {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        for item in &self.items {
            item.gen_sym_table(table);
        }
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for Item {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        match self {
            Item::Fn(func) => func.gen_sym_table(table),
            Item::ForeignFn(func) => func.gen_sym_table(table),
        };
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for FnItem {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        let mut symbol = FnSymbol::new();
        self.decl.gen_sym_table(table, &mut symbol);
        table.push_symbol(&self.ident, Symbol::Fn(symbol));
        table.push_scope(Scope::with_ident(self.span.clone(), self.ident.clone()));
        for arg in &self.decl.inputs {
            let mut arg_symbol = VarSymbol::new();
            arg_symbol.ty = arg.ty.clone();
            table.push_symbol(&arg.ident, Symbol::Var(arg_symbol));
        }
        self.block.gen_sym_table(table);
        table.prev_scope();
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for ForeignFnItem {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        let mut symbol = FnSymbol::new();
        self.decl.gen_sym_table(table, &mut symbol);
        table.push_symbol(&self.ident, Symbol::Fn(symbol));
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl FnDecl {
    fn gen_sym_table(&self, _table: &mut SymbolTable, fn_symbol: &mut FnSymbol) {
        for arg in &self.inputs {
            fn_symbol.inputs.push(arg.ty.clone());
        }
        fn_symbol.output = self.output.clone();
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for Expr {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        match self {
            Expr::Block(expr) => {
                let ident = table.current_id().clone();
                table.push_scope(Scope::with_ident(expr.span.clone(), ident));
                expr.gen_sym_table(table);
                table.prev_scope();
            },
            Expr::Local(expr) => expr.gen_sym_table(table),
            _ => {},
        };
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for ExprBlock {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        for expr in &self.stmts {
            expr.gen_sym_table(table);
        }
    }
}


/**
 * Genreate symbol table for item AST node.
 */
impl GenSymbolTable for ExprLocal {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        let mut symbol = VarSymbol::new();
        symbol.ty = self.ty.clone();
        table.push_symbol(&self.ident, Symbol::Var(symbol));
    }
}
