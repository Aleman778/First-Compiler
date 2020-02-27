
/***************************************************************************
 * Symbol table generator
 ***************************************************************************/

use crate::core::{
    symbol::table::{SymbolTable, Scope},
    symbol::*,
};
use crate::ast::*;


/**
 * Geneerates a symbol table from the given file AST.
 */
pub fn gen_sym_table(ast: &Node) -> SymbolTable {
    let mut table = SymbolTable::new(ast.span);
    ast.gen_sym_table(&mut table);
    table.reset_ptr();
    table
}


/**
 * Generate symbol table for a file AST node.
 */
impl GenSymbolTable for  {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        for item in &self.items {
            item.gen_sym_table(table);
        }
    }
}


/**
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for Item {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        match self {
            Item::Fn(func) => func.gen_sym_table(table),
            Item::ForeignFn(func) => func.gen_sym_table(table),
            Item::ForeignMod(func) => func.gen_sym_table(table),
        };
    }
}


/**
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for FnItem {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        let mut symbol = FnSymbol::new();
        self.decl.gen_sym_table(table, &mut symbol);
        table.push_symbol(&self.ident, Symbol::Fn(symbol));
        table.push_scope(Scope::with_ident(self.span, self.ident.clone()));
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
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for ForeignFnItem {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        let mut symbol = FnSymbol::new();
        self.decl.gen_sym_table(table, &mut symbol);
        table.push_symbol(&self.ident, Symbol::Fn(symbol));
    }
}


/**
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for ForeignModItem {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        for item in &self.items {
            item.gen_sym_table(table);
        } 
    }
}


/**
 * Generate symbol table for item AST node.
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
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for Stmt {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        match self {
            Stmt::Local(local) => local.gen_sym_table(table),
            Stmt::Item(item) => item.gen_sym_table(table),
            Stmt::Semi(expr) => expr.gen_sym_table(table),
            Stmt::Expr(expr) => expr.gen_sym_table(table),
        }
    }
}


/**
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for Block {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        for expr in &self.stmts {
            expr.gen_sym_table(table);
        }
    }
}


/**
 * Generate symbol table for item AST node.
 */
impl GenSymbolTable for Local {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        let mut symbol = VarSymbol::new();
        symbol.ty = self.ty.clone();
        table.push_symbol(&self.ident, Symbol::Var(symbol));
        match &*self.init {
            Some(expr) => expr.gen_sym_table(table),
            None => {},
        };
    }
}


/**
 * Generate symbol table for expressions.
 */
impl GenSymbolTable for Expr {
    fn gen_sym_table(&self, table: &mut SymbolTable) {
        match self {
            Expr::Block(expr) => {
                let ident = table.current_id().clone();
                table.push_scope(Scope::with_ident(expr.span, ident));
                expr.block.gen_sym_table(table);
                table.prev_scope();
            }
            Expr::If(expr) => {
                let ident = table.current_id().clone();
                table.push_scope(Scope::with_ident(expr.span, ident));
                expr.then_block.gen_sym_table(table);
                table.prev_scope();

                match &expr.else_block {
                    Some(else_block) => {
                        let ident = table.current_id().clone();
                        table.push_scope(Scope::with_ident(expr.span, ident));
                        else_block.gen_sym_table(table);
                        table.prev_scope();
                    }
                    None => {}
                }
            }
            Expr::While(expr) => {
                let ident = table.current_id().clone();
                table.push_scope(Scope::with_ident(expr.span, ident));
                expr.block.gen_sym_table(table);
                table.prev_scope();
            }
            _ => {}
        }
    }
}
