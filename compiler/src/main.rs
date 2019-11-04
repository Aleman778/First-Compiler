#![allow(dead_code)]
#![allow(unused_imports)]

/***************************************************************************
 * The main entry point of the sqrrl compiler.
 ***************************************************************************/


use std::fs;
use compiler::error::convert_error;
use compiler::ast::{
    base::*,
    expr::*,
    ty::*,
};
use compiler::parser::{Parser, ParseSpan};
use compiler::interp::{
    debug::debug_functions,
    env::RuntimeEnv,
    Eval,
};
use compiler::type_checker::{
    env::TypeEnv,
    TypeChecker,
};
use compiler::sqrrlc::symbol_table::*;



fn main() {
    // let a: i32 = 5 + false;
    
    // Parse from file
    let filename = "c:/dev/sqrrl-lang/compiler/examples/sandbox.sq";
    let contents = fs::read_to_string(filename).expect("file was not found");
    let span = ParseSpan::new_extra(contents.as_str(), filename);
    let expr = File::parse(span).unwrap().1;
    // expr.extend(debug_functions());

    let mut symbols = SymbolTable::new(expr.span.clone());
    let current = &mut symbols;
    for item in &expr.items {
        match item {
            Item::Fn(func) => {
                let mut fntable = SymbolTable::new(func.span.clone());
                let mut fnsymbol = Symbol::new(SymbolKind::Function);
                for arg in &func.decl.inputs {
                    fnsymbol.push_type(arg.ty.clone());
                    let mut sym = Symbol::new(SymbolKind::Variable);
                    sym.push_type(arg.ty.clone());
                    fntable.push_symbol(arg.ident.to_string.clone(), sym);
                }
                let outtype = match &func.decl.output {
                    Some(ty) => ty.clone(),
                    None => Type::None,
                };
                fnsymbol.push_type(outtype);
                current.push_symbol(func.ident.to_string.clone(), fnsymbol);
                
                fntable = sym_table_block(fntable, &func.block);
                current.push_table(fntable);
            },
            _ => unimplemented!(),
        }
    }
    println!("Symbol Table: \n{:#?}", symbols);

    //Parse expression
    // let source = "5 + false";
    // let span = ParseSpan::new_extra(source, "src\\main.rs");
    // let expr = Expr::parse_math(span).unwrap().1;
    // let mut env = TypeEnv::new();
    // println!("AST: {:#?}", expr);
    // expr.check_type(&mut env);
    // env.done(source);
    
    // let mut env = RuntimeEnv::new(contents.clone());
    // let val = expr.eval(&mut env);
    // match val {
        // Ok(_) => println!("Ok"),
        // Err(e) => println!("{}", convert_error(e.kind.description().as_str(), &e.span, contents.as_str(), "")),
    // };
}



fn sym_table_block(mut current: SymbolTable, block: &ExprBlock) -> SymbolTable {
    for expr in &block.stmts {
        match expr {
            Expr::Local(expr) => {
                let mut letsymbol = Symbol::new(SymbolKind::Variable);
                letsymbol.push_type(expr.ty.clone());
                current.push_symbol(expr.ident.to_string.clone(), letsymbol);
            },
            Expr::Block(expr) => {
                let mut table = SymbolTable::new(expr.span.clone());
                table = sym_table_block(table, &expr);
                current.push_table(table);
            },
            _ => {},
        }
    }
    return current;
}
