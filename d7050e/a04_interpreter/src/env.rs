/**
 * Require the parser from assignment 2.
 */
use a02_parser::{Span, SpanArg, AST};


/**
 * Require interpreter functionality.
 */
use crate::interpreter::{
    RuntimeError,
    Result,
    Val,
    get_ident,
};


/**
 * Require hash map from standard library.
 */
use std::collections::HashMap;


/**
 * The environment used for interpreting a function.
 */
#[derive(Debug, PartialEq)]
pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    mem: HashMap<&'a str, Val>,
    ast: &'a AST<'a>,
}


/**
 * Environment implementation.
 */
impl<'a> Env<'a> {

    /**
     * Constructs an empty environment.
     */
    pub fn new(ast: &'a AST<'a>, parent: Option<&'a Env<'a>>) -> Env<'a> {
        Env {
            parent: parent,
            mem: HashMap::new(),
            ast: ast,
        }
    }
 
    
    /**
     * Constructs an environment from function arguments
     * and their respective values from a function call.
     */
    pub fn from_args(args: Vec<SpanArg<'a>>, values: Vec<Val>, ast: &'a AST<'a>) -> Env<'a> {
        let mut mem = HashMap::new();
        for i in 0..args.len() {
            mem.insert(get_ident(&(args[i].1).0).unwrap(), values[i]);
        }

        Env {
            parent: None,
            mem: mem,
            ast: ast,
        }
    }


    /**
     * Stores a given value to a given variable identifier in this environment.
     */
    pub fn store(&mut self, ident: &'a str, val: Val) {
        self.mem.insert(ident, val);
    }


    /**
     * Loads a variable from the memory in this environment.
     */
    pub fn load(&mut self, ident: &'a str, s: Span<'a>) -> Result<'a, Val>{
        match self.mem.get(ident) {
            Some(val) => Ok(*val),
            None => Err(RuntimeError::MemoryError("not found in this scope", s)),
            // {
            //     match self.parent {
            //         Some(env) => env.load(ident, s),
            //     ,
            //     }
            // }
        }
    }

    
    /**
     * Returns the Abstract Syntax Tree.
     */
    pub fn get_ast(&self) -> &'a AST<'a>{
        &self.ast
    }
}
