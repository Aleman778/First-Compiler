/**
 * Require the parser from assignment 2.
 */
use a02_parser::{Span, SpanArg, SpanExpr, Val};


/**
 * Require interpreter functionality.
 */
use crate::interpreter::{
    RuntimeError,
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
    mem: HashMap<&'a str, Val>,
}


/**
 * Environment implementation.
 */
impl<'a> Env<'a> {

    /**
     * Constructs an empty environment.
     */
    pub fn new() -> Env<'a> {
        Env {
            mem: HashMap::new(),
        }
    }
 
    
    /**
     * Constructs an environment from function arguments
     * and their respective values from a function call.
     */
    pub fn from_args(args: Vec<SpanArg<'a>>, values: Vec<Val>) -> Env<'a> {
        let mem = HashMap::new();
        for i in 0..args.len() {
            mem.insert(get_ident(&(args[i].1).0).unwrap(), values[i]);
        }

        Env {
            mem: mem,
        }
    }


    /**
     * Stores a given value to a given variable identifier in this environment.
     */
    pub fn store(&mut self, ident: &'a str, val: Val) {
        self.mem.insert(ident, val);
    }


    pub fn load(&mut self, ident: &'a str, s: Span<'a>) -> Result<&Val, RuntimeError<'a>>{
        match self.mem.get(ident) {
            Some(val) => Ok(val),
            None => Err(RuntimeError::MemoryError("not found in this scope", s)),
        }
    }
}
