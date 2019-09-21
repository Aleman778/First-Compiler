/**
 * Require the parser from assignment 2.
 */
use a02_parser::{SpanArg, SpanExpr, Val};

/**
 * Require hash map from standard library.
 */
use std::collections::HashMap;


/**
 * The environment used for interpreting a function.
 */
#[derive(Debug)]
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
        let mut mem = HashMap::new();
        for i in 0..args.len() {
            mem.insert((((args[i].1).0).1).1, values[i]);
        }

        Env {
            mem: mem,
        }
    }


    /**
     * Stores a given value to a given variable identifier in this environment.
     */
    pub fn store(self, ident: &str, val: Val) {
        self.mem.insert(ident, val);
    }
}
