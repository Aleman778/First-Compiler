/**
 * Require the parser from assignment 2.
 */
use a02_parser::{Span, SpanArg, SpanExpr, Function, AST};


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
    // The parent environment, enables scoping.
    parent: Box<Option<Env<'a>>>,
    
    // Maps identifier to function used for function calls.
    func: HashMap<&'a str, ()>,

    // Maps identifier to value used for storing variable data.
    vars: HashMap<&'a str, Val>,
}


struct Func<'a>{
    arg: Vec<SpanArgs<'a>>,
    ty: SpanType<'a>,
    block: SpanExpr<'a>,
};


/**
 * Environment implementation.
 */
impl<'a> Env<'a> {
    /**
     * Constructs an empty environment.
     */
    pub fn new() -> Env<'a> {
        Env {
            parent: Box::new(None),
            func: HashMap::new(),
            vars: HashMap::new(),
        }
    }


    /**
     * Loads a function onto a new environment containing all the parameters
     * and then returns the new environment include the block expression to execute.
     */
    pub fn load_func(self, ident: &'a str, values: Vec<Val>) -> Result<'a, Env<'a>, SpanExpr<'a>>{
        match self.func.get(ident) {
            Some(func) => {
                let mut env = Env::new();
                for i in 0..func.1.len() {
                    let id = get_ident(&((func.1)[i].1).0).unwrap();
                    env.store_var(id, values[i]);
                }
                Ok((env, *func.3))
            },
            None => Err(RuntimeError::MemoryError("function is not loaded into memory", Span::new(ident)))
        }        
    }


    pub fn load_var(self, ident: &'a str, val: Val) {
        
    }
    

    pub fn store_func(&mut self, ident: &'a str, func: Function) {

    }


    pub fn store_var(&mut self, ident: &'a str, val: Val) {
    }
    
    
    // /**
    //  * Constructs an environment from function arguments
    //  * and their respective values from a function call.
    //  */
    // pub fn from_args(args: Vec<SpanArg<'a>>, values: Vec<Val>, ast: &'a AST<'a>) -> Env<'a> {
    //     let mut mem = HashMap::new();
    //     for i in 0..args.len() {
    //         mem.insert(get_ident(&(args[i].1).0).unwrap(), values[i]);
    //     }

    //     Env {
    //         parent: Box::new(None),
    //         mem: mem,
    //         ast: ast,
    //     }
    // }


    // /**
    //  * Stores a given value to a given variable identifier in this environment.
    //  */
    // pub fn store(&mut self, ident: &'a str, val: Val) {
    //     self.mem.insert(ident, val);
    // }


    // /**
    //  * Loads a variable from the memory in this environment.
    //  */
    // pub fn load(&mut self, ident: &'a str, s: Span<'a>) -> Result<'a>{
    //     match self.mem.get(ident) {
    //         Some(val) => Ok(*val),
    //         None => Err(RuntimeError::MemoryError("not found in this scope", s)),
    //         // {
    //         //     match self.parent {
    //         //         Some(env) => env.load(ident, s),
    //         //     ,
    //         //     }
    //         // }
    //     }
    // }

    
//     /**
//      * Returns the Abstract Syntax Tree.
//      */
//     pub fn get_ast(&self) -> &'a AST<'a>{
//         &self.ast
//     }
}
