/**
 * Require the parser from assignment 2.
 */
use a02_parser::{Span, SpanArg, SpanExpr, SpanType, Function, AST};


/**
 * Require interpreter functionality.
 */
use crate::interpreter::{RuntimeError, Val, SpanIdent, get_ident};


/**
 * Type alias of result to include output and runtime errors.
 */
pub type Result<'a, O, E = RuntimeError<'a>> = std::result::Result<O, E>;


/**
 * Require hash map from standard library.
 */
use std::collections::HashMap;


/**
 * The environment used for interpreting a function.
 */
#[derive(Debug, PartialEq)]
pub struct Env<'a> {
    // Maps identifier to function used for function calls.
    func: HashMap<&'a str, Func<'a>>,

    // Stack containing each scope in a call stack
    scopes: Vec<Scope<'a>>,
    
    // The current scope currently in use.
    current: Option<&'a Scope<'a>>,
}


/**
 * The scope is used to store memory inside a particular scope.
 * For nested blocks then this scope has access to parent scopes.
 * Function calls creates a new scope and puts on a stack however
 * other functions scope is inaccessable from this scope.
 */
#[derive(Debug, PartialEq)]
struct Scope<'a> {
    // The parents scope.
    parent: Box<Option<Scope<'a>>>,
    
    // Maps identifier to value used for storing variable data.
    mem: HashMap<&'a str, Val>,
}


/**
 * Defines the attributes of a function.
 */
#[derive(Debug, PartialEq)]
struct Func<'a> {
    arg: Vec<SpanArg<'a>>,
    ty: Option<SpanType<'a>>,
    block: Box<SpanExpr<'a>>,
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
            func: HashMap::new(),
            scopes: Vec::new(),
            current: None,
        }
    }


    /**
     * Loads a function from this environment. This creates a new scope containing all the parameters
     * and then returns the block expression from the function that should be executed.
     */
    pub fn load_func(&'a mut self, ident: SpanIdent<'a>, values: Vec<Val>) -> Result<'a, Box<SpanExpr<'a>>> {
        match self.func.get(ident.1) {
            Some(func) => {
                let mut scope = Scope::new();
                for i in 0..func.arg.len() {
                    let arg = &func.arg[i].1;
                    let id = get_ident(&arg.0).unwrap();
                    scope.store_var(id, values[i]);
                }
                self.scopes.push(scope);
                match self.scopes.last() {
                    Some(scope) => self.current = Some(&scope),
                    None => self.current = None,
                }    
                Ok(func.block.clone())
            },
            None => Err(RuntimeError::MemoryError("not found in this scope", ident.0))
        }        
    }


    /**
     * Stores a function onto the 
     */
    pub fn store_func(&mut self, ident: SpanIdent<'a>, func: Function<'a>) {
        self.func.insert(ident.1,
            Func {
                arg: func.1,
                ty: func.2,
                block: func.3,
            }
        );
    }
}


/**
 * Implementation of the scope
 */
impl<'a> Scope<'a> {
    /**
     * Constructs a new empty scope.
     */
    pub fn new() -> Scope<'a> {
        Scope {
            parent: Box::new(None),
            mem: HashMap::new(),
        }
    }

    
    /**
     * Loads a variable from the provided identifier
     */
    pub fn load_var(&self, ident: SpanIdent<'a>) -> Result<Val> {
        match self.mem.get(ident.1) {
            Some(val) => Ok(*val),
            None => {
                match &*self.parent {
                    Some(p) => p.load_var(ident),
                    None => Err(RuntimeError::MemoryError("not found in this scope", ident.0)),
                }
            }
        }
    }

    
    /**
     * Stores a variable using the given identifier and value.
     */
    pub fn store_var(&mut self, ident: SpanIdent<'a>, val: Val) {
        self.mem.insert(ident.1, val);
    }

    
    /**
     * Set the provided environment to be the parent of this environment.
     */
    pub fn set_parent(&mut self, parent: Scope<'a>) {
        self.parent = Box::new(Some(parent));
    }
}
