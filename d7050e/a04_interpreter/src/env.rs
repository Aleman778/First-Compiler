/**
 * Require AST DS from the parser in assignment 2.
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
#[derive(Debug, Clone, PartialEq)]
pub struct Env<'a> {
    // Maps identifier to function used for function calls.
    func: HashMap<&'a str, Func<'a>>,

    // Stack containing each scope in a call stack
    scopes: Vec<Scope<'a>>,
}


/**
 * The scope is used to store memory inside a particular scope.
 * For nested blocks then this scope has access to the child scope.
 * Function calls creates a new scope and puts on a stack however
 * other function scopes are inaccessable from this scope.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Scope<'a> {
    // The child scope, used for 
    child: Box<Option<Scope<'a>>>,
    
    // Maps identifier to value used for storing variable data.
    mem: HashMap<&'a str, Val>,
}


/**
 * Defines the attributes of a function.
 */
#[derive(Debug, Clone, PartialEq)]
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
        let global = Scope::new();
        let mut scopes = Vec::new();
        scopes.push(global);
        
        Env {
            func: HashMap::new(),
            scopes: scopes,
        }
    }


    /**
     * Push a new scope onto the scope stack.
     * Use this when entering a new function, also
     * make sure to pop from stack when done.
     */
    pub fn push(&'a mut self, scope: Scope<'a>) {
        self.scopes.push(scope);
    }


    /**
     * Removes the last scope from the scope stack.
     * Call this when exiting a function.
     */
    pub fn pop(&'a mut self) {
        self.scopes.pop();
    }


    pub fn load_main(&'a mut self) -> Result<'a, Box<SpanExpr<'a>>> {
        match self.func.get("main") {
            Some(func) => {
                let scope = Scope::new();
                self.scopes.push(scope);
                
            }
            None => Err(RuntimeError::MemoryError("there is no main method")),
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
                Ok(func.block.clone())
            },
            None => Err(RuntimeError::MemoryError("not found in this scope", ident.0))
        }        
    }


    /**
     * Stores a function onto the function signature mapper.
     */
    pub fn store_func(&mut self, func: Function<'a>) {
        let id = get_ident(func.0).unwrap();
        self.func.insert(id.1,
            Func {
                arg: func.1,
                ty: func.2,
                block: func.3,
            }
        );
    }


    /**
     * Loads a variable from the current scope. If there is a current
     * scope present then the value is returned otherwise a memory error is returned.
     */
    pub fn load_var(&self, ident: SpanIdent<'a>) -> Result<Val> {
        match self.scopes.last() {
            Some(scope) => scope.load_var(ident),
            None => Err(RuntimeError::MemoryError("not inside a scope", ident.0)),
        }
    }

    
    /**
     * Stores a variable to the current scope. Returns previously stored value.
     * If no current scope is present then this returns a memory error.
     */
    pub fn store_var(&mut self, ident: SpanIdent<'a>, val: Val) -> Result<Option<Val>> {
        // match self.scopes.last() {
            // Some(scope) => Ok(scope.store_var(ident, val)),
            // None => Err(RuntimeError::MemoryError("not inside a scope", ident.0)),
        // }
        let len = self.scopes.len();
        let scope = &mut self.scopes[len - 1];
        Ok(scope.store_var(ident, val))
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
            child: Box::new(None),
            mem: HashMap::new(),
        }
    }

    
    /**
     * Loads a variable from the provided identifier
     */
    pub fn load_var(&self, ident: SpanIdent<'a>) -> Result<Val> {
        match &*self.child {
            Some(child) => {
                match child.load_var(ident) {
                    Ok(val) => Ok(val),
                    Err(_) => self.find_mem(ident),
                }
            }
            None => self.find_mem(ident),
        }
    }
    
    
    /**
     * Stores a variable using the given identifier and value.
     * If this variable was already allocated then this function
     * returns the previous value otherwise None is returned.
     */
    pub fn store_var(&mut self, ident: SpanIdent<'a>, val: Val) -> Option<Val> {
        self.mem.insert(ident.1, val)
    }

    
    /**
     * Set the provided environment to be the parent of this environment.
     */
    pub fn set_child(&mut self, child: Scope<'a>) {
        self.child = Box::new(Some(child));
    }


    /**
     * Find a value in the memory using the provided identifier.
     * Returns memory error if not found in this scope.
     */
    fn find_mem(&self, ident: SpanIdent<'a>) -> Result<Val> {
        match self.mem.get(ident.1) {
            Some(val) => Ok(*val),
            None => Err(RuntimeError::MemoryError("not found in this scope", ident.0)),
        }
    }
}
