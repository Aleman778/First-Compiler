#![allow(dead_code)]


use a02_parser::ast::{Span, SpanArg, SpanExpr, SpanType, Function};
use crate::interpreter::{Result, RuntimeError, Val, SpanIdent, get_ident, check_type};
use crate::scope::Scope;
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
 * Defines the attributes of a function.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Func<'a> {
    // Vector of function arguments.
    pub arg: Vec<SpanArg<'a>>,

    // The return type, optional no return type.
    pub ty: Option<SpanType<'a>>,

    // The function block.
    pub block: Box<SpanExpr<'a>>,
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
        }
    }

    
    /**
     * Pushes a new block scope on the environment
     * i.e. creates a new scope for handling the block.
     * There has to be a scope already int the call stack.
     */
    pub fn push_block(&mut self) {
        let len = self.scopes.len();
        let scope = &mut self.scopes[len - 1];
        scope.push();
    }


    /**
     * Pops the latest pushed block scope.
     */
    pub fn pop_block(&mut self) {
        let len = self.scopes.len();
        let scope = &mut self.scopes[len - 1];
        scope.pop();
    }


    /**
     * Loads the main function from this environment. This creates a new scope
     * and then returns the function data for evaluation.
     */
    pub fn push_main(&mut self) -> Result<'a, Func<'a>> {
        match self.func.get("main") {
            Some(func) => {
                let scope = Scope::new();
                self.scopes.push(scope);
                Ok(func.clone())
            }
            None => Err(RuntimeError::MemoryError("there is no main method", Span::new(""))),
        }
    }
    

    /**
     * Pushes a specific function on the stack in this environment.
     * This creates a new scope containing all the parameters
     * and also returns the function data for evaluation.
     */
    pub fn push_func(&mut self, ident: SpanIdent<'a>, values: Vec<Val>) -> Result<'a, Func<'a>> {
        match self.func.get(ident.1) {
            Some(func) => {
                let mut scope = Scope::new();
                for i in 0..func.arg.len() {
                    let arg = &func.arg[i].1;
                    let id = get_ident(&arg.0)?;
                    check_type(&func.ty, values[i], ident.0)?;
                    scope.store_var(id, values[i]);
                }
                self.scopes.push(scope);
                Ok(func.clone())
            },
            None => Err(RuntimeError::MemoryError("not found in this scope", ident.0))
        }        
    }


    /**
     * Removes the last scope from the scope stack.
     * Call this when exiting a function.
     */
    pub fn pop_func(&mut self) {
        self.scopes.pop();
    }


    /**
     * Stores a function onto the function signature mapper.
     */
    pub fn store_func(&mut self, func: Function<'a>) -> Result<'a, ()> {
        let id = get_ident(&func.0)?;
        self.func.insert(id.1, Func {
            arg: func.1,
            ty: func.2,
            block: func.3,
        });
        Ok(())
    }


    /**
     * Loads a variable from the current scope. If there is a current
     * scope present then the value is returned otherwise a memory error is returned.
     */
    pub fn load_var(&self, ident: SpanIdent<'a>) -> Result<Val> {
        let len = self.scopes.len();
        let scope = &self.scopes[len - 1];
        scope.load_var(ident).clone()
    }

    
    /**
     * Stores a variable to the current scope. Returns previously stored value.
     * If no current scope is present then this returns a memory error.
     */
    pub fn store_var(&mut self, ident: SpanIdent<'a>, val: Val) -> Result<Option<Val>> {
        let len = self.scopes.len();
        let scope = &mut self.scopes[len - 1];
        Ok(scope.store_var(ident, val))
    }
}
