#![allow(dead_code)]

/***************************************************************************
 * The scope sbumodule defines a scope and is used with the callstack and
 * also for sub-block expressions.
 ***************************************************************************/


use std::collections::{
    hash_map::Values,
    HashMap
};
use crate::ast::{
    span::Span,
    expr::ExprIdent,
};
use crate::interp::{
    IResult,
    error::*,
};


/**
 * The scope is used to handle the memory inside a particular scope. 
 * For nested blocks this scope acts as an item in a linked list.
 * The outer block is always defined inside the call stack for
 * the environment.
 */
#[derive(Debug, Clone)]
pub struct Scope {
    /// The child scope, used for sub-block expressions
    child: Box<Option<Scope>>,

    /// Variable memory mapper, maps strings to memory addresses
    vars: HashMap<String, usize>,
}


/**
 * Implementation of scope.
 */
impl Scope {
    /**
     * Constructs a new scope.
     */
    pub fn new() -> Self {
        Scope {
            child: Box::new(None),
            vars: HashMap::new(),
        }
    }


    /**
     * Returns the address of a given variable identifier.
     */
    pub fn address_of(&self, id: &ExprIdent, backtrack: bool) -> IResult<usize> {
        match &*self.child {
            Some(child) => {
                match child.address_of(id, backtrack) {
                    Ok(addr) => Ok(addr),
                    Err(e) => {
                        if backtrack {
                            self.find_mem(id)
                        } else {
                            Err(e)
                        }
                    }
                }
            },
            None => self.find_mem(id),
        }
    }

    
    /**
     * Returns the registered addresses.
     */
    pub fn addresses(&self) -> Values<'_, String, usize> {
        self.vars.values()
    }
    

    /**
     * Registers the given address and identifier in the variable list.
     */
    pub fn register(&mut self, id: &ExprIdent, addr: usize) -> Option<usize> {
        match &mut *self.child {
            Some(child) => child.register(id, addr),
            None => self.vars.insert(id.to_string.clone(), addr),
        }
    }


    /**
     * Push a new scope onto the outer most scope.
     */
    pub fn push(&mut self, new_scope: Scope) {
        match &mut *self.child {
            Some(child) => child.push(new_scope),
            None => self.child = Box::new(Some(new_scope)),
        }
    }


    /**
     * Pops the outer most scope. Returns the popped scope.
     */
    pub fn pop(&mut self) -> IResult<Scope> {
        let (opt, _) = self.pop_impl();
        match opt {
            Some(scope) => Ok(scope.clone()),
            None => Err(RuntimeError::context(Span::new_empty(), "cannot pop the function scope"))
        }
    }

    
    /**
     * Pop implementation returns true if the outer most
     * scope has been reached and should be popped.
     */
    fn pop_impl(&mut self) -> (Option<Scope>, bool) {
        match &mut *self.child {
            Some(child) => {
                let (mut scope, found) = child.pop_impl();
                if found {
                    scope = Some(child.clone());
                    self.child = Box::new(None);
                }
                (scope, false)
            },
            None => (None, true),
        }
    }    

    
    /**
     * Find an address in the memory using the provided identifier.
     * Returns memory error if not found in this scope.
     */
    fn find_mem(&self, ident: &ExprIdent) -> IResult<usize> {
        match self.vars.get(&ident.to_string) {
            Some(addr) => Ok(*addr),
            None => Err(RuntimeError::context(ident.span.clone(), "not found in this scope")),
        }
    }
}
