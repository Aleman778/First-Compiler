#![allow(dead_code)]


use crate::interpreter::{Result, RuntimeError, Val, SpanIdent};
use std::collections::HashMap;


/**
 * The scope is used to store memory inside a particular scope.
 * For nested blocks then this scope has access to the child scope.
 * Function calls creates a new scope and puts on a stack however
 * other function scopes are inaccessable from this scope.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Scope<'a> {
    // The child scope, used for sub block epressions
    child: Box<Option<Scope<'a>>>,
    
    // Maps identifier to value used for storing variable data
    mem: HashMap<&'a str, Val>,
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
        match &mut *self.child {
            Some(child) => child.store_var(ident, val),
            None => self.mem.insert(ident.1, val),
        }
    }

    
    /**
     * Push a child scope onto this scope used when entering sub block expressions.
     */
    pub fn push(&mut self) {
        match &mut *self.child {
            Some(child) => child.push(),
            None => self.child = Box::new(Some(Scope::new()))
        };
    }

    
    /**
     * Pops the outer most child scope used when exiting sub block expressions.
     */
    pub fn pop(&mut self) {
        self.pop_impl();
    }


    pub fn pop_impl(&mut self) -> bool {
        match &mut *self.child {
            Some(child) => {
                if child.pop_impl() {
                    self.child = Box::new(None);
                }
                false
            },
            None => true,
        }
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
