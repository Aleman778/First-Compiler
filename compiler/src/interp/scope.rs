#![allow(dead_code)]

/***************************************************************************
 * The scope sbumodule defines a scope and is used with the callstack and
 * also for sub-block expressions.
 ***************************************************************************/


use std::collections::HashMap;
use crate::ast::expr::ExprIdent;
use crate::interp::{
    IResult,
    error::*,
};


/**
 * The scope is used to handle the memory inside a particular scope. 
 * For nested blocks this scope acts as an item in a linked list.
 * The outer block is always defined inside the callstack for
 * the environment.
 */
#[derive(Debug, Clone)]
pub struct Scope {
    /// The parent scope, used for sub-block expressions
    pub parent: Box<Option<Scope>>,

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
            parent: Box::new(None),
            vars: HashMap::new(),
        }
    }


    /**
     * Returns the address of a given variable identifier.
     */
    pub fn address_of(&self, id: &ExprIdent) -> IResult<usize> {
        match self.vars.get(id.to_string) {
            Some(addr) => addr,
            None => {
                match &*self.parent {
                    Some(parent) => parent.address_of(),
                    None => Err(RuntimeError {
                        span: id.span,
                        kind: ErrorKind::ValueNotFound(id.to_string.as_str()),
                    }),
                }
            },
        }
    }


    /**
     * Registers the given address and identifier in the variable list.
     */
    pub fn register(&mut self, id: &ExprIdent, addr: usize) {
        self.vars.insert(id.to_string, addr);
    }
}
