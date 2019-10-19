
/***************************************************************************
 * The runtime environment submodule is used to store information
 * about the current runtime i.e. scoping, memory, function signatures etc.
 ***************************************************************************/


use std::collections::HashMap;
use crate::ast::{
    base::{Item, FnItem},
    expr::ExprIdent,
};
use crate::interp::{
    IResult,
    value::Val,
    scope::Scope,
    memory::Memory,
    error::*,
};


/**
 * The environment used when interpreting a program.
 * Environment stores runtime information such as
 * the memory, call stack and item signatures etc.
 */
#[derive(Debug, Clone)]
pub struct Env<'a> {
    /// Stores item signatures, maps strings to items
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes
    call_stack: Vec<Scope>,

    /// The main memory heap storage, stores variables
    memory: Memory,

    /// Reference to the current scope
    current: Option<&'a Scope>,
}


/**
 * Implementation of the runtime environment.
 */
impl<'a> Env<'a> {
    /**
     * Constructs an empty environment.
     */
    pub fn new() -> Self {
        Env {
            signatures: HashMap::new(),
            call_stack: Vec::new(),
            memory: Memory::new(),
            current: None,
        }
    }


    /**
     * Pushes a new block scope on the environment.
     * Note: there has to be a scope already on the call stack.
     */
    pub fn push_block(&mut self) {
        
    }


    /**
     * Pops the latest pushed block scope.
     */
    pub fn pop_block(&mut self) {
        
    }


    /**
     * Push a function call scope on the call stack from the given id and with
     * specific argument values. The arguments are stored in the new scope.
     */
    pub fn push_func(&mut self, func_id: String, arg_values: Vec<Val>) -> IResult<FnItem> {
        
    }


    /**
     * Pops and removes the the latest function call scope of the call stack
     */
    pub fn pop_func(&mut self) {
        self.call_stack.pop();
    }


    /**
     * Stores an item signature from the given item.
     */
    pub fn store_item(&mut self, item: Item) {
        
    }


    pub fn load_item(&self, id: &ExprIdent) -> IResult<Item> {
        match self.signatures.get(id) {
            Some(item) => Ok(item),
            None => Err(RuntimeError {
                span: id.span,
                kind: ErrorKind::ItemNotFound(id.to_string.as_str()),
            }),
        }
    }
}
