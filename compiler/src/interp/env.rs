
/***************************************************************************
 * The runtime environment submodule is used to store information
 * about the current runtime i.e. scoping, memory, function signatures etc.
 ***************************************************************************/


use std::path::Path;
use std::collections::HashMap;
use crate::ast::{
    base::{Item, FnItem},
    expr::ExprIdent,
    span::Span,
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
    
    /// Reference to the current scope
    current: Option<&'a Scope>,
    
    /// The main memory heap storage, stores variables
    memory: Memory,
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
    pub fn push_block(&mut self) -> IResult<()> {
        match self.current {
            Some(scope) => {
                let new_scope = Scope::new();
                new_scope.parent = Box::new(Some(*scope));
                self.current = Some(&new_scope);
                Ok(())
            },
            _ => Err(RuntimeError::context(Span::new_empty(), "cannot push a block outside a function")),
        }
    }


    /**
     * Pops the latest pushed block scope.
     */
    pub fn pop_block(&mut self) -> IResult<()> {
        match self.current {
            Some(scope) => {
                match &*scope.parent {
                    Some(parent) => {
                        scope.parent = Box::new(None);
                        self.current = Some(parent);
                        Ok(())
                    },
                    _ => Err(RuntimeError::context(Span::new_empty(), "cannot pop the outer most block")),

                }
            },
            _ => Err(RuntimeError::context(Span::new_empty(), "there is no scope to pop")),
        }
    }


    /**
     * Push a function call scope on the call stack from the given id and with
     * specific argument values. The arguments are stored in the new scope.
     */
    pub fn push_func(&mut self, func_id: &ExprIdent, values: Vec<Val>) -> IResult<FnItem> {
        match self.load_item(func_id.to_string)? {
            Item::Fn(func) => {
                let mut new_scope = Scope::new();
                let inputs = func.decl.inputs;
                for i in 0..inputs.len() {
                    let id = inputs[i].ident.to_string;
                    let addr = self.memory.alloc(values[i]);
                    new_scope.register(id, addr);
                }
                
                self.call_stack.push(new_scope);
                self.current = &new_scope;
            },
            _ => Err(RuntimeError::item_not_found(func_id.span, func_id.to_string.as_str(), vec!["function"])),
        }
    }


    /**
     * Pops and removes the the latest function call scope of the call stack
     */
    pub fn pop_func(&mut self) -> IResult<()> {
        match self.call_stack.pop() {
            Some(_) => Ok(()),
            None => Err(RuntimeError::context(Span::new_empty(), "cannot pop an empty call stack"))
        }
    }


    /**
     * Stores an item signature from the given item.
     */
    pub fn store_item(&mut self, item: Item) {
        let key = item.get_id();
        if self.signatures.contains_key(&key) {
            self.signatures.insert(key, item);
        }
    }


    /**
     * Loads an item from a given identifier.
     */
    pub fn load_item(&self, id: &ExprIdent) -> IResult<Item> {
        match self.signatures.get(&id.to_string) {
            Some(item) => Ok(item),
            None => Err(RuntimeError {
                span: id.span,
                kind: ErrorKind::ItemNotFound(id.to_string.as_str()),
            }),
        }
    }
}
