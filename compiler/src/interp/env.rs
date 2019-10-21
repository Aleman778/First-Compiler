
/***************************************************************************
 * The runtime environment submodule is used to store information
 * about the current runtime i.e. scoping, memory, function signatures etc.
 ***************************************************************************/


use std::collections::HashMap;
use crate::ast::{
    base::{Item, FnItem},
    expr::{ExprIdent, ExprCall},
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
pub struct Env {
    /// Stores item signatures, maps strings to items
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes
    call_stack: Vec<Scope>,
    
    /// The main memory heap storage, stores variables
    memory: Memory,
}


/**
 * Implementation of the runtime environment.
 */
impl Env {
    /**
     * Constructs an empty environment.
     */
    pub fn new() -> Self {
        Env {
            signatures: HashMap::new(),
            call_stack: Vec::new(),
            memory: Memory::new(),
        }
    }


    /**
     * Pushes a new block scope on the environment.
     * Note: there has to be a scope already on the call stack.
     */
    pub fn push_block(&mut self, new_scope: Scope) -> IResult<()> {
        let scope = self.current();
        scope.push(new_scope);
        Ok(())
            // },
            // _ => Err(RuntimeError::context(Span::new_empty(), "cannot push a block outside a function")),
        // }
    }


    /**
     * Pops the latest pushed block scope.
     */
    pub fn pop_block(&mut self) -> IResult<()> {
        let scope = self.current();
        scope.pop();
        Ok(())
            // }
            // _ => Err(RuntimeError::context(Span::new_empty(), "there is no scope to pop")),
        // }
    }


    /**
     * Push a function call scope on the call stack from the given id and with
     * specific argument values. The arguments are stored in the new scope.
     */
    #[allow(unreachable_patterns)]
    pub fn push_func(&mut self, call: &ExprCall, values: Vec<Val>) -> IResult<FnItem> {
        match self.load_item(&call.ident)? {
            Item::Fn(func) => {
                let mut new_scope = Scope::new();
                let inputs = &func.decl.inputs;
                if inputs.len() == values.len() {
                    for i in 0..inputs.len() {
                        let id = &inputs[i].ident;
                        let addr = self.memory.alloc(values[i].clone())?;
                        new_scope.register(id, addr);
                    }
                    self.call_stack.push(new_scope);
                    // self.current = Some(&new_scope);
                    Ok(func)
                } else {
                    Err(RuntimeError::context(call.span.clone(), "hello world!"))
                }
            },
            _ => Err(RuntimeError::item_not_found(&call.ident, &["function"])),
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
    pub fn load_item(&self, ident: &ExprIdent) -> IResult<Item> {
        match self.signatures.get(&ident.to_string) {
            Some(item) => Ok(item.clone()),
            None => Err(RuntimeError::item_not_found(&ident, &["signature"]))
        }
    }

    
    /**
     * Returns a mutable reference to the highest scope in the call stack.
     */
    fn current(&mut self) -> &mut Scope {
        let len = self.call_stack.len();
        &mut self.call_stack[len - 1]
    }
}
