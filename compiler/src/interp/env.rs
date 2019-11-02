
/***************************************************************************
 * The runtime environment submodule is used to store information
 * about the current runtime i.e. scoping, memory, function signatures etc.
 ***************************************************************************/


use std::fmt;
use std::collections::HashMap;
use crate::ast::{
    base::{Item, FnItem},
    expr::{ExprIdent},
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
#[derive(Clone)]
pub struct RuntimeEnv {
    /// Stores item signatures, maps strings to items
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes
    call_stack: Vec<Scope>,
    
    /// The main memory heap storage, stores variables
    memory: Memory,

    /// The program source code.
    source: String,
}


/**
 * Implementation of the runtime environment.
 */
impl RuntimeEnv {
    /**
     * Constructs an empty environment.
     */
    pub fn new(source: String) -> Self {
        RuntimeEnv {
            signatures: HashMap::new(),
            call_stack: Vec::new(),
            memory: Memory::new(),
            source: source,
        }
    }


    /**
     * Pushes a new block scope on the environment.
     * Note: there has to be a scope already on the call stack.
     */
    pub fn push_block(&mut self, new_scope: Scope) -> IResult<()> {
        let scope = self.current()?;
        scope.push(new_scope);
        Ok(())
    }


    /**
     * Pops the latest pushed block scope. The variables
     * in this scope are freed, they go out of scope.
     */
    pub fn pop_block(&mut self) -> IResult<()> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        let popped = scope.pop()?;
        for addr in popped.addresses() {
            self.memory.free(*addr)?;
        }
        Ok(())
    }


    /**
     * Push a function call scope on the call stack from the given id and with
     * specific argument values. The arguments are stored in the new scope.
     */
    pub fn push_func(&mut self, func: &FnItem, values: Vec<Val>) -> IResult<()> {
        let mut new_scope = Scope::new(func.span.clone());
        let inputs = &func.decl.inputs;
        if inputs.len() == values.len() {
            for i in 0..inputs.len() {
                let id = &inputs[i].ident;
                let addr = self.memory.alloc(values[i].clone())?;
                new_scope.register(id, addr);
            }
            self.call_stack.push(new_scope);
            Ok(())
        } else {
            Err(RuntimeError::context(func.span.clone(), "incorrect number of values provided"))
        }
    }


    /**
     * Push the main function scope on the call stack.
     */
    pub fn push_main(&mut self) -> IResult<FnItem> {
        match self.signatures.get("main") {
            Some(item) => {
                match item {
                    Item::Fn(func) => {
                        let new_scope = Scope::new(func.span.clone());
                        self.call_stack.push(new_scope);
                        Ok(func.clone())
                            
                    },
                    _ => Err(RuntimeError::context(Span::new_empty(), "there is no main function")),
                }
            }
            None => Err(RuntimeError::context(Span::new_empty(), "there is no main function")),
        }
    }
    

    /**
     * Pops and removes the the latest function call scope of the call stack
     */
    pub fn pop_func(&mut self) -> IResult<()> {
        match self.call_stack.pop() {
            Some(scope) => {
                for addr in scope.addresses() {
                    self.memory.free(*addr)?;
                }
                Ok(())
            },
            None => Err(RuntimeError::context(Span::new_empty(), "cannot pop an empty call stack")),
        }
    }


    /**
     * Stores an item signature from the given item.
     */
    pub fn store_item(&mut self, item: Item) {
        let key = item.get_id();
        if !self.signatures.contains_key(&key) {
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
     * Stores a variable into memory and registers it in the current scope.
     * Note: if trying to store to previously allocated variable then
     * that value will be overwritten with the provided value (shadowing).
     */
    pub fn store_var(&mut self, ident: &ExprIdent, val: Val) -> IResult<()> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        let prev = scope.address_of(&ident, false);
        match prev {
            Ok(addr) => self.memory.store(addr, val),
            Err(_) => {
                let addr = self.memory.alloc(val)?;
                scope.register(&ident, addr);
                Ok(())
            },
        }
    }

    
    /**
     * Assigns an already allocated mutable variable and updates the memory.
     */
    pub fn assign_var(&mut self, ident: &ExprIdent, val: Val) -> IResult<()> {
        let scope = self.current()?;
        let addr = scope.address_of(&ident, true)?;
        self.memory.store(addr, val)
    }


    /**
     * Loads an already allocated variable from the given identifier.
     */
    pub fn load_var(&mut self, ident: &ExprIdent) -> IResult<Val> {
        let scope = self.current()?;
        let addr = scope.address_of(&ident, true)?;
        self.memory.load(addr)
    }
    
    
    /**
     * Returns a mutable reference to the highest scope in the call stack.
     */
    fn current(&mut self) -> IResult<&mut Scope> {
        let len = self.call_stack.len();
        if len > 0 {
            Ok(&mut self.call_stack[len - 1])
        } else {
            Err(RuntimeError::context(Span::new_empty(), "the call stack is empty"))
        }
    }
}


/**
 * Formatting tracing of the runtime environment.
 */
impl fmt::Debug for RuntimeEnv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Trace:")
            .field("signatures", &format_args!("{:?}", self.signatures.keys()))
            .field("call_stack", &self.call_stack)
            .field("memory", &self.memory)
            .finish()
    }
}
