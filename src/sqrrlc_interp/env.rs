
/***************************************************************************
 * The runtime environment submodule is used to store information
 * about the current runtime i.e. scoping, memory, function signatures etc.
 ***************************************************************************/

use std::fmt;
use std::collections::HashMap;
use crate::sqrrlc::session::Session;
use crate::sqrrlc_ast::{
    base::{Item, FnItem},
    expr::ExprIdent,
    span::Span,
};
use crate::sqrrlc_interp::{
    IResult,
    value::Val,
    scope::Scope,
    memory::Memory,
};


/**
 * The environment used when interpreting a program.
 * Environment stores runtime information such as
 * the memory, call stack and item signatures etc.
 */
#[derive(Clone)]
pub struct RuntimeEnv<'a> {
    /// The compiler session, used for storing common data.
    pub sess: &'a Session,
    
    /// Stores item signatures, maps strings to items
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes
    call_stack: Vec<Scope<'a>>,
    
    /// The main memory heap storage, stores variables
    memory: Memory<'a>,
}


/**
 * Implementation of the runtime environment.
 */
impl<'a> RuntimeEnv<'a> {
    /**
     * Constructs an empty environment.
     */
    pub fn new(session: &'a Session) -> Self {
        RuntimeEnv {
            sess: session,
            signatures: HashMap::new(),
            call_stack: Vec::new(),
            memory: Memory::new(session),
        }
    }


    /**
     * Pushes a new block scope on the environment.
     * Note: there has to be a scope already on the call stack.
     */
    pub fn push_block(&mut self, new_scope: Scope<'a>) -> IResult<()> {
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
        let mut new_scope = Scope::new(self.sess, func.span.clone());
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
            let mut err = struct_fatal!(
                self.sess,
                "this function takes {} parameters but {} parameters were supplied",
                inputs.len(),
                values.len()
            );
            let mut span = Span::from_bounds(
                func.span.start, func.block.span.start,
            );
            span.end.column += 1;
            err.span_label(span, "defined here");
            Err(err)
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
                        let new_scope = Scope::new(self.sess, func.span.clone());
                        self.call_stack.push(new_scope);
                        Ok(func.clone())
                            
                    },
                    _ => Err(struct_fatal!(self.sess, "there is no main function")),
                }
            }
            None => Err(struct_fatal!(self.sess, "there is no main function")),
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
            None => Err(struct_fatal!(self.sess, "cannot pop an empty call stack")),
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
            None => {
                let mut err = struct_span_fatal!(
                    self.sess,
                    ident.span,
                    "cannot find function `{}` in this scope",
                    ident.to_string
                );
                err.span_label(ident.span, "not found in this scope");
                Err(err)
            }
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
            Err(mut err) => {
                err.cancel();
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
    fn current(&mut self) -> IResult<&mut Scope<'a>> {
        let len = self.call_stack.len();
        if len > 0 {
            Ok(&mut self.call_stack[len - 1])
        } else {
            Err(struct_fatal!(self.sess, "the call stack is empty"))
        }
    }
}


/**
 * Formatting tracing of the runtime environment.
 */
impl<'a> fmt::Debug for RuntimeEnv<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Trace:")
            .field("signatures", &format_args!("{:?}", self.signatures.keys()))
            .field("call_stack", &self.call_stack)
            .field("memory", &self.memory)
            .finish()
    }
}
