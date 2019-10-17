
/***************************************************************************
 * The runtime environment submodule is used to store information
 * about the current runtime i.e. scoping, memory, function signatures etc.
 ***************************************************************************/


use std::collections::HashMap;
use crate::ast::base::Item;
use crate::interpreter::scope::Scope;


/**
 * The environment used when interpreting a program.
 */
#[derive(Debug, Clone)]
pub struct Env {
    /// Stores item signatures, maps strings to items.
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes
    call_stack: Vec<Scope>,
}


/**
 * Implementation of the runtime environment.
 */
impl Env {
    
}
