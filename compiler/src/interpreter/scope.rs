
/***************************************************************************
 * The scope sbumodule defines a scope and is used with the callstack and
 * also for sub-block expressions.
 ***************************************************************************/


use std::collections::HashMap;


/**
 * The scope is used to handle the memory inside a particular scope. 
 * For nested blocks this scope acts as an item in a linked list.
 * The outer block is always defined inside the callstack for
 * the environment.
 */
#[derive(Debug, Clone)]
pub struct Scope {
    /// The parent scope, used for sub-block expressions
    parent: Box<Option<Scope>>,

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

    
}
