//! The abstract syntax tree is stored inside a hash map structure
//! for conineance, you can iterate over items lookup by id etc.


pub use crate::sqrrlc_ast::ast::Node;


/**
 * Identifier to an ast node.
 */
pub struct NodeId(pub u32);


pub struct AstMap {
    pub mapping: Vec<Node>,
}


impl AstMap {
    pub fn new() -> Self {
        AstMap {
            mapping: Vec::new(),
        }
    }
}
