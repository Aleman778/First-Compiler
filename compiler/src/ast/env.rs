#![allow(dead_code)]

/***************************************************************************
 * Environment struct used during parsing to cache identifiers.
 ***************************************************************************/


/**
 * Requires HashMap from standard library.
 */
use std::collections::HashMap;


/**
 * Environment is used to cache variable and function identifiers.
 */
#[derive(Debug, PartialEq)]
pub struct Env<'a> {
    ids: HashMap<&'a str, i32>,
    idents: Vec<&'a str>,
}


/**
 * Implementation for parser environment
 */
impl<'a> Env<'a> {

    /**
     * Constructs a new parser environment.
     */
    pub fn new() -> Env<'a> {
        Env {
            ids: HashMap::new(),
            idents: Vec::new(),
        }
    }


    /**
     * Get the id value of an identifier, if this identifier is new
     * then a new id is generated and cached to faster referencing.
     */
    pub fn get_id(&'a mut self, ident: &'a str) -> i32 {
        match self.ids.get(ident) {
            Some(id) => *id,
            None => {
                let id = self.idents.len() as i32;
                self.ids.insert(ident, id);
                self.idents.push(ident);
                return id;
            }
        }
    }


    /**
     * Get the identifier string from an id number.
     */
    pub fn get_identifier(&'a mut self, id: i32) -> Option<&str> {
        let ident = self.idents.get(id as usize)?;
        Some(ident)
    }
}
