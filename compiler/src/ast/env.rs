
/***************************************************************************
 * Environment struct used during parsing to cache identifiers.
 ***************************************************************************/


/**
 * Requires HashMap from standard library.
 */
use std::collections::HashMap;


/**
 * Requires the identifier struct.
 */
use crate::ast::{Span, atom::Ident};


/**
 * Environment is used to cache variable and function identifiers.
 */
#[derive(Debug, PartialEq)]
pub struct Env<'a> {
    pub identifiers: HashMap<&'a str, Ident<'a>>,
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
            identifiers: HashMap::new(),
        }
    }


    // pub fn ident(&'a mut self, s: Span<'a>) -> &'a Ident<'a> {
        // match self.identifiers.get(s.fragment) {
            // Some(ident) => ident,
            // None => {
                // let ident = Ident{to_string: s.fragment, span: s};
                // self.identifiers.insert(s.fragment, ident);
                // return self.ident(s);
            // }
        // }
    // }
}
