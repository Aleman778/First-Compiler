
/***************************************************************************
 * Environment struct used during parsing to cache identifiers.
 ***************************************************************************/


/**
 * Requires HashMap from standard library.
 */
use std::collections::HashMap;

use crate::ast::{Span, atom::Ident};


/**
 * Environment is used to cache variable and function identifiers.
 */
#[derive(Debug, PartialEq)]
pub struct Env<'a> {
    variables: HashMap<&'a str, Ident<'a>>,
    functions: HashMap<&'a str, Ident<'a>>,
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
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }


    /**
     * Retrive the variable identifier 
     */
    pub fn var(&'a mut self, ident: Span<'a>) -> &'a mut Ident<'a> {
        self.variables.entry(ident.fragment).or_insert(Ident{to_string: ident.fragment, span: ident})
    }


    pub fn func(&'a mut self, ident: Span<'a>) -> &'a mut Ident<'a> {
        self.functions.entry(ident.fragment).or_insert(Ident{to_string: ident.fragment, span: ident})
    }
}
