#![allow(dead_code)]

/***************************************************************************
 * The scope sbumodule defines a scope and is used with the callstack and
 * also for sub-block expressions.
 ***************************************************************************/


use std::fmt;
use std::collections::HashMap;
use crate::sqrrlc::{
    source_map::SourceFile,
    session::Session,
};
use crate::sqrrlc_ast::{
    span::Span,
    expr::ExprIdent,
};
use crate::sqrrlc_interp::{
    env::write_str,
    IResult,
};


/**
 * The scope is used to handle the memory inside a particular scope. 
 * For nested blocks this scope acts as an item in a linked list.
 * The outer block is always defined inside the call stack for
 * the environment.
 */
#[derive(Clone)]
pub struct Scope<'a> {
    /// The compiler session.
    sess: &'a Session,
    
    /// The child scope, used for sub-block expressions.
    child: Box<Option<Scope<'a>>>,

    /// Variable memory mapper, maps strings to memory addresses.
    symbols: HashMap<String, usize>,

    /// Address to values stored in this scope, used to free memory later.
    values: Vec<usize>,

    /// The location in code where this scope was created from.
    pub span: Span,
}


/**
 * Implementation of scope.
 */
impl<'a> Scope<'a> {
    /**
     * Constructs a new scope.
     */
    pub fn new(session: &'a Session, span: Span) -> Self {
        Scope {
            sess: session,
            child: Box::new(None),
            symbols: HashMap::new(),
            values: vec![],
            span: span,
        }
    }


    /**
     * Returns the address of a given variable identifier.
     * If backtracking is enabled than this will also search child scopes.
     */
    pub fn address_of(&self, id: &ExprIdent, backtrack: bool) -> IResult<usize> {
        match &*self.child {
            Some(child) => {
                match child.address_of(id, backtrack) {
                    Ok(addr) => Ok(addr),
                    Err(e) => {
                        if backtrack {
                            self.find_mem(id)
                        } else {
                            Err(e)
                        }
                    }
                }
            },
            None => self.find_mem(id),
        }
    }

    
    /**
     * Returns the registered addresses.
     */
    pub fn addresses(&self) -> Vec<usize> {
        let mut addresses = self.symbols.iter().map(|(_, addr)| *addr).collect::<Vec<usize>>();
        addresses.extend(&self.values);
        addresses
    }
    

    /**
     * Registers the given address and identifier in the variable list.
     * If you register an already existing identifier then the previous address
     * will be changed to a regular value that is only accessable via the address.
     * This enables the ability to shadow already existing let bindings.
     */
    pub fn register(&mut self, id: &ExprIdent, addr: usize) {
        match &mut *self.child {
            Some(child) => child.register(id, addr),
            None => {
                match self.symbols.insert(id.to_string.clone(), addr) {
                    Some(prev) => self.values.push(prev),
                    None => { },
                }
            }
        };
    }


    /**
     * Registers the given address so it can be freed when exiting this scope.
     */
    pub fn register_addr(&mut self, addr: usize) {
        match &mut *self.child {
            Some(child) => child.register_addr(addr),
            None => self.values.push(addr),
        }
    }


    /**
     * Push a new scope onto the outer most scope.
     */
    pub fn push(&mut self, new_scope: Scope<'a>) {
        match &mut *self.child {
            Some(child) => child.push(new_scope),
            None => self.child = Box::new(Some(new_scope)),
        }
    }


    /**
     * Pops the outer most scope. Returns the popped scope.
     */
    pub fn pop(&mut self) -> IResult<Scope> {
        let (opt, _) = self.pop_impl();
        match opt {
            Some(scope) => Ok(scope.clone()),
            None => Err(struct_fatal!(self.sess, "cannot pop the function scope")),
        }
    }

    
    /**
     * Pop implementation returns true if the outer most
     * scope has been reached and should be popped.
     */
    fn pop_impl(&mut self) -> (Option<Scope<'a>>, bool) {
        match &mut *self.child {
            Some(child) => {
                let (mut scope, found) = child.pop_impl();
                if found {
                    scope = Some(child.clone());
                    self.child = Box::new(None);
                }
                (scope, false)
            },
            None => (None, true),
        }
    }    

    
    /**
     * Find an address in the memory using the provided identifier.
     * Returns memory error if not found in this scope.
     */
    fn find_mem(&self, ident: &ExprIdent) -> IResult<usize> {
        match self.symbols.get(&ident.to_string) {
            Some(addr) => Ok(*addr),
            None => Err(struct_span_fatal!(self.sess, ident.span, "not found in this scope")),
        }
    }


    /**
     * Debug formatting of this scope.
     */
    pub fn fmt_scope(
        &self,
        f: &mut fmt::Formatter<'_>,
        mut indent: usize,
        index: usize
    ) -> fmt::Result {
        match self.sess.source_map().get_file(self.span.loc) {
            Some(file) => {
                write_str(f, &format!("\n{}: ", index), indent - 4)?;
                self.fmt_sub_scope(f, indent + 4, &file, index, 1)?;
                if (*self.child).is_some() {
                    write_str(f, &format!("\n{}.0: ", index), indent)?;
                    indent += 4;
                }
                let line = self.span.start.line;
                if line > 0 {
                    write_str(f, file.get_line(line).trim(), indent)?;
                    write_str(f, &format!("\n at {}:{}", file.filename, line), indent)?;
                } else {
                    write_str(f, &format!("{}", file.filename), indent)?;
                }
                if self.symbols.len() > 0 {
                    write_str(f, &format!("\nSymbols Table: {:#?},", &self.symbols), indent)?;
                }
                write_str(f, "\n},", indent - 4)
            },
            None => {
                write_str(f, "<unknown>", indent)
            }
        }
    }
    

    /**
     * Debug formatting of this child scope.
     */
    pub fn fmt_sub_scope(
        &self,
        f: &mut fmt::Formatter<'_>,
        indent: usize,
        file: &SourceFile,
        index: usize,
        sub_index: usize
    ) -> fmt::Result {
        match &*self.child {
            Some(child) => {
                write_str(f, &format!("\n{}.{}: ", index, sub_index), indent - 4)?;
                let line = child.span.start.line;
                if line > 0 {
                    let code = file.get_line(line).trim().to_owned();
                    if code == "{" {
                        write_str(f, "<block>", indent)?;
                    } else {
                        write_str(f, &code.trim(), indent)?;
                    }
                    write_str(f, file.get_line(line).trim(), indent)?;
                    write_str(f, &format!("\n at {}:{}", file.filename, line), indent)?;
                } else {
                    write_str(f, &format!("{}", file.filename), indent)?;
                }
                if self.symbols.len() > 0 {
                    write_str(f, &format!("\nSymbols Table: {:#?},", &child.symbols), indent)?;
                }
                child.fmt_sub_scope(f, indent + 4, file, index, sub_index + 1)?;
                write_str(f, "\n},", indent - 4)
            }
            None => Ok(()),
        }
    }
}


/**
 * Debug formatting of scopes.
 */
impl<'a> fmt::Debug for Scope<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_scope(f, 0, 0)
    }
}
