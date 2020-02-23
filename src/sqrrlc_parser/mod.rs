//! Parser stage of the compiler uses the lexed tokens and combines
//! them to form semantic meaning to the program.


pub mod expr;
pub mod lit;


use std::rc::Rc;
use std::iter::Peekable;
use crate::sqrrlc::session::Session;
use crate::sqrrlc::source_map::SourceFile;
use crate::sqrrlc::span::*;
use crate::sqrrlc_lexer::*;
use crate::sqrrlc_ast::ast_map::AstMap;


/**
 * The parser takes in a file and token stream
 * and combines these tokens and builds the abstract
 * syntax tree.
 */
pub struct Parser<'a> {
    /// Current compiler session.
    sess: &'a mut Session<'a>,
    /// Source file currently being parsed.
    file: &'a SourceFile,
    /// Token stream for lexing input code.
    tokens: Peekable<TokenStream<'a>>,
    /// Output mapper used by abstract syntax tree.
    ast_map: AstMap,
}
    

impl<'a> Parser<'a> {
    /**
     * Create a new parser object
     */
    pub fn new(
        sess: &'a mut Session<'a>,
        file: &'a SourceFile, 
        tokens: TokenStream<'a>
    ) -> Self {
        Parser { 
            sess,
            file,
            tokens: tokens.peekable(), 
            ast_map: AstMap::new(), 
        }
    }
}
