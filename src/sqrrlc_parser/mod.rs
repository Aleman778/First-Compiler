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
    sess: &'a mut Session,
    /// Source file currently being parsed.
    file: Rc<SourceFile>,
    /// Token stream for lexing input code.
    tokens: Peekable<TokenStream<'a>>,
    /// Output mapper used by abstract syntax tree.
    ast_map: AstMap<'a>,
}
    

impl<'a> Parser<'a> {
    /**
     * Create a new parser object
     */
    pub fn new(
        sess: &'a mut Session,
        file: Rc<SourceFile>, 
        tokens: TokenStream<'a>
    ) -> Self {
        Parser { 
            sess,
            file,
            tokens: tokens.peekable(), 
            ast_map: AstMap::new(), 
        }
    }

        
    /**
     * Get the next token in token stream and consume it.
     */
    pub fn next_token(&mut self) -> Option<Token> {
        let token = self.tokens.next()?;
        self.check_token(token)
    }


    /**
     * Get the next token in the token stream without consuming it.
     */
    pub fn peek_token(&mut self) -> Option<Token> {
        let token = *self.tokens.peek()?;
        self.check_token(token)
    }


    /**
     * Check if a given token 
     */
    fn check_token(&self, token: Token) -> Option<Token> {
        if let TokenKind::Unknown = token.kind {
            let span = Span::new(token.base, token.len);
            span_err!(self.sess, span, "unknown start of a token `{}`", self.file.get_source(span));
            None
        } else {
            Some(token)
        }
    }
}

