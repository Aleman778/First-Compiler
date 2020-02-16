//! Parser stage of the compiler uses the lexed tokens and combines
//! them to form semantic meaning to the program.


use crate::sqrrlc_lexer::*;
use crate::sqrrlc_ast::ast_map::AstMap;
use crate::sqrrlc_ast::*;


pub struct Parser<'a> {
    // sess: &'a mut Session,
    /// The token stream for lexing input code.
    tokens: &'a mut TokenStream<'a>,
    /// The mapper for abstract syntax tree.
    ast_map: AstMap<'a>,
    /// The base position accumulate for each token
    base_pos: usize,
}


impl<'a> Parser<'a> {
    fn next_token() -> Token {
        Token::new(TokenKind::Unknown, 0)
    }
}

