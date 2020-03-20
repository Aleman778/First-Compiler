//! Parser implementation for statements.


use crate::lexer::tokens::*;
use crate::parser::ParseCtxt;
use crate::parser::expr;
use crate::ast;
use TokenKind::*;



pub fn parse_stmt(
    ctx: &mut ParseCtxt, 
    token: &Token
) -> Option<ast::Stmt> {
    None
}


pub fn parse_block(
    ctx: &mut ParseCtxt, 
    token: &Token
) -> Option<ast::Block> {
    None
}
