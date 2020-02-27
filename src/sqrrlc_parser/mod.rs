//! Parser stage of the compiler uses the lexed tokens and combines
//! them to form semantic meaning to the program.


mod expr;
mod lit;


use std::iter::Peekable;
use crate::sqrrlc::session::Session;
use crate::sqrrlc::source_map::SourceFile;
use crate::sqrrlc_lexer::*;
use crate::sqrrlc_parser::expr::parse_expr;
use crate::sqrrlc_ast::ast_map::AstMap;


/**
 * Parses the given source file in the given
 * compiler session and using the provided token stream.
 */
pub fn parse_file<'a>(session: &'a mut Session<'a>, file: &'a SourceFile) -> AstMap {
    let mut tokens = tokenize(&file.source, file.start_pos.index());
    let mut ctx = ParseCtxt {
        sess: session,
        file: file,
        tokens: tokens.peekable(),
        ast_map: AstMap::new(),
    };
    do_parse(ctx)
}


/**
 * Parses the usign the given parse context.
 * The resulting ast map is returned.
 */
pub fn do_parse<'a>(mut ctx: ParseCtxt<'a>) -> AstMap {
    parse_expr(&mut ctx);
    ctx.ast_map
}


/**
 * The parser takes in a file and token stream
 * and combines these tokens and builds the abstract
 * syntax tree.
 */
pub struct ParseCtxt<'a> {
    /// Current compiler session.
    sess: &'a mut Session<'a>,
    /// Source file currently being parsed.
    file: &'a SourceFile,
    /// Token stream for lexing input code.
    tokens: Peekable<TokenStream<'a>>,
    /// Output mapper used by abstract syntax tree.
    ast_map: AstMap,
}
