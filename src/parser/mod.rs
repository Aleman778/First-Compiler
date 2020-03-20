//! Parser stage of the compiler uses the lexed tokens and combines
//! them to form semantic meaning to the program.


mod stmt;
mod expr;
mod lit;
mod op;
mod utils;


use std::time::Instant;
use log::{debug};
use crate::core::session::Session;
use crate::core::source_map::SourceFile;
use crate::lexer::tokenize;
use crate::lexer::stream::TokenStream;
use crate::parser::expr::parse_expr;
use crate::ast::map::AstMap;


/**
 * Parses the given source file in the given
 * compiler session and using the provided token stream.
 */
pub fn parse_file<'a>(session: &'a mut Session<'a>, file: &'a SourceFile) -> AstMap {
    let tokens = tokenize(&file.source, file.start_pos.index());
    let ctx = ParseCtxt {
        sess: session,
        file: file,
        tokens: tokens,
        ast_map: AstMap::new(),
    };
    do_parse(ctx)
}


/**
 * Parses the usign the given parse context.
 * The resulting ast map is returned.
 */
pub fn do_parse<'a>(mut ctx: ParseCtxt<'a>) -> AstMap {
    debug!("parsing file {}...", ctx.file.name);
    let start = Instant::now();
    let token = ctx.tokens.next().unwrap();
    let expr = parse_expr(&mut ctx, &token, 1);
    let elapsed = start.elapsed();
    debug!("parsed file {} in {:?}", ctx.file.name, elapsed);
    
    println!("{:#?}", expr);

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
    tokens: TokenStream<'a>,
    /// Output mapper used by abstract syntax tree.
    ast_map: AstMap,
}
