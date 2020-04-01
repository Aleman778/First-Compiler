//! Parser stage of the compiler uses the lexed tokens and combines
//! them to form semantic meaning to the program.


mod item;
mod stmt;
mod expr;
mod lit;
mod ty;
mod utils;


use std::time::Instant;
use log::{debug, info};
use crate::core::session::Session;
use crate::core::source_map::SourceFile;
use crate::lexer::tokenize;
use crate::lexer::stream::TokenStream;
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
    debug!("parsing file `{}`...", ctx.file.name.display());
    let start = Instant::now();
    let mut units = Vec::new();
    
    while let Some(token) = utils::next_non_comment_token(&mut ctx) {
        let item = item::parse_item(&mut ctx, &token);
        if item.is_none() {
            info!("parsing file `{}` failed!", ctx.file.name.display());
            return ctx.ast_map;
        } else {
            units.push(item.unwrap());
        }
    }
    
    let elapsed = start.elapsed();
    debug!("parsed file `{}` in {:?}", ctx.file.name.display(), elapsed);
    println!("{:#?}", units);
    
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
