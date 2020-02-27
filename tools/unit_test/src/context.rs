//! Macros for easily setting up compiler contexts for every 
//! step in the compilation pipeline.


use sqrrlc::core::session::Session;
use sqrrlc::lexer::tokenize;
use sqrrlc::parser::ParseCtxt;


#[macro_export]
macro_rules! create_source_file {
    // Create source file from a given file path
    {file: $file:expr, $session:expr} => (
        match $session.source_map().load_file(&file) {
            Ok(file) => file,
            Err(err) => panic!("could not find the given file `{}`", $file),
        }
    );
    
    // Create source file from given code input
    {source: $code:expr, name: $name:expr, $session:expr} => (
        sess.source_map().insert_source_file(name, input)
    );
}



#[macro_export]
macro_rules! create_parse_ctx {
    ($source:expr) => (
        let mut session = Session::new();
        let file = create_source_file!{source: $source, name: "test", sess};
        let mut tokens = tokenize(&file.source, file.start_pos.index());
        ParseCtxt {
            sess: &mut session,
            file: file,
            tokens: tokens.peekable(),
            ast_map: AstMap::new(),
        }
    );
}
