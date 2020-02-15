
/***************************************************************************
 * Parser stage of the compiler uses the lexed tokens and combines
 * them to form semantic meaning to the program.
 ***************************************************************************/


use crate::sqrrlc_lexer::TokenStream;
use crate::sqrrlc_ast::*;


pub struct Parser<'a> {
    // sess: &'a mut Session,
    /// The token stream for lexing input code.
    tokens: &'a mut TokenStream<'a>,
    /// The base position accumulate for each token
    base_pos: usize,
}


impl<'a> Parser<'a> {
    
     // * Get the next token in the stream that
     // * is not a whitespace token.
//     fn next_token() -> Token {
//         loop {
//             let token = tokens.next();
//             base_pos 
//             if let TokenKind::Whitespace = token.kind {
                
//             }
//         }
//     }
    
//     fn parse_expr(&mut self) -> Expr {
        
//     }


//     fn parse_binary(&mut self) -> Expr {
        
//     }
    
    
//     fn parse_literal(&mut self) -> ExprKind {
        
//     } 
}



// fn parse(tokens: TokenStream) -> Expr {
    // let parser = Parser { tokens };
    // parser.parse_expr()
// }
