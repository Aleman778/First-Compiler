
/***************************************************************************
 * The error module provides common traits and formatters for easily
 * dealing with errors and printing them to console.
 ***************************************************************************/


use crate::ast::span::LineColumn;


/**
 * General compilation error trait used for retriving
 * information about an error for formatting into a
 * rust-like human readable error message.
 */
pub trait CompileError<'a> {

    fn get_level(&self) -> &'a str;

    
    fn get_file_location(&self) -> (&'a str, LineColumn);

    
    fn get_message(&self) -> &'a str;

}
