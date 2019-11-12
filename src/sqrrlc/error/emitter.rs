
/***************************************************************************
 * The `emitter` module is used to convert diagnostic to string
 * Note: this code is based on the rust compiler (but simplified).
 ***************************************************************************/


extern crate termcolor;


use std::rc::Rc;
use crate::sqrrlc::error::{
    styled_buffer::*,
    diagnostic::*,
    snippet::*,
};
use crate::sqrrlc::source_map::SourceMap;


/**
 * The emitter struct prints the diagnostic information
 * to the console.
 */
pub struct Emitter {
    /// The source map containing all the source files.
    pub sm: Rc<SourceMap>,
}


/**
 * Implementation for emitter.
 */
impl Emitter {
    /**
     * Creates a new emitter with default settings.
     */
    pub fn new(source_map: Rc<SourceMap>) -> Self {
        Emitter {
            sm: source_map,
        }
    }
    

    /**
     * Emit diagnostic information to console.
     */
    pub fn emit_diagnostic(&self, d: &Diagnostic) {
        
    }


    pub fn emit_message(
        &self,
        message: &Vec<StyledString>,
        spans: &MultiSpan,
    ) {

    }



    /**
     * Draw the code snippet if there is a primary span that refers to a source file.
     */
    fn draw_code_snippet(
        &self,
        buf: &mut StyledBuffer,
        message: &Vec<StyledString>,
        spans: &MultiSpan,
    ) {
        
    }

    pub fn draw_col_separator(buf: &mut StyledBuffer, line: usize, col: usize) {
        buf.puts("| ", Style::LineNumber, line, col);
    }
}


pub struct Margin {
    pub left_col: usize,

}
