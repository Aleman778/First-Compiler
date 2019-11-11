
/***************************************************************************
 * The `emitter` module is used to convert diagnostic to string
 ***************************************************************************/


use std::rc::Rc;
use std::collections::HashMap;
use ansi_term::{Style, Color};
use crate::sqrrlc::error::diagnostic::*;
use crate::sqrrlc::source_map::SourceMap;


/**
 * The emitter struct prints the diagnostic information
 * to the console.
 */
pub struct Emitter {
    /// Defines the different color styles for each level.
    pub colors: HashMap<Level, Style>,

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
        let mut colors = HashMap::new();
        colors.insert(Level::Note, Color::Cyan.normal());
        colors.insert(Level::Warning, Color::Yellow.normal());
        colors.insert(Level::Error, Color::Red.normal());
        colors.insert(Level::Fatal, Color::Black.on(Color::Red));
        Emitter {
            colors: colors,
            sm: source_map,
        }
    }
    

    /**
     * Emit diagnostic information to console.
     */
    pub fn emit_diagnostic(&self, d: &Diagnostic) {
        let mut result = String::new();
        self.emit_header(&mut result, d);
        if d.has_primary_span() {
            // self.draw_code_snippet();
        }
        println!("{}", result);
    }


    /**
     * Emit the diagnostic header information.
     * E.g. warning: function `do_nothing` is unused
     */
    fn emit_header(&self, result: &mut String, d: &Diagnostic) {
        self.emit_colored(result, d.level, &d.level.to_string());
        result.push_str(": ");
        result.push_str(&d.message());
    }


    // fn draw_code_snippet(&self, result: &mut String, d: &Diagnostic) {
    // }
 

    // fn draw_line(&self, ) {

    // }
    

    /**
     * Emits an ansi colored string based on the diagnostic level.
     */
    pub fn emit_colored(&self, result: &mut String, level: Level, string: &str) {
        match self.colors.get(&level) {
            Some(color) => result.push_str(format!("{}", color.paint(string)).as_str()),
            None => result.push_str(string),
        };
    }
}
