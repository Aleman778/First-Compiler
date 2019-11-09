
/***************************************************************************
 * The `emitter` module is used to convert diagnostic to string
 ***************************************************************************/


use ansi_term::{Style, Color};
use std::collections::HashMap;
use crate::sqrrlc::error::diagnostic::*;


/**
 * The emitter struct prints the diagnostic information
 * to the console.
 */
pub struct Emitter {
    pub colors: HashMap<Level, Style>,
}


/**
 * Implementation for emitter.
 */
impl Emitter {
    /**
     * Creates a new emitter with default settings.
     */
    pub fn new() -> Self {
        let mut colors = HashMap::new();
        colors.insert(Level::Note, Color::Cyan.normal());
        colors.insert(Level::Warning, Color::Yellow.normal());
        colors.insert(Level::Error, Color::Red.normal());
        colors.insert(Level::Fatal, Color::White.on(Color::Red));
        Emitter {
            colors: colors,
        }
    }
    

    /**
     * Emit diagnostic information to console.
     */
    pub fn emit_diagnostic(&self, d: &Diagnostic) {
        let mut result = String::new();
        self.emit_header(&mut result, d);
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
