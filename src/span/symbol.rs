//!Strings are stored inside a global vector and symbols are are
//!indices to that global vector.


use std::collections::HashMap;
use macros::define_symbols;


define_symbols! {
    Keywords {
        Invalid:    "",
        Underscore: "_",
        As:         "as",
        Break:      "break",
        Continue:   "continue",
        Defer:      "Defer",
        Else:       "else" ,
        Enum:       "enum",
        Extern:     "extern",
        False:      "false",
        Fn:         "fn",
        For:        "for",
        Hidden:     "hidden",
        If:         "if",
        In:         "in",
        Loop:       "loop",
        Return:     "return",
        Struct:     "struct",
        True:       "true",
        While:      "while",
    }
    Symbols {
        bool,
        char,
        f32, 
        f64, 
        i8, 
        i16, 
        i32, 
        i64, 
        i128, 
        isize, 
        str, 
        u8,
        u16,
        u32, 
        u64, 
        u128, 
        usize, 
    }
}


#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct Symbol(pub u32);


impl Symbol {
    #[inline]
    const fn new(index: usize) -> Self {
        Symbol(index as u32)
    }
    

    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}


/**
 * Maps symbols to indices and vice versa.
 */
#[derive(Default)]
pub struct SymbolMap<'a> {
    /// All the symbol strings used to lookup by index.
    strings: Vec<&'a str>,
    /// Maps symbol names to indices, used to lookup by name.
    names: HashMap<&'a str, Symbol>,
}


impl<'a> SymbolMap<'a> {
    /**
     * Fills the symbols before initialization.
     */
    fn prefill(init: &'a [&str]) -> Self {
        SymbolMap {
            strings: init.into(),
            names: init.iter().copied().zip((0..).map(Symbol::new)).collect(),
        }
    }


    /**
     * Get the string corresponding to the given symbol.
     */
    pub fn as_str(&self, sym: Symbol) -> &'a str {
        self.strings[sym.index()]
    }


    /**
     * Get the symbol from the given string.
     * If symbol does't already then new symbol is created,
     * otherwise previous stored symbol is loaded instead.
     */
    #[inline]
    pub fn as_symbol(&mut self, name: &'a str) -> Symbol {
        match self.names.get(name) {
            Some(sym) => *sym,
            None => {
                let sym = Symbol::new(self.strings.len());
                self.strings.push(name);
                self.names.insert(name, sym);
                sym
            }
        }
    }
}
