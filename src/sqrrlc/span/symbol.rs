//!Strings are stored inside a global vector and symbols are are
//!indices to that global vector.


use std::collections::HashMap;


/**
 * Module containing all the keywords supported.
 */
pub mod kw {
    use super::Symbol;

    /// Empty strings are invalid symbols.
    pub const Invalid: Symbol    = Symbol::new(0);
    /// Underscore refers to unused variable.
    pub const Underscore: Symbol = Symbol::new(1);
    /// Keyword for type casting.
    pub const As: Symbol         = Symbol::new(2);
    /// Keyword for breaking out of loops.
    pub const Break: Symbol      = Symbol::new(3);
    /// Keyword for continuing to next loop cycle.
    pub const Continue: Symbol   = Symbol::new(4);
    /// Keyword for defining code on false if-condition.
    pub const Else: Symbol       = Symbol::new(5);
    /// Keyword for declaring an enumerator.
    pub const Enum: Symbol       = Symbol::new(6);
    /// Keyword defines the boolean value false.
    pub const False: Symbol      = Symbol::new(7);
    /// Keyword for declaring functions.
    pub const Fn: Symbol         = Symbol::new(8);
    /// Keyword for declaring for loops.
    pub const For: Symbol        = Symbol::new(9);
    /// Keyword for declaring if-statement.
    pub const If: Symbol         = Symbol::new(10);
    /// Keyword for declaring a let binding.
    pub const Let: Symbol        = Symbol::new(11);
    /// Keyword for annotating mutability for variable.
    pub const Mut: Symbol        = Symbol::new(12);
    /// Keyword for returing value from function.
    pub const Return: Symbol     = Symbol::new(13);
    /// Keyword for declaring a data structure.
    pub const Struct: Symbol     = Symbol::new(14);
    /// Keyword defines the boolean value true.
    pub const True: Symbol       = Symbol::new(15);
    /// Keyword declares a while loop.
    pub const While: Symbol      = Symbol::new(16);
}


/**
 * Module containig all the symbols predefined by the compiler.
 */
pub mod sym {
    use super::Symbol;

    /// Boolean type symbol.
    pub const bool: Symbol  = Symbol::new(17);
    /// 32-bit floating-point type symbol.
    pub const f32: Symbol   = Symbol::new(18);
    /// 64-bit floating-point type symbol.
    pub const f64: Symbol   = Symbol::new(19);
    /// 8-bit signed integer type symbol.
    pub const i8: Symbol    = Symbol::new(20);
    /// 16-bit signed integer type symbol.
    pub const i16: Symbol   = Symbol::new(21);
    /// 32-bit signed integer type symbol.
    pub const i32: Symbol   = Symbol::new(22);
    /// 64-bit signed integer type symbol.
    pub const i64: Symbol   = Symbol::new(23);
    /// 128-bit signed integer type symbol.
    pub const i128: Symbol  = Symbol::new(24);
    /// 32- or 64-bit signed integer type symbol.
    pub const isize: Symbol = Symbol::new(25);
    /// String type symbol.
    pub const str: Symbol   = Symbol::new(26);
    /// 8-bit unsigned integer type symbol.
    pub const u8: Symbol    = Symbol::new(27);
    /// 16-bit unsigned integer type symbol.
    pub const u16: Symbol   = Symbol::new(28);
    /// 32-bit unsigned integer type symbol.
    pub const u32: Symbol   = Symbol::new(29);
    /// 64-bit unsigned integer type symbol.
    pub const u64: Symbol   = Symbol::new(30);
    /// 128-bit unsigned integer type symbol.
    pub const u128: Symbol  = Symbol::new(31);
    /// 32- or 64-bit unsigned integer type symbol.
    pub const usize: Symbol = Symbol::new(32);
}


#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct Symbol(pub u32);


impl Symbol {
    #[inline]
    const fn new(index: usize) -> Self {
        Symbol(index as u32)
    }
    

    #[inline]
    fn index(self) -> usize {
        self.0 as usize
    }
}


/**
 * Maps symbols to indices and vice versa.
 */
#[derive(Default)]
pub struct SymbolMap {
    /// All the symbol strings used to lookup by index.
    strings: Vec<&'static str>,
    /// Maps symbol names to indices, used to lookup by name.
    names: HashMap<&'static str, Symbol>,
}


impl SymbolMap {
    pub fn new() -> Self {
        Self::prefill(&[
            // Compiler defined keywords
            "",
            "_",
            "as",   
            "break",
            "continue",
            "else",
            "false",
            "fn",
            "for",
            "if",
            "let",
            "mut",
            "return",
            "struct",
            "true",
            "while",

            // Compiler defined symbols
            "bool",
            "f32",
            "f64",
            "i8",
            "i16",
            "i32",
            "i64",
            "i128",
            "isize",
            "str",
            "u8",
            "u16",
            "u32",
            "u64",
            "u128",
            "usize",
        ])
    }
    
    
    /**
     * Fills the symbols before initialization.
     */
    fn prefill(init: &[&'static str]) -> Self {
        SymbolMap {
            strings: init.into(),
            names: init.iter().copied().zip((0..).map(Symbol::new)).collect(),
        }
    }


    /**
     * Get the string corresponding to the given symbol.
     */
    pub fn as_str(&self, sym: Symbol) -> &'static str {
        self.strings[sym.index()]
    }


    /**
     * Get the symbol from the given string.
     * If symbol does't already then new symbol is created,
     * otherwise previous stored symbol is loaded instead.
     */
    pub fn as_symbol(&mut self, name: &'static str) -> Symbol {
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
