//!Strings are stored inside a global vector and symbols are are
//!indices to that global vector.



#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct SymbolIndex(pub u32);


impl SymbolIndex {
    #[inline]
    pub fn from_u32(n: u32) -> Self {
        SymbolIndex(n)
    }

    
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}


#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct Symbol(SymbolIndex);


impl Symbol {
    pub fn new(n: u32) -> Self {
        Symbol(SymbolIndex::from_u32(n))
    }
}

