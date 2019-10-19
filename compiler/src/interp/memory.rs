
/***************************************************************************
 * The memory submodule defines the memory modell used by the interpreter.
 ***************************************************************************/


use crate::interp::value::Val;


/**
 * The main memory storage for interpreter.
 */
#[derive(Debug, Clone)]
pub struct Memory {
    /// The data storage vector of values
    data: Vec<Val>,

    /// The next address of unallocated space
    next: usize,
}


/**
 * Implementation of the memory module.
 */
impl Memory {
    /**
     * Constructs an empry memory module.
     */
    pub fn new() -> Self {
        Memory {
            data: Vec::new(),
            next: 0,
        }
    }
}
