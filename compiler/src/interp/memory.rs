
/***************************************************************************
 * The memory submodule defines the memory modell used by the interpreter.
 ***************************************************************************/

use std::fmt;
use crate::ast::span::Span;
use crate::interp::{
    error::RuntimeError,
    value::Val,
    IResult,
};


/**
 * The main memory storage for interpreter.
 */
#[derive(Clone)]
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
     * Constructs an empty memory module. Default capacity
     * is 1000, use `Memory::with_capacity` to set a custom capacity.
     */
    pub fn new() -> Self {
        Memory {
            data: vec![Val::None; 1000],
            next: 0,
        }
    }


    /**
     * Constructs an empty memory module with specific capacity.
     */
    pub fn with_capacity(capacity: usize) -> Self {
        Memory {
            data: vec![Val::None; capacity],
            next: 0,
        }
    }


    /**
     * Allocates a value and returns the memory address.
     */
    pub fn alloc(&mut self, val: Val) -> IResult<usize> {
        if !val.has_value() {
            Err(RuntimeError::memory_error(val.get_span(), "cannot store empty value"))
        } else if self.next < self.data.capacity() {
            self.data[self.next] = val;
            let addr = self.next;
            self.find_next();
            Ok(addr)
        } else {
            Err(RuntimeError::memory_error(val.get_span(), "out of memory error"))
        }
    }


    /**
     * Loads a value from memory at specific address.
     */
    pub fn load(&self, addr: usize) -> IResult<Val> {
        if addr < self.data.capacity() {
            match self.data.get(addr) {
                Some(val) => Ok(val.clone()),
                None => Err(RuntimeError::memory_error(Span::new_empty(), "reading unallocated memory")),
            }
        } else {
            Err(RuntimeError::memory_error(Span::new_empty(), "reading out of bounds"))
        }
    }


    /**
     * Stores a new value at a specific memory address.
     * Note: has to be an already allocated address.
     */
    pub fn store(&mut self, addr: usize, val: Val) -> IResult<()> {
        if addr < self.data.capacity() {
            let prev = &self.data[addr];
            match prev {
                Val::None => Err(RuntimeError::memory_error(val.get_span(), "cannot update unallocated memory")),
                _ => {
                    self.data[addr] = val;
                    Ok(())
                },
            }
        } else {
            Err(RuntimeError::memory_error(Span::new_empty(), "writing out of bounds"))
        }
    }
    

    /**
     * Frees the memory at the specific memory address.
     */
    pub fn free(&mut self, addr: usize) -> IResult<()> {
        if addr < self.data.capacity() {
            if self.is_alloc(addr) {
                self.data[addr] = Val::None;
                if self.next > addr {
                    self.next = addr;
                }
                Ok(())
            } else {
                Err(RuntimeError::memory_error(Span::new_empty(), "cannot free unallocated memory"))
            }
        } else {
            Err(RuntimeError::memory_error(Span::new_empty(), "cannot free out of bounds"))
        }
    }


    /**
     * Find the next unallocated memory address.
     */
    fn find_next(&mut self) {
        loop {
            self.next += 1;
            if self.next < self.data.capacity() {
                if !self.is_alloc(self.next) {
                    break;
                }
            } else {
                break;
            }
        }
    }

    
    /**
     * Check if a memory address is allocated.
     */
    fn is_alloc(&self, addr: usize) -> bool {
        match self.data[addr] {
            Val::None => false,
            _ => true,
        }
    }


    /**
     * Dumps the memory from address 0 to the last allocated address.
     */
    fn memory_dump(&self) -> String {
        let mut dump = String::new();
        dump.push_str("[");
        for i in 0..self.next {
            dump.push_str(format!("\n    [{}] = {},", i, self.data[i]).as_str());
        }
        dump.pop();
        dump.push_str("\n]");
        return dump;
    }
}


/**
 * Formatting the memory so it does print the entire memory state.
 */
impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Memory")
            .field("capacity", &self.data.len())
            .field("next", &self.next)
            .field("data", &format_args!("{}", self.memory_dump()))
            .finish()
    }
}
