
/***************************************************************************
 * The memory submodule defines the memory modell used by the interpreter.
 ***************************************************************************/

use std::fmt;
use crate::sqrrlc::session::Session;
use crate::sqrrlc_interp::{
    value::*,
    IResult,
};


/**
 * The main memory storage for interpreter.
 */
pub struct Memory<'a> {
    /// The compiler session.
    sess: &'a Session,
    
    /// The data storage is a vector of memory entries.
    data: Vec<MemEntry>,

    /// The next address of unallocated space.
    next: usize,
}


/**
 * Memory entry is an entry in the memory storage.
 * An entry holds the data and some other meta data.
 */
#[derive(Clone)]
struct MemEntry {
    /// Is this memory enrty mutable, or immuatable?
    mutable: bool,

    /// Is this entry reserved or avaible to be allocated.
    reserved: bool,

    /// The actual data that is stored in the memory entry.
    data: ValData,
}


/**
 * Implementation of memory entry.
 */
impl MemEntry {
    /**
     * Creates a new empty memory entry.
     */
    pub fn new() -> Self {
        MemEntry {
            mutable: false,
            reserved: false,
            data: ValData::None,
        }
    }
}


/**
 * Implementation of the memory module.
 */
impl<'a> Memory<'a> {
    /**
     * Constructs an empty memory module. Default capacity
     * is 1000, use `Memory::with_capacity` to set a custom capacity.
     */
    pub fn new(session: &'a Session) -> Self {
        Memory {
            sess: session,
            data: vec![MemEntry::new(); 1000],
            next: 0,
        }
    }


    /**
     * Constructs an empty memory module with specific capacity.
     */
    pub fn with_capacity(session: &'a Session, capacity: usize) -> Self {
        Memory {
            sess: session,
            data: vec![MemEntry::new(); capacity],
            next: 0,
        }
    }


    /**
     * Allocates value data and returns the memory address to that data.
     */
    pub fn alloc(&mut self, val: &Val, mutable: bool) -> IResult<usize> {
        if self.next < self.data.capacity() {
            self.data[self.next] = MemEntry {
                mutable,
                reserved: true,
                data: val.data.clone(),
            };
            let addr = self.next;
            self.find_next();
            Ok(addr)
        } else {
            Err(struct_span_fatal!(self.sess, val.span, "out of memory error"))
        }
    }


    /**
     * Loads a value from memory at specific address.
     */
    pub fn load(&self, addr: usize) -> IResult<ValData> {
        if addr < self.data.capacity() {
            let entry = &self.data[addr];
            if entry.data.has_data() {
                Ok(entry.data.clone())
            } else {
                Err(struct_fatal!(self.sess, "reading unallocated memory"))
            }
        } else {
            Err(struct_fatal!(self.sess, "reading out of bounds"))
        }
    }


    /**
     * Stores a new value at a specific memory address.
     * Note: has to be an already allocated address.
     */
    pub fn store(&mut self, addr: usize, val: &Val) -> IResult<()> {
        if addr < self.data.capacity() {
            let entry = &mut self.data[addr];
            if entry.reserved || entry.data.has_data() {
                if entry.mutable || !entry.data.has_data() {
                    entry.data = val.data.clone();
                    Ok(())
                } else {
                    Err(struct_span_fatal!(self.sess, val.span, "cannot assign twice to immutable variable"))
                }
            } else {
                Err(struct_span_fatal!(self.sess, val.span, "cannot update unallocated memory"))
            }
        } else {
            Err(struct_fatal!(self.sess, "writing out of bounds"))
        }
    }
    

    /**
     * Frees the memory at the specific memory address.
     */
    pub fn free(&mut self, addr: usize) -> IResult<()> {
        if addr < self.data.capacity() {
            if self.is_alloc(addr) {
                let entry = &mut self.data[addr];
                entry.data = ValData::None;
                entry.reserved = false;
                if self.next > addr {
                    self.next = addr;
                }
                Ok(())
            } else {
                Err(struct_fatal!(self.sess, "cannot free unallocated memory"))
            }
        } else {
            Err(struct_fatal!(self.sess, "cannot free out of bounds"))
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
     * Check if a memory address is allocated, or reserved for future allcations.
     */
    fn is_alloc(&self, addr: usize) -> bool {
        let entry = &self.data[addr];
        entry.reserved || entry.data.has_data()
    }


    /**
     * Dumps the memory from address 0 to the last allocated address.
     */
    fn memory_dump(&self) -> String {
        let mut dump = String::new();
        dump.push_str("[");
        for i in 0..self.next {
            let mut mutable = String::new();
            if self.data[i].mutable {
                mutable.push_str(" mut");
            }
            dump.push_str(&format!("\n    [{}{}] = {:?},", i, mutable, self.data[i]));
        }
        dump.pop();
        dump.push_str("\n]");
        return dump;
    }
}


/**
 * Formatting the memory so it does print the entire memory state.
 */
impl<'a> fmt::Debug for Memory<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Memory:")
            .field("capacity", &self.data.len())
            .field("next", &self.next)
            .field("data", &format_args!("{}", self.memory_dump()))
            .finish()
    }
}


/**
 * Debug formatting for memory entry.
 */
impl fmt::Debug for MemEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.reserved || self.data.has_data() {
            write!(f, "{}", self.data)
        } else {
            write!(f, "reserved")
        }
    }
}
