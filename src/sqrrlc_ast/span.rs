#![allow(dead_code)]

/***************************************************************************
 * Span information for storing location data used for debugging
 ***************************************************************************/


use std::fmt;
use crate::sqrrlc_parser::ParseSpan;


pub struct BytePos(pub u32);


/**
 * Span data holds the low and high byte position in a given program source.
 * Whenever possible use Span instead as this data structure currently takes 8 bytes.
 */
pub struct SpanData {
    pub lo: Pos,
    pub hi: Pos,
}


impl SpanData {
    /**
     * Convert span data into regular span with specific lo byte position.
     */
    pub fn with_lo(&self, lo: BytePos) -> Span {
        SpanData::new(lo, self.hi)
    }


    pub fn with_hi(&self, hi: BytePos) -> Span {
        SpanData::new(self.lo, hi)
    }
}


/**
 * Span defines the base byte position and byte length, in 6 bytes.
 * This data structure should be used over SpanData when possible.
 * 
 */
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Span {
    pub base: u32,
    pub len: u16,
}


impl Span {
    /**
     * Creates a new span from low and high byte positions.
     */
    pub fn new(lo: BytePos, hi: BytePos) {
        Span {
            base: lo.0,
            len: hi.0 - lo.0,
        }
    }
}
