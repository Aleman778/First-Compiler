#![allow(dead_code)]

/***************************************************************************
 * Span information for storing location data used for debugging
 ***************************************************************************/


/**
 * Dummy span, points to position 0 and has length of 0.
 */
pub const DUMMY_SPAN: Span = Span { base: 0, len: 0 };


/**
 * The byte position in a source file.
 */
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct BytePos(pub u32);


/**
 * Span data holds the low and high byte position in a given program source.
 * Whenever possible use Span instead as this data structure currently takes 8 bytes.
 */
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct SpanData {
    pub lo: BytePos,
    pub hi: BytePos,
}


impl SpanData {
    /**
     * Convert span data into regular span with specific lo byte position.
     */
    pub fn with_lo(&self, lo: BytePos) -> Span {
        Span::new(lo, self.hi)
    }


    pub fn with_hi(&self, hi: BytePos) -> Span {
        Span::new(self.lo, hi)
    }
}


/**
 * Span defines the base byte position and byte length, in 6 bytes.
 * This data structure should be used over SpanData when possible.
 * 
 */
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub base: u32,
    pub len: u16,
}


impl Span {
    /**
     * Creates a new span from low and high byte positions.
     */
    pub fn new(lo: BytePos, hi: BytePos) -> Self {
        Span {
            base: lo.0,
            len: (hi.0 - lo.0) as u16,
        }
    }


    /**
     * Creates a new span with base position and length.
     */
    pub fn new(base: u32, len: u16) -> Self {
        Span { base, len }
    }
}
