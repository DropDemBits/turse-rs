//! Instruction definitions that are used by both encoding and decoding
//! bytecode blobs.

mod generated;

pub use generated::*;

impl Opcode {
    /// Size of the opcode, in bytes.
    pub fn size(&self) -> usize {
        4
    }
}

impl RelocatableOffset {
    pub fn new(link: Offset, offset: Offset) -> Self {
        Self { link, offset }
    }

    /// Creates an unlinked relocatable offset
    pub fn empty() -> Self {
        Self { link: 0, offset: 0 }
    }
}
