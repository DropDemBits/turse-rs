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

impl TryFrom<StreamKind> for StdStreamKind {
    type Error = &'static str;

    fn try_from(value: StreamKind) -> Result<Self, Self::Error> {
        match value {
            // Default stream kind defaults to `put` operations
            StreamKind::Default => Ok(Self::Put),
            StreamKind::Get => Ok(Self::Get),
            StreamKind::Put => Ok(Self::Put),
            StreamKind::Read | StreamKind::Write => Err("invalid standard stream kind"),
        }
    }
}

// FIXME: Figure out a way of attaching descriptions to property accessors
impl PutKind {
    /// If the put kind has an exponent width option.
    pub fn has_exponent_width(self) -> bool {
        matches!(self, PutKind::IntExp | PutKind::NatExp | PutKind::RealExp)
    }

    /// If the put kind has a fractional width option.
    /// Having an exponential width also implies having a fractional width option.
    pub fn has_fractional_width(self) -> bool {
        matches!(
            self,
            PutKind::IntExp
                | PutKind::NatExp
                | PutKind::RealExp
                | PutKind::IntFract
                | PutKind::NatFract
                | PutKind::RealFract
        )
    }
}
