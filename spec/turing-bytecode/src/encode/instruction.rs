use std::{
    io,
    num::NonZeroU8,
    ops::{Index, IndexMut},
};

// Will be generated
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Opcode {
    ABORT = 0x00,
    ADDINTNAT = 0x05,
    CALL = 0x32,
    CASE = 0x35,
    JUMP = 0x89,
    JUMPB = 0x8A,
    LOCATELOCAL = 0x96,
    LOCATETEMP = 0x98,
    PROC = 0xBA,
    PUSHADDR1 = 0xBC,
    PUSHVAL0 = 0xC2,
    PUSHVAL1 = 0xC3,
    RETURN = 0xCD,
}

impl Opcode {
    /// Size of the opcode, in bytes.
    pub fn size(&self) -> usize {
        4
    }
}

// Will be generated
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u32)]
pub enum AbortReason {
    NoResult = 9,
}
//
// Can be generated
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelocatableOffset {
    pub link: u32,
    pub offset: u32,
}

impl RelocatableOffset {
    pub fn new(link: u32, offset: u32) -> Self {
        Self { link, offset }
    }

    /// Creates an unlinked relocatable offset
    pub fn empty() -> Self {
        Self { link: 0, offset: 0 }
    }
}

// Can be generated
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operand {
    AbortReason(AbortReason),
    Nat4(u32),
    Offset(u32),
    RelocatableOffset(RelocatableOffset),
}

impl Operand {
    /// Size of the operand, in bytes.
    pub fn size(&self) -> usize {
        match self {
            Operand::AbortReason(_) => 4,
            Operand::Nat4(_) => 4,
            Operand::Offset(_) => 4,
            Operand::RelocatableOffset(_) => 8,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct OperandRef(NonZeroU8);

// Can be generated
const MAX_OPERANDS: usize = 4;

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    operands: [Option<Operand>; MAX_OPERANDS],
}

impl Instruction {
    pub fn new(opcode: Opcode) -> Self {
        Self {
            opcode,
            operands: [None, None, None, None],
        }
    }

    pub fn with_operand(mut self, operand: Operand) -> Self {
        let Some(slot) = self.operands.iter_mut().find(|it| it.is_none()) else {
            unreachable!("exceeded max number of operands {MAX_OPERANDS}")
        };
        *slot = Some(operand);
        self
    }

    pub fn opcode(&self) -> Opcode {
        self.opcode
    }

    pub fn operands(&self) -> impl Iterator<Item = &Operand> {
        self.operands
            .iter()
            .take_while(|slot| slot.is_some())
            .flat_map(|slot| Some(slot.as_ref()?))
    }

    pub fn operand_refs(&self) -> impl Iterator<Item = OperandRef> + use<'_> {
        self.operands
            .iter()
            .take_while(|slot| slot.is_some())
            .enumerate()
            .flat_map(|(index, _)| Some(OperandRef(NonZeroU8::new((index as u8) + 1).unwrap())))
    }

    /// Size of the instruction, in bytes.
    pub fn size(&self) -> usize {
        self.opcode().size() + self.operands().map(|operand| operand.size()).sum::<usize>()
    }

    /// Encodes the instruction into the equivalent byte representation.
    pub fn encode(&self, out: &mut impl io::Write) -> io::Result<()> {
        use byteorder::{WriteBytesExt, LE};

        // this can be generated?
        out.write_u32::<LE>(self.opcode() as u32)?;

        for operand in self.operands() {
            match operand {
                Operand::AbortReason(value) => out.write_u32::<LE>(*value as u32)?,
                Operand::Nat4(value) => out.write_u32::<LE>(*value)?,
                Operand::Offset(value) => out.write_u32::<LE>(*value)?,
                Operand::RelocatableOffset(value) => {
                    out.write_u32::<LE>(value.link)?;
                    out.write_u32::<LE>(value.offset)?;
                }
            }
        }

        Ok(())
    }
}

impl Index<OperandRef> for Instruction {
    type Output = Operand;

    fn index(&self, index: OperandRef) -> &Self::Output {
        self.operands[index.0.get().saturating_sub(1) as usize]
            .as_ref()
            .unwrap()
    }
}

impl IndexMut<OperandRef> for Instruction {
    fn index_mut(&mut self, index: OperandRef) -> &mut Self::Output {
        self.operands[index.0.get().saturating_sub(1) as usize]
            .as_mut()
            .unwrap()
    }
}
