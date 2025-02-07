use std::{
    io,
    num::NonZeroU8,
    ops::{Index, IndexMut},
};

use crate::instruction::{AbortReason, Opcode, RelocatableOffset};

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
