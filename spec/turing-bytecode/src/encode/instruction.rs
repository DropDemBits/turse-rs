//! Encoded instruction format.
use std::{
    io,
    num::{NonZeroU32, NonZeroU8},
    ops::{Index, IndexMut},
};

use crate::instruction::Opcode;

#[rustfmt::skip]
mod generated;

pub use generated::*;

/// A reference to an [`Operand`] within an [`Instruction`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct OperandRef(NonZeroU8);

/// An encoded instruction.
///
/// [`OperandRef`]s can be used to access and modify individual operands of an instruction.
#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    operands: [Option<Operand>; MAX_OPERANDS],
}

impl Instruction {
    pub fn new(opcode: Opcode) -> Self {
        Self {
            opcode,
            operands: [None; MAX_OPERANDS],
        }
    }

    pub fn with_operand(mut self, operand: Operand) -> Self {
        let Some(slot) = self.operands.iter_mut().find(|it| it.is_none()) else {
            unreachable!("exceeded max number of operands {MAX_OPERANDS}")
        };
        *slot = Some(operand);
        self
    }

    /// Encoded opcode.
    pub fn opcode(&self) -> Opcode {
        self.opcode
    }

    /// Iterator over the encoded instruction's operands.
    pub fn operands(&self) -> impl Iterator<Item = &Operand> {
        self.operands
            .iter()
            .take_while(|slot| slot.is_some())
            .flat_map(|slot| Some(slot.as_ref()?))
    }

    /// Iterator over [`OperandRef`]s that refer to the instruction's operands.
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

        out.write_u32::<LE>(self.opcode() as u32)?;

        for operand in self.operands() {
            operand.encode(out)?;
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

/// Encodes a sequence of instructions.
///
/// Each instruction has a function that generates an [`Instruction`] of the right format.
#[derive(Debug, Default)]
pub struct InstructionEncoder {
    instrs: Vec<Instruction>,
}

/// A reference to an [`Instruction`] inside of a [`InstructionEncoder`]
/// or procedure instruction list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct InstructionRef(NonZeroU32);

impl InstructionRef {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

impl InstructionEncoder {
    pub fn new() -> Self {
        Self { instrs: vec![] }
    }

    /// Finishes encoding instructions, returning all of the encoded
    /// instructions.
    pub fn finish(self) -> Box<[Instruction]> {
        self.instrs.into_boxed_slice()
    }

    /// Gets an [`InstructionRef`] to the potential next instruction.
    /// Intended for prospectively placing a branch target at an instruction
    /// that's yet to be encoded.
    pub fn next_ref(&self) -> InstructionRef {
        NonZeroU32::new(self.instrs.len().saturating_add(1) as u32)
            .map(InstructionRef)
            .unwrap()
    }

    /// Every instruction that has been encoded so far.
    pub fn instrs(&self) -> &[Instruction] {
        &self.instrs
    }

    /// Every instruction that has been encoded so far, as a mutable slice.
    pub fn instrs_mut(&mut self) -> &mut [Instruction] {
        &mut self.instrs
    }

    fn add(&mut self, instr: Instruction) -> InstructionRef {
        let slot = self.next_ref();
        self.instrs.push(instr);
        slot
    }
}

impl Index<InstructionRef> for InstructionEncoder {
    type Output = Instruction;

    fn index(&self, index: InstructionRef) -> &Self::Output {
        &self.instrs[index.as_usize()]
    }
}

impl IndexMut<InstructionRef> for InstructionEncoder {
    fn index_mut(&mut self, index: InstructionRef) -> &mut Self::Output {
        &mut self.instrs[index.as_usize()]
    }
}
