//! Generated by `xtask codegen turing-bytecode`, do not edit by hand

use crate::encode::instruction::{Instruction, InstructionEncoder, InstructionRef};
use crate::instruction::*;
use byteorder::{WriteBytesExt, LE};
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[doc = "An operand for an encoded instruction."]
pub enum Operand {
    Int1(Int1),
    Int2(Int2),
    Int4(Int4),
    Nat2(Nat2),
    Nat4(Nat4),
    Real8(Real8),
    Offset(Offset),
    RelocatableOffset(RelocatableOffset),
    Addrint(Addrint),
    AbortReason(AbortReason),
    PutKind(PutKind),
}
impl Operand {
    #[doc = "Size of the operand, in bytes."]
    pub fn size(&self) -> usize {
        match self {
            Self::Int1(_) => 4usize,
            Self::Int2(_) => 4usize,
            Self::Int4(_) => 4usize,
            Self::Nat2(_) => 4usize,
            Self::Nat4(_) => 4usize,
            Self::Real8(_) => 8usize,
            Self::Offset(_) => 4usize,
            Self::RelocatableOffset(_) => 8usize,
            Self::Addrint(_) => 4usize,
            Self::AbortReason(_) => 4usize,
            Self::PutKind(_) => 4usize,
        }
    }
    #[doc = "Encodes the operand into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Self::Int1(value) => {
                out.write_i8(*value)?;
                out.write_u8(0)?;
                out.write_u8(0)?;
                out.write_u8(0)?;
                Ok(())
            }
            Self::Int2(value) => {
                out.write_i16::<LE>(*value)?;
                out.write_u8(0)?;
                out.write_u8(0)?;
                Ok(())
            }
            Self::Int4(value) => out.write_i32::<LE>(*value),
            Self::Nat2(value) => {
                out.write_u16::<LE>(*value)?;
                out.write_u8(0)?;
                out.write_u8(0)?;
                Ok(())
            }
            Self::Nat4(value) => out.write_u32::<LE>(*value),
            Self::Real8(value) => out.write_f64::<LE>(*value),
            Self::Offset(value) => out.write_u32::<LE>(*value),
            Self::RelocatableOffset(value) => value.encode(out),
            Self::Addrint(value) => out.write_u32::<LE>(*value),
            Self::AbortReason(value) => value.encode(out),
            Self::PutKind(value) => value.encode(out),
        }
    }
}
pub(crate) const MAX_OPERANDS: usize = 2usize;
impl RelocatableOffset {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(self.link)?;
        out.write_u32::<LE>(self.offset)?;
        Ok(())
    }
}
impl AbortReason {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(*self as u32)
    }
}
impl PutKind {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(*self as u32)
    }
}
impl GetKind {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(*self as u32)
    }
}
impl StdStream {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(*self as u32)
    }
}
impl StreamKind {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(*self as u32)
    }
}
impl CheckKind {
    #[doc = "Encodes the type into the equivalent byte representation."]
    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        out.write_u32::<LE>(*self as u32)
    }
}
impl InstructionEncoder {
    #[doc = "Encode a [**ABORT**](Opcode::ABORT) instruction.\n\n## Operands\n\n- abort_kind: What is the reason for aborting\n"]
    pub fn abort(&mut self, abort_kind: AbortReason) -> InstructionRef {
        self.add(Instruction::new(Opcode::ABORT).with_operand(Operand::AbortReason(abort_kind)))
    }
    #[doc = "Encode a [**ABORTCOND**](Opcode::ABORTCOND) instruction.\n\n## Operands\n\n- abort_kind: What is the reason for aborting\n"]
    pub fn abortcond(&mut self, abort_kind: AbortReason) -> InstructionRef {
        self.add(Instruction::new(Opcode::ABORTCOND).with_operand(Operand::AbortReason(abort_kind)))
    }
    #[doc = "Encode a [**ADDINT**](Opcode::ADDINT) instruction."]
    pub fn addint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::ADDINT)) }
    #[doc = "Encode a [**ADDINTNAT**](Opcode::ADDINTNAT) instruction."]
    pub fn addintnat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::ADDINTNAT)) }
    #[doc = "Encode a [**ADDNAT**](Opcode::ADDNAT) instruction."]
    pub fn addnat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::ADDNAT)) }
    #[doc = "Encode a [**ADDNATINT**](Opcode::ADDNATINT) instruction."]
    pub fn addnatint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::ADDNATINT)) }
    #[doc = "Encode a [**ADDREAL**](Opcode::ADDREAL) instruction."]
    pub fn addreal(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::ADDREAL)) }
    #[doc = "Encode a [**CALL**](Opcode::CALL) instruction.\n\n## Operands\n\n- address_offset: Offset to the call address in the stack.\n"]
    pub fn call(&mut self, address_offset: Offset) -> InstructionRef {
        self.add(Instruction::new(Opcode::CALL).with_operand(Operand::Offset(address_offset)))
    }
    #[doc = "Encode a [**CASE**](Opcode::CASE) instruction.\n\n## Operands\n\n- descriptor: Offset to the case descriptor table describing the jump targets\n"]
    pub fn case(&mut self, descriptor: Offset) -> InstructionRef {
        self.add(Instruction::new(Opcode::CASE).with_operand(Operand::Offset(descriptor)))
    }
    #[doc = "Encode a [**EQADDR**](Opcode::EQADDR) instruction."]
    pub fn eqaddr(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::EQADDR)) }
    #[doc = "Encode a [**EQCHARN**](Opcode::EQCHARN) instruction.\n\n## Operands\n\n- length: Byte length of the `char(N)` type.\n"]
    pub fn eqcharn(&mut self, length: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::EQCHARN).with_operand(Operand::Nat4(length)))
    }
    #[doc = "Encode a [**EQINT**](Opcode::EQINT) instruction."]
    pub fn eqint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::EQINT)) }
    #[doc = "Encode a [**EQINTNAT**](Opcode::EQINTNAT) instruction."]
    pub fn eqintnat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::EQINTNAT)) }
    #[doc = "Encode a [**EQNAT**](Opcode::EQNAT) instruction."]
    pub fn eqnat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::EQNAT)) }
    #[doc = "Encode a [**EQREAL**](Opcode::EQREAL) instruction."]
    pub fn eqreal(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::EQREAL)) }
    #[doc = "Encode a [**EQSET**](Opcode::EQSET) instruction.\n\n## Operands\n\n- set_length: Length of the set, in bytes.\n"]
    pub fn eqset(&mut self, set_length: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::EQSET).with_operand(Operand::Nat4(set_length)))
    }
    #[doc = "Encode a [**EQSTR**](Opcode::EQSTR) instruction."]
    pub fn eqstr(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::EQSTR)) }
    #[doc = "Encode a [**GECHARN**](Opcode::GECHARN) instruction.\n\n## Operands\n\n- length: Byte length of the `char(N)` type.\n"]
    pub fn gecharn(&mut self, length: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::GECHARN).with_operand(Operand::Nat4(length)))
    }
    #[doc = "Encode a [**GECLASS**](Opcode::GECLASS) instruction."]
    pub fn geclass(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GECLASS)) }
    #[doc = "Encode a [**GEINT**](Opcode::GEINT) instruction."]
    pub fn geint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GEINT)) }
    #[doc = "Encode a [**GEINTNAT**](Opcode::GEINTNAT) instruction."]
    pub fn geintnat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GEINTNAT)) }
    #[doc = "Encode a [**GENAT**](Opcode::GENAT) instruction."]
    pub fn genat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GENAT)) }
    #[doc = "Encode a [**GENATINT**](Opcode::GENATINT) instruction."]
    pub fn genatint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GENATINT)) }
    #[doc = "Encode a [**GEREAL**](Opcode::GEREAL) instruction."]
    pub fn gereal(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GEREAL)) }
    #[doc = "Encode a [**GESET**](Opcode::GESET) instruction.\n\n## Operands\n\n- set_length: Length of the set, in bytes.\n"]
    pub fn geset(&mut self, set_length: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::GESET).with_operand(Operand::Nat4(set_length)))
    }
    #[doc = "Encode a [**GESTR**](Opcode::GESTR) instruction."]
    pub fn gestr(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GESTR)) }
    #[doc = "Encode a [**GTCLASS**](Opcode::GTCLASS) instruction."]
    pub fn gtclass(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::GTCLASS)) }
    #[doc = "Encode a [**LECHARN**](Opcode::LECHARN) instruction.\n\n## Operands\n\n- length: Byte length of the `char(N)` type.\n"]
    pub fn lecharn(&mut self, length: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::LECHARN).with_operand(Operand::Nat4(length)))
    }
    #[doc = "Encode a [**LECLASS**](Opcode::LECLASS) instruction."]
    pub fn leclass(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LECLASS)) }
    #[doc = "Encode a [**LEINT**](Opcode::LEINT) instruction."]
    pub fn leint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LEINT)) }
    #[doc = "Encode a [**LEINTNAT**](Opcode::LEINTNAT) instruction."]
    pub fn leintnat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LEINTNAT)) }
    #[doc = "Encode a [**LENAT**](Opcode::LENAT) instruction."]
    pub fn lenat(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LENAT)) }
    #[doc = "Encode a [**LENATINT**](Opcode::LENATINT) instruction."]
    pub fn lenatint(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LENATINT)) }
    #[doc = "Encode a [**LEREAL**](Opcode::LEREAL) instruction."]
    pub fn lereal(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LEREAL)) }
    #[doc = "Encode a [**LESET**](Opcode::LESET) instruction.\n\n## Operands\n\n- set_length: Length of the set, in bytes.\n"]
    pub fn leset(&mut self, set_length: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::LESET).with_operand(Operand::Nat4(set_length)))
    }
    #[doc = "Encode a [**LESTR**](Opcode::LESTR) instruction."]
    pub fn lestr(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LESTR)) }
    #[doc = "Encode a [**JUMP**](Opcode::JUMP) instruction.\n\n## Operands\n\n- offset: Offset to jump forward by, in bytes.\n"]
    pub fn jump(&mut self, offset: Offset) -> InstructionRef {
        self.add(Instruction::new(Opcode::JUMP).with_operand(Operand::Offset(offset)))
    }
    #[doc = "Encode a [**JUMPB**](Opcode::JUMPB) instruction.\n\n## Operands\n\n- offset: Offset to jump backwards by, in bytes.\n"]
    pub fn jumpb(&mut self, offset: Offset) -> InstructionRef {
        self.add(Instruction::new(Opcode::JUMPB).with_operand(Operand::Offset(offset)))
    }
    #[doc = "Encode a [**LOCATELOCAL**](Opcode::LOCATELOCAL) instruction.\n\n## Operands\n\n- offset: Offset in the locals area.\n"]
    pub fn locatelocal(&mut self, offset: Offset) -> InstructionRef {
        self.add(Instruction::new(Opcode::LOCATELOCAL).with_operand(Operand::Offset(offset)))
    }
    #[doc = "Encode a [**LOCATETEMP**](Opcode::LOCATETEMP) instruction.\n\n## Operands\n\n- frame_size: Size of the call frame.\n- offset: Offset in the temporary area.\n"]
    pub fn locatetemp(&mut self, frame_size: Nat4, offset: Offset) -> InstructionRef {
        self.add(
            Instruction::new(Opcode::LOCATETEMP)
                .with_operand(Operand::Nat4(frame_size))
                .with_operand(Operand::Offset(offset)),
        )
    }
    #[doc = "Encode a [**LTCLASS**](Opcode::LTCLASS) instruction."]
    pub fn ltclass(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::LTCLASS)) }
    #[doc = "Encode a [**PROC**](Opcode::PROC) instruction.\n\n## Operands\n\n- frame_size: Size of the call frame to allocate for locals and temporaries.\n"]
    pub fn proc(&mut self, frame_size: Nat4) -> InstructionRef {
        self.add(Instruction::new(Opcode::PROC).with_operand(Operand::Nat4(frame_size)))
    }
    #[doc = "Encode a [**PUSHADDR**](Opcode::PUSHADDR) instruction.\n\n## Operands\n\n- addr: Absolute address to refer to.\n"]
    pub fn pushaddr(&mut self, addr: Addrint) -> InstructionRef {
        self.add(Instruction::new(Opcode::PUSHADDR).with_operand(Operand::Addrint(addr)))
    }
    #[doc = "Encode a [**PUSHADDR1**](Opcode::PUSHADDR1) instruction.\n\n## Operands\n\n- offset: Runtime-resolved offset within a section.\n"]
    pub fn pushaddr1(&mut self, offset: RelocatableOffset) -> InstructionRef {
        self.add(
            Instruction::new(Opcode::PUSHADDR1).with_operand(Operand::RelocatableOffset(offset)),
        )
    }
    #[doc = "Encode a [**PUSHCOPY**](Opcode::PUSHCOPY) instruction."]
    pub fn pushcopy(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::PUSHCOPY)) }
    #[doc = "Encode a [**PUSHINT**](Opcode::PUSHINT) instruction.\n\n## Operands\n\n- literal: Literal value to push.\n"]
    pub fn pushint(&mut self, literal: Int4) -> InstructionRef {
        self.add(Instruction::new(Opcode::PUSHINT).with_operand(Operand::Int4(literal)))
    }
    #[doc = "Encode a [**PUSHINT1**](Opcode::PUSHINT1) instruction.\n\n## Operands\n\n- literal: Literal value to push.\n"]
    pub fn pushint1(&mut self, literal: Int1) -> InstructionRef {
        self.add(Instruction::new(Opcode::PUSHINT1).with_operand(Operand::Int1(literal)))
    }
    #[doc = "Encode a [**PUSHINT2**](Opcode::PUSHINT2) instruction.\n\n## Operands\n\n- literal: Literal value to push.\n"]
    pub fn pushint2(&mut self, literal: Int2) -> InstructionRef {
        self.add(Instruction::new(Opcode::PUSHINT2).with_operand(Operand::Int2(literal)))
    }
    #[doc = "Encode a [**PUSHREAL**](Opcode::PUSHREAL) instruction.\n\n## Operands\n\n- literal: Literal value to push.\n"]
    pub fn pushreal(&mut self, literal: Real8) -> InstructionRef {
        self.add(Instruction::new(Opcode::PUSHREAL).with_operand(Operand::Real8(literal)))
    }
    #[doc = "Encode a [**PUSHVAL0**](Opcode::PUSHVAL0) instruction."]
    pub fn pushval0(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::PUSHVAL0)) }
    #[doc = "Encode a [**PUSHVAL1**](Opcode::PUSHVAL1) instruction."]
    pub fn pushval1(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::PUSHVAL1)) }
    #[doc = "Encode a [**PUT**](Opcode::PUT) instruction.\n\n## Operands\n\n- put_kind: Which kind of item to put.\n"]
    pub fn put(&mut self, put_kind: PutKind) -> InstructionRef {
        self.add(Instruction::new(Opcode::PUT).with_operand(Operand::PutKind(put_kind)))
    }
    #[doc = "Encode a [**RETURN**](Opcode::RETURN) instruction."]
    pub fn return_(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::RETURN)) }
    #[doc = "Encode a [**INCLINENO**](Opcode::INCLINENO) instruction."]
    pub fn inclineno(&mut self) -> InstructionRef { self.add(Instruction::new(Opcode::INCLINENO)) }
    #[doc = "Encode a [**SETFILENO**](Opcode::SETFILENO) instruction.\n\n## Operands\n\n- file_no: File number to set location to.\n- line_no: Line number to set location to.\n"]
    pub fn setfileno(&mut self, file_no: Nat2, line_no: Nat2) -> InstructionRef {
        self.add(
            Instruction::new(Opcode::SETFILENO)
                .with_operand(Operand::Nat2(file_no))
                .with_operand(Operand::Nat2(line_no)),
        )
    }
    #[doc = "Encode a [**SETLINENO**](Opcode::SETLINENO) instruction.\n\n## Operands\n\n- line_no: Line number to set location to.\n"]
    pub fn setlineno(&mut self, line_no: Nat2) -> InstructionRef {
        self.add(Instruction::new(Opcode::SETLINENO).with_operand(Operand::Nat2(line_no)))
    }
}
