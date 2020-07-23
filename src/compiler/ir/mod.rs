//! IR Generation and manipulation
mod ir_builder;

pub use ir_builder::IrBuilder;

use crate::compiler::types::TypeRef;
use crate::compiler::value::Value;
use crate::compiler::{Location, Operator};
use petgraph::prelude::*;
use std::collections::HashMap;
use std::fmt;

/// Reference to an IR block
pub type BlockIndexType = u32;
pub type BlockIndex = NodeIndex<BlockIndexType>;

/// Variable reference spaces
#[derive(Debug, Copy, Clone)]
pub enum AddressSpace {
    /// Global, per-unit space
    Global,
    /// Local stack space
    Local,
}

/// Reference in an IR instruction to an identifier
#[derive(Clone)]
pub struct Reference {
    /// Name of the reference
    pub name: String,
    /// Generation of the reference.
    /// All references are only assigned to once
    pub generation: usize,
    /// Type of the reference
    pub type_ref: TypeRef,
    /// The address space the reference lives in
    pub address_space: AddressSpace,
}

impl fmt::Debug for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{}.{} [{:?}]",
            self.name, self.generation, self.type_ref
        ))
    }
}

/// A block of instructions.
/// After coalescing, the last instruction must be a branch.
#[derive(Debug)]
pub struct Block {
    /// Instructions in the block
    instrs: Vec<Instruction>,
}

impl Block {
    /// Creates a new IR block
    pub fn new() -> Self {
        Self { instrs: vec![] }
    }

    /// Inserts an instruction into the block, returning the instruction
    /// index.
    pub fn insert_instruction(&mut self, inst: Instruction) -> usize {
        self.instrs.push(inst);
        self.instrs.len() - 1
    }
}

/// Reference node in a block scope (a group of references)
#[derive(Debug)]
pub struct ReferenceNode {
    /// References used in the current reference scope
    references: HashMap<String, Reference>,
}

impl ReferenceNode {
    /// Creates a new reference scope
    pub fn new() -> Self {
        Self {
            references: HashMap::new(),
        }
    }

    /// Assigns a value to a given reference.
    /// Creates a new one if one doesn't exist.
    pub fn assign_ref(
        &mut self,
        name: &str,
        type_ref: &TypeRef,
        address_space: AddressSpace,
    ) -> Reference {
        self.references
            .entry(name.to_string())
            .and_modify(|existing| {
                // Advance the generation number
                existing.generation += 1;
            })
            .or_insert(Reference {
                name: name.to_string(),
                type_ref: type_ref.clone(),
                generation: 0,
                address_space,
            })
            .clone()
    }

    /// Uses a reference.
    pub fn use_ref(&self, name: &str) -> Reference {
        self.references
            .get(name)
            .map(|r| r.clone())
            .expect(&format!("Reference '{}' is missing an 'assign_ref'", name))
    }
}

/// A single IR instruction
pub struct Instruction {
    /// Source line number of the instruction
    line: u32,
    /// Source unit number of the instrucion
    unit: u32,
    /// The operation performed by the instruction
    op: InstructionOp,
}

impl Instruction {
    pub fn new(location: &Location, op: InstructionOp) -> Self {
        Self {
            line: location.line as u32,
            unit: 0, // Unit number isn't tracked right now
            op,
        }
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{:4} {:4} {:?}",
            self.unit, self.line, self.op
        ))
    }
}

/// Instruction Operations
pub enum InstructionOp {
    /// No operation performed
    Nop,
    /// Allocates space in the specified space. Stores the address in the
    /// given reference.
    Alloc {
        dest: Reference,
        address_space: AddressSpace,
        alloc_type: TypeRef,
    },
    /// Loads a constant value into `dest`
    LoadConst { dest: Reference, constant: Value },
    /// Loads a value from `src` into `dest`, from the specified address space
    Load {
        dest: Reference,
        src: Reference,
        address_space: AddressSpace,
    },
    /// Stores `src` into a location pointed to by `dest`
    Store { dest: Reference, src: Reference },
    /// Moves the value from `src` into `dest`
    Move { dest: Reference, src: Reference },
    /// Applies a binary op and stores the result into `dest`
    BinaryOp {
        dest: Reference,
        op: Operator,
        lhs: Reference,
        rhs: Reference,
    },
    /// Applies a unary op and stores the result into `dest`
    UnaryOp {
        dest: Reference,
        op: Operator,
        rhs: Reference,
    },
    /// Casts a reference from the source type into the destination type
    Cast { dest: Reference, src: Reference },
    /// Phi operation to merge SSA values
    Phi { info: PhiInfo },
    /// Unconditional branch
    Branch { to: BlockIndex },
    /// Conditional branch (branch if true)
    CondBranch {
        test: Reference,
        true_branch: BlockIndex,
        false_branch: BlockIndex,
    },
    /// Return from a function, with some value
    Return { result: Option<Reference> },
}

impl fmt::Debug for InstructionOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstructionOp::Nop => f.write_str("<nop>"),
            InstructionOp::Move { dest, src } => {
                f.write_fmt(format_args!("{:?} := {:?}", dest, src))
            }
            InstructionOp::Alloc {
                dest,
                alloc_type,
                address_space,
            } => f.write_fmt(format_args!(
                "{:?} := {:?} {:?}",
                dest, address_space, alloc_type
            )),
            InstructionOp::BinaryOp { dest, lhs, op, rhs } => {
                f.write_fmt(format_args!("{:?} := {:?} {:?} {:?}", dest, lhs, op, rhs))
            }
            InstructionOp::UnaryOp { dest, op, rhs } => {
                f.write_fmt(format_args!("{:?} := {:?} {:?}", dest, op, rhs))
            }
            InstructionOp::LoadConst { dest, constant } => {
                f.write_fmt(format_args!("{:?} := {:?}", dest, constant))
            }
            InstructionOp::Load {
                dest,
                address_space,
                src,
            } => f.write_fmt(format_args!(
                "{:?} := load({:?}, {:?})",
                dest, address_space, src
            )),
            InstructionOp::Store { dest, src } => {
                f.write_fmt(format_args!("store({:?}, {:?})", dest, src))
            }
            _ => todo!(),
        }
    }
}

/// Phi merging information
#[derive(Debug)]
pub struct PhiInfo {
    dest: Reference,
    branches: Vec<Reference>,
}
