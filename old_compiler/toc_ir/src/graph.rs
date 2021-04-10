//! All of the components in an IrGraph/IrModule
use crate::AddressSpace;
use petgraph::prelude::*;
use petgraph::stable_graph::StableDiGraph;
use std::collections::{HashMap, HashSet};
use std::fmt;
use toc_ast::ast::expr::{BinaryOp, UnaryOp};
use toc_ast::types::TypeRef;
use toc_ast::value::Value;
use toc_core::Location;

/// Reference to an IR block
pub type BlockIndexType = u32;
pub type BlockIndex = NodeIndex<BlockIndexType>;

/// Reference to another function
/// Only unique within an import boundary
pub type FuncRef = String;

#[derive(Debug)]
pub struct IrGraph {
    /// All blocks used in the IrGraph
    pub blocks: StableDiGraph<Block, (), BlockIndexType>,
    /// All functions defined in the IrGraph
    pub funcs: HashMap<String, Function>,
}

impl IrGraph {
    /// Creates a new IR graph
    pub fn new() -> Self {
        Self {
            blocks: StableDiGraph::new(),
            funcs: HashMap::new(),
        }
    }

    /// Creates a new block, returning the block index
    pub fn create_block(&mut self) -> BlockIndex {
        self.blocks.add_node(Block::new())
    }

    /// Creates a new function
    pub fn create_function(
        &mut self,
        name: &str,
        parent: Option<FuncRef>,
        type_def: Option<TypeRef>,
        entry_block: BlockIndex,
    ) {
        self.funcs.insert(
            name.to_string(),
            Function {
                parent,
                func_type: type_def,
                entry_block,
            },
        );
    }
}

impl Default for IrGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// A function definition
#[derive(Debug)]
pub struct Function {
    /// Reference to a parent function.
    /// Used when looking up References
    pub parent: Option<FuncRef>,
    /// Reference to the function's type definition.
    /// If None, the function is either the main function, the class constructor, or the module initializer
    pub func_type: Option<TypeRef>,
    /// Entry block of the function
    pub entry_block: BlockIndex,
}

/// A block of instructions.
/// After coalescing, the last instruction must be a branch.
#[derive(Debug)]
pub struct Block {
    /// Instructions in the block
    pub instrs: Vec<Instruction>,
    /// All references modified in the block.
    /// Only the identifier name is stored.
    pub modified_refs: HashSet<String>,
}

impl Block {
    /// Creates a new IR block
    pub fn new() -> Self {
        Self {
            instrs: vec![],
            modified_refs: HashSet::new(),
        }
    }

    /// Inserts an instruction into the block, returning the instruction
    /// index.
    pub fn insert_instruction(&mut self, inst: Instruction) -> usize {
        self.instrs.push(inst);
        self.instrs.len() - 1
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

/// A single IR instruction
pub struct Instruction {
    /// Source line number of the instruction
    pub line: u32,
    /// Source unit number of the instrucion
    pub unit: u32,
    /// The operation performed by the instruction
    pub inst_op: InstructionOp,
}

impl Instruction {
    pub fn new(location: &Location, inst_op: InstructionOp) -> Self {
        Self {
            line: location.line as u32,
            unit: 0, // Unit number isn't tracked right now
            inst_op,
        }
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{:4} {:4} {:?}",
            self.unit, self.line, self.inst_op
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
        op: BinaryOp,
        lhs: Reference,
        rhs: Reference,
    },
    /// Applies a unary op and stores the result into `dest`
    UnaryOp {
        dest: Reference,
        op: UnaryOp,
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
            InstructionOp::Branch { to } => f.write_fmt(format_args!("br block({:?})", to)),
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
