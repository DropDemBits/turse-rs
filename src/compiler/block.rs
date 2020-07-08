use crate::compiler::ast::Stmt;
use crate::compiler::scope::Scope;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub enum BlockKind {
    /// Main block, root for all blocks, can be an execution block
    Main,
    /// Unit block, cannot be an execution block
    Unit,
    /// Inner block (begin ... end) inside of another block
    InnerBlock,
    /// Function block
    Function,
    /// Procedure block
    Procedure,
    /// Module block
    Module,
    /// Class block
    Class,
    /// Monitor block
    Monitor,
    /// Monitor-Class block, allows both class & monitor statments
    MonitorClass,
}

/// A block of statements
#[derive(Debug)]
pub struct CodeBlock {
    /// The current kind of block
    pub block_kind: BlockKind,
    /// The associated scope
    pub scope: Scope,
    /// Blocks that enclose the current ont
    pub enclosing_blocks: Vec<Weak<RefCell<Self>>>,
    /// Statements associated with this block
    pub stmts: Vec<Stmt>,
}

impl CodeBlock {
    pub fn new(block_kind: BlockKind, enclosing_blocks: &Vec<Rc<RefCell<Self>>>) -> Self {
        Self {
            block_kind,
            stmts: vec![],
            scope: Scope::new(enclosing_blocks),
            enclosing_blocks: enclosing_blocks
                .iter()
                .map(|block| Rc::downgrade(block))
                .collect(),
        }
    }
}
