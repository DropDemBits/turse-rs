use crate::compiler::ast::{ASTVisitorMut, Stmt};
use crate::compiler::scope::Scope;
use crate::compiler::types::TypeTable;
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

#[derive(Debug)]
pub struct CodeUnit {
    //blocks: Vec<Rc<RefCell<CodeBlock>>>,
    /// Root block of the unit
    root_block: Rc<RefCell<CodeBlock>>,
    /// Root statements
    stmts: Vec<Stmt>,
    /// Type table associated with the unit.
    /// May be moved into and outside of the unit for mutability purposes
    types: Option<TypeTable>,
}

impl CodeUnit {
    pub fn new(is_main: bool) -> Self {
        Self {
            root_block: Rc::new(RefCell::new(CodeBlock::new(
                if is_main {
                    BlockKind::Main
                } else {
                    BlockKind::Unit
                },
                &vec![],
            ))),
            stmts: vec![],
            types: Some(TypeTable::new()),
        }
    }

    /// Visits the AST using the given ASTVisitorMut, providing mutable access
    pub fn visit_ast_mut<T, U, V>(&mut self, visitor: &mut T)
    where
        T: ASTVisitorMut<U, V>,
    {
        for stmt in self.stmts.iter_mut() {
            visitor.visit_stmt(stmt);
        }
    }

    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }

    pub fn stmts_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.stmts
    }

    pub fn root_block(&self) -> &Rc<RefCell<CodeBlock>> {
        &self.root_block
    }

    pub fn root_block_mut(&mut self) -> &mut Rc<RefCell<CodeBlock>> {
        &mut self.root_block
    }

    // TODO: Revisit these when dealing with multiple files, as requirements will change

    pub fn take_types(&mut self) -> TypeTable {
        self.types.take().unwrap()
    }

    pub fn put_types(&mut self, table: TypeTable) {
        self.types.replace(table);
    }

    pub fn types_mut(&mut self) -> &mut TypeTable {
        self.types.as_mut().unwrap()
    }

    pub fn types(&self) -> &TypeTable {
        self.types.as_ref().unwrap()
    }
}
