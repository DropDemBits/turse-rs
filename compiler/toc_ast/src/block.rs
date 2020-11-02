use crate::ast::stmt::Stmt;
use crate::ast::{Visitor, VisitorMut};
use crate::scope::UnitScope;
use crate::types::TypeTable;

#[derive(Debug, Copy, Clone)]
#[allow(unused_variables, dead_code)]
pub enum BlockKind {
    /// Main block, root for all blocks, can be an execution block
    Main,
    /// Unit block, cannot be an execution block
    Unit,
    /// Inner block (begin ... end) inside of another block, or general statement block
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

#[derive(Debug)]
pub struct CodeUnit {
    /// Root statements
    stmts: Vec<Stmt>,
    /// Unit scope
    /// May be moved into and outside of the unit for mutability purposes
    unit_scope: Option<UnitScope>,
    /// Type table associated with the unit.
    /// May be moved into and outside of the unit for mutability purposes
    types: Option<TypeTable>,
}

impl CodeUnit {
    pub fn new(
        _is_main: bool,
        stmts: Vec<Stmt>,
        unit_scope: UnitScope,
        type_table: TypeTable,
    ) -> Self {
        Self {
            stmts,
            unit_scope: Some(unit_scope),
            types: Some(type_table),
        }
    }

    /// Visits the AST using the given VisitorMut, providing mutable access
    pub fn visit_ast_mut<T, St, Ex>(&mut self, visitor: &mut T)
    where
        T: VisitorMut<St, Ex>,
    {
        visitor.start_visit();

        for stmt in self.stmts.iter_mut() {
            visitor.visit_stmt(stmt);
        }

        visitor.end_visit();
    }

    /// Visits the AST using the given Visitor, only providing immutable access
    pub fn visit_ast<T, St, Ex>(&self, visitor: &mut T)
    where
        T: Visitor<St, Ex>,
    {
        visitor.start_visit();

        for stmt in self.stmts.iter() {
            visitor.visit_stmt(stmt);
        }

        visitor.end_visit();
    }

    #[allow(dead_code)] // Used only by the tests right now
    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }

    pub fn unit_scope(&self) -> &UnitScope {
        self.unit_scope.as_ref().unwrap()
    }

    pub fn types(&self) -> &TypeTable {
        self.types.as_ref().unwrap()
    }

    pub fn stmts_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.stmts
    }

    pub fn unit_scope_mut(&mut self) -> &mut UnitScope {
        self.unit_scope.as_mut().unwrap()
    }

    pub fn types_mut(&mut self) -> &mut TypeTable {
        self.types.as_mut().unwrap()
    }

    // TODO: Revisit these when dealing with multiple files, as requirements will change

    pub fn take_types(&mut self) -> TypeTable {
        self.types.take().unwrap()
    }

    pub fn put_types(&mut self, table: TypeTable) {
        self.types.replace(table);
    }

    pub fn take_unit_scope(&mut self) -> UnitScope {
        self.unit_scope.take().unwrap()
    }

    pub fn put_unit_scope(&mut self, unit_scope: UnitScope) {
        self.unit_scope.replace(unit_scope);
    }
}
