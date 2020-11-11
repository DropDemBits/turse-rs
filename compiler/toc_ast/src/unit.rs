use crate::ast::stmt::{self, Stmt, StmtKind};
use crate::ast::{Visitor, VisitorMut};
use crate::scope::UnitScope;
use crate::types::TypeTable;

#[derive(Debug)]
pub struct CodeUnit {
    /// Root stmt
    root_stmt: Box<Stmt>,
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
        root_stmt: Box<Stmt>,
        unit_scope: UnitScope,
        type_table: TypeTable,
    ) -> Self {
        Self {
            root_stmt,
            unit_scope: Some(unit_scope),
            types: Some(type_table),
        }
    }

    /// Visits the AST using the given `VisitorMut`, providing mutable access
    pub fn visit_ast_mut<T, St, Ex, Ty>(&mut self, visitor: &mut T)
    where
        T: VisitorMut<St, Ex, Ty>,
    {
        visitor.visit_stmt(&mut self.root_stmt);
    }

    /// Visits the AST using the given Visitor, only providing immutable access
    pub fn visit_ast<T, St, Ex, Ty>(&self, visitor: &mut T)
    where
        T: Visitor<St, Ex, Ty>,
    {
        visitor.visit_stmt(&self.root_stmt);
    }

    #[allow(dead_code)] // Used only by the tests, visit_ast should be used
    pub fn stmts(&self) -> &Vec<Stmt> {
        if let StmtKind::Block { block } = &self.root_stmt.kind {
            &block.stmts
        } else {
            unreachable!("not a StmtKind::Block!!!")
        }
    }

    pub fn root_block(&self) -> &stmt::Block {
        if let StmtKind::Block { block } = &self.root_stmt.kind {
            &block
        } else {
            unreachable!("not a StmtKind::Block!!!")
        }
    }

    pub fn root_stmt(&self) -> &Stmt {
        &self.root_stmt
    }

    pub fn root_stmt_mut(&mut self) -> &mut Stmt {
        &mut self.root_stmt
    }

    pub fn unit_scope(&self) -> &UnitScope {
        self.unit_scope.as_ref().unwrap()
    }

    pub fn types(&self) -> &TypeTable {
        self.types.as_ref().unwrap()
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
