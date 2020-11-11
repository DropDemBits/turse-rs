use crate::ast::stmt::Stmt;
use crate::scope::UnitScope;
use crate::types::TypeTable;

#[derive(Debug)]
pub struct CodeUnit {
    /// Root stmt
    pub root_stmt: Box<Stmt>,
    /// Unit scope
    pub unit_scope: UnitScope,
    /// Type table associated with the unit.
    pub type_table: TypeTable,
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
            unit_scope,
            type_table,
        }
    }
}
