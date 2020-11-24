use crate::ast::stmt::Stmt;
use crate::scope::UnitScope;
use crate::types::TypeTable;

/// Unit id
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct UnitId(u32);

// Only to be used in unit testing & unit creation
impl From<u32> for UnitId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

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
