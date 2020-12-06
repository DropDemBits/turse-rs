use std::num::NonZeroU32;

use crate::ast::stmt::Stmt;
use crate::scope::UnitScope;
use crate::types::TypeTable;

/// Unit id
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct UnitId(NonZeroU32);

// Only to be used in unit testing & unit creation
impl UnitId {
    pub fn new(id: u32) -> Self {
        Self(NonZeroU32::new(id).expect("passing in id 0"))
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
