//! Code unit stuff

use crate::symbol::SymbolTable;
use crate::visitor::HirVisitor;
use crate::{db, stmt};

crate::hir_id_wrapper!(UnitId);

/// Code Unit
#[derive(Debug)]
pub struct Unit {
    /// Id in the global `Database`
    pub id: UnitId,
    /// Top level statements in the unit
    pub stmts: Vec<stmt::StmtId>,
    /// Unit-local symbol table
    pub symbol_table: SymbolTable,
}

impl Unit {
    pub fn walk_nodes(&self, hir_db: db::HirDb, visitor: &dyn HirVisitor) {
        let walker = crate::visitor::Walker::new(hir_db, self, visitor);

        walker.walk_nodes();
    }
}
