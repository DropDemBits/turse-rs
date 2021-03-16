//! Crate containing all of the HIR node representations
//!
//! Note: All `expr` & `stmt` nodes are to be used with the module's prefix, e.g.
//! `expr::Name` instead of importing the node directly

use std::collections::HashMap;
use std::ops;

use la_arena::{Arena, Idx};
use toc_reporting::TextRange;

use crate::expr::ExprIdx;
use crate::stmt::StmtIdx;
use crate::symbol::SymbolTable;
use crate::ty::TypeIdx;
pub mod expr;
pub mod stmt;
pub mod symbol;
pub mod ty;

/// Code Unit
#[derive(Debug)]
pub struct Unit {
    /// Top level statements in the unit
    pub stmts: Vec<stmt::StmtIdx>,
    /// Unit-local symbol table
    pub symbol_table: SymbolTable,
}

/// Aggregate HIR structure
#[derive(Debug)]
pub struct Database {
    pub stmt_nodes: SpannedArena<stmt::Stmt>,
    pub expr_nodes: SpannedArena<expr::Expr>,
    pub type_nodes: SpannedArena<ty::Type>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            stmt_nodes: SpannedArena::new(),
            expr_nodes: SpannedArena::new(),
            type_nodes: SpannedArena::new(),
        }
    }
}

impl ops::Index<StmtIdx> for Database {
    type Output = stmt::Stmt;

    fn index(&self, index: StmtIdx) -> &Self::Output {
        &self.stmt_nodes.arena[index]
    }
}

impl ops::Index<ExprIdx> for Database {
    type Output = expr::Expr;

    fn index(&self, index: ExprIdx) -> &Self::Output {
        &self.expr_nodes.arena[index]
    }
}

impl ops::Index<TypeIdx> for Database {
    type Output = ty::Type;

    fn index(&self, index: TypeIdx) -> &Self::Output {
        &self.type_nodes.arena[index]
    }
}

#[derive(Debug)]
pub struct SpannedArena<T> {
    pub arena: Arena<T>,
    pub spans: HashMap<Idx<T>, TextRange>,
}

impl<T> SpannedArena<T> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            spans: HashMap::new(),
        }
    }

    pub fn alloc_spanned(&mut self, value: T, span: TextRange) -> Idx<T> {
        let idx = self.arena.alloc(value);
        self.spans.insert(idx, span);
        idx
    }

    pub fn alloc(&mut self, value: T) -> Idx<T> {
        self.arena.alloc(value)
    }
}
