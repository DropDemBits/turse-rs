//! Crate containing all of the HIR node representations
//!
//! Note: All `expr` & `stmt` nodes are to be used with the module's prefix, e.g.
//! `expr::Name` instead of importing the node directly

use std::collections::HashMap;

use la_arena::{Arena, Idx};
use toc_reporting::TextRange;
pub mod expr;
pub mod stmt;
pub mod ty;

/// Code Unit
#[derive(Debug)]
pub struct Unit {
    pub stmts: Vec<stmt::StmtIdx>,
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
