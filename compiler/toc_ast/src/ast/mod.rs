//! AST structure definitions

pub mod expr;
pub mod ident;
pub mod stmt;
pub mod types;

/// Mutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait VisitorMut<St, Ex> {
    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &mut stmt::Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut expr::Expr) -> Ex;
}

/// Immutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait Visitor<St, Ex> {
    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &stmt::Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &expr::Expr) -> Ex;
}
