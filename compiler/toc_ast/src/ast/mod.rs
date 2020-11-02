//! AST structure definitions

pub mod expr;
pub mod ident;
pub mod stmt;

/// Mutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait VisitorMut<St, Ex> {
    /// Starts a visit to the tree. Allows the visitor to set itself up
    fn start_visit(&mut self) {}

    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &mut stmt::Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut expr::Expr) -> Ex;

    /// Ends a visit to the tree. Allows the visitor to perform any cleanup
    fn end_visit(&mut self) {}
}

/// Immutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait Visitor<St, Ex> {
    /// Starts a visit to the tree. Allows the visitor to set itself up
    fn start_visit(&mut self) {}

    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &stmt::Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &expr::Expr) -> Ex;

    /// Ends a visit to the tree. Allows the visitor to perform any cleanup
    fn end_visit(&mut self) {}
}
