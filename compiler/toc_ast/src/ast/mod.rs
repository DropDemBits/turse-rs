//! AST structure definitions

pub mod expr;
pub mod ident;
pub mod stmt;
pub mod types;

/// Mutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements,
/// `Ex` is the type returned from visiting expressions, and
/// `Ty` is the type returned from visiting types.
pub trait VisitorMut<St, Ex, Ty> {
    /// Visit a statement in the tree
    fn visit_stmt(&mut self, stmt: &mut stmt::Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut expr::Expr) -> Ex;

    /// Visit a type in the tree
    fn visit_type(&mut self, ty: &mut types::Type) -> Ty;
}

/// Immutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements,
/// `Ex` is the type returned from visiting expressions, and
/// `Ty` is the type returned from visiting types.
pub trait Visitor<St, Ex, Ty> {
    /// Visit a statement in the tree
    fn visit_stmt(&mut self, stmt: &stmt::Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &expr::Expr) -> Ex;

    /// Visit a type in the tree
    fn visit_type(&mut self, ty: &types::Type) -> Ty;
}
