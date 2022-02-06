//! Query implementations

use std::cell::RefCell;
use std::sync::Arc;

use toc_hir::expr;
use toc_hir::{
    body, item,
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    stmt,
    symbol::{BindingKind, DefId, DefOwner, DefTable, LocalDefId, Mutability},
    visitor::HirVisitor,
};

use crate::db::{BindingSource, HirDatabase};

pub fn library_query(db: &dyn HirDatabase, library: LibraryId) -> LoweredLibrary {
    let file = db.library_graph().file_of(library);
    db.lower_library(file).result().clone()
}

pub fn library_graph_query(db: &dyn HirDatabase) -> LibraryGraph {
    db.lower_library_graph().result().clone()
}

pub fn collect_defs(db: &dyn HirDatabase, library_id: LibraryId) -> Arc<DefTable> {
    use toc_hir::visitor::Walker;

    let library = db.library(library_id);

    // Collect definitions
    let def_collector = DefCollector::default();
    Walker::from_library(&library).visit_preorder(&def_collector);

    Arc::new(def_collector.def_table.into_inner())
}

pub fn lookup_def_owner(db: &dyn HirDatabase, def_id: DefId) -> Option<DefOwner> {
    db.defs_of(def_id.0).get_owner(def_id.1)
}

pub fn lookup_item(db: &dyn HirDatabase, def_id: DefId) -> Option<InLibrary<item::ItemId>> {
    db.def_owner(def_id).and_then(|owner| match owner {
        DefOwner::Item(id) => Some(InLibrary(def_id.0, id)),
        _ => None,
    })
}

pub fn lookup_bodies(db: &dyn HirDatabase, library: LibraryId) -> Arc<Vec<body::BodyId>> {
    let library = db.library(library);
    Arc::new(library.body_ids())
}

pub(crate) fn binding_to(db: &dyn HirDatabase, ref_src: BindingSource) -> Option<DefId> {
    lookup_binding_def(db, ref_src).ok()
}

pub(crate) fn binding_kind(db: &dyn HirDatabase, ref_src: BindingSource) -> Option<BindingKind> {
    let def_id = match lookup_binding_def(db, ref_src) {
        Ok(def_id) => def_id,
        // Have missing exprs fall back to the undeclared path
        // Undeclared bindings are already a fall back, so we have missing exprs piggy-back on this
        Err(NotBinding::Missing) => return Some(BindingKind::Undeclared),
        Err(NotBinding::NotRef) => return None,
    };

    // Take the binding kind from the def owner
    let def_owner = db.def_owner(def_id);
    let library = db.library(def_id.0);

    match def_owner {
        Some(DefOwner::Item(item_id)) => Some(match &library.item(item_id).kind {
            item::ItemKind::ConstVar(item) if item.is_register => {
                BindingKind::Register(item.mutability)
            }
            item::ItemKind::Binding(item) if item.is_register => {
                BindingKind::Register(item.mutability)
            }
            item::ItemKind::ConstVar(item) => BindingKind::Storage(item.mutability),
            item::ItemKind::Binding(item) => BindingKind::Storage(item.mutability),
            item::ItemKind::Type(_) => (BindingKind::Type),
            item::ItemKind::Module(_) => (BindingKind::Module),
        }),
        Some(DefOwner::Stmt(stmt_id)) => {
            match &library.body(stmt_id.0).stmt(stmt_id.1).kind {
                stmt::StmtKind::Item(_) => {
                    unreachable!("item def owners shouldn't be stmt def owners")
                }
                // for-loop counter var is an immutable ref
                stmt::StmtKind::For(_) => Some(BindingKind::Storage(Mutability::Const)),
                _ => None,
            }
        }
        // From an undeclared identifier, technically produces a binding
        None => Some(BindingKind::Undeclared),
    }
}

fn lookup_binding_def(db: &dyn HirDatabase, ref_src: BindingSource) -> Result<DefId, NotBinding> {
    match ref_src {
        // Trivial, def bindings are bindings to themselves
        // ???: Do we want to perform canonicalization / symbol resolution here?
        BindingSource::DefId(it) => Ok(it),
        BindingSource::Body(lib_id, body) => {
            let library = db.library(lib_id);

            match &library.body(body).kind {
                // Stmt bodies never produce bindings
                body::BodyKind::Stmts(..) => Err(NotBinding::NotRef),
                // Defer to expr form
                body::BodyKind::Exprs(expr) => lookup_binding_def(db, (lib_id, body, *expr).into()),
            }
        }
        BindingSource::BodyExpr(lib_id, expr) => {
            // Traverse nodes until we encounter a valid binding
            let library = db.library(lib_id);

            // For now, only name exprs can produce a binding
            match &library.body(expr.0).expr(expr.1).kind {
                expr::ExprKind::Missing => Err(NotBinding::Missing),
                expr::ExprKind::Name(name) => match name {
                    expr::Name::Name(def_id) => Ok(DefId(lib_id, *def_id)),
                    expr::Name::Self_ => todo!(),
                },
                _ => Err(NotBinding::NotRef),
            }
        }
    }
}

#[derive(Clone, Copy)]
enum NotBinding {
    Missing,
    NotRef,
}

#[derive(Default)]
struct DefCollector {
    def_table: RefCell<DefTable>,
}

impl DefCollector {
    fn add_owner(&self, def_id: LocalDefId, owner: DefOwner) {
        self.def_table.borrow_mut().add_owner(def_id, owner);
    }
}

impl HirVisitor for DefCollector {
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        self.add_owner(item.def_id, DefOwner::Item(id));
    }

    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {
        self.add_owner(item.def_id, DefOwner::Item(id));
    }

    fn visit_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        self.add_owner(item.def_id, DefOwner::Item(id));
    }

    fn visit_for(&self, id: stmt::BodyStmt, stmt: &stmt::For) {
        if let Some(def_id) = stmt.counter_def {
            self.add_owner(def_id, DefOwner::Stmt(id))
        }
    }
}
