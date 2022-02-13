//! Query implementations

use std::{cell::RefCell, sync::Arc};

use toc_hir::{
    body,
    body::{BodyOwner, BodyTable},
    expr, item,
    item::ParameterInfo,
    library::{InLibrary, Library, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    stmt,
    symbol::{
        BindingKind, DefId, DefOwner, DefTable, LocalDefId, Mutability, NotBinding, SymbolKind,
    },
    ty::{self, PassBy},
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
    let def_collector = DefCollector {
        def_table: Default::default(),
        library: &library,
    };
    Walker::from_library(&library).visit_preorder(&def_collector);

    Arc::new(def_collector.def_table.into_inner())
}

pub fn lookup_def_owner(db: &dyn HirDatabase, def_id: DefId) -> Option<DefOwner> {
    db.defs_of(def_id.0).get_owner(def_id.1)
}

pub fn collect_body_owners(db: &dyn HirDatabase, library_id: LibraryId) -> Arc<BodyTable> {
    use toc_hir::visitor::Walker;

    let library = db.library(library_id);

    // Collect definitions
    let body_collector = BodyCollector {
        body_table: Default::default(),
        _library: &library,
    };
    Walker::from_library(&library).visit_preorder(&body_collector);

    Arc::new(body_collector.body_table.into_inner())
}

pub fn lookup_body_owner(
    db: &dyn HirDatabase,
    body_id: InLibrary<body::BodyId>,
) -> Option<BodyOwner> {
    db.body_owners_of(body_id.0).get_owner(body_id.1)
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

pub(crate) fn binding_kind(
    db: &dyn HirDatabase,
    ref_src: BindingSource,
) -> Result<BindingKind, NotBinding> {
    let def_id = lookup_binding_def(db, ref_src)?;

    // Take the binding kind from the def owner
    let def_owner = db.def_owner(def_id);
    let library = db.library(def_id.0);

    match def_owner {
        Some(DefOwner::Item(item_id)) => Ok(match &library.item(item_id).kind {
            item::ItemKind::ConstVar(item) if item.is_register => {
                BindingKind::Register(item.mutability)
            }
            item::ItemKind::Binding(item) if item.is_register => {
                BindingKind::Register(item.mutability)
            }
            item::ItemKind::ConstVar(item) => BindingKind::Storage(item.mutability),
            item::ItemKind::Binding(item) => BindingKind::Storage(item.mutability),
            item::ItemKind::Subprogram(item) => BindingKind::Subprogram(item.kind),
            item::ItemKind::Type(_) => BindingKind::Type,
            item::ItemKind::Module(_) => BindingKind::Module,
        }),
        Some(DefOwner::ItemParam(item_id, param_def)) => {
            // Lookup the arg
            let item = match &library.item(item_id).kind {
                item::ItemKind::Subprogram(item) => item,
                _ => unreachable!(),
            };

            Ok(match item.lookup_param_info(param_def) {
                ParameterInfo::Param(param_info) => {
                    // This is the real parameters

                    // Pass-by value parameters are always const
                    // Register parameters become register bindings
                    match param_info.pass_by {
                        PassBy::Value if param_info.is_register => {
                            BindingKind::Register(Mutability::Const)
                        }
                        PassBy::Reference(mutability) if param_info.is_register => {
                            BindingKind::Register(mutability)
                        }
                        PassBy::Value => BindingKind::Storage(Mutability::Const),
                        PassBy::Reference(mutability) => BindingKind::Storage(mutability),
                    }
                }
                ParameterInfo::Result => {
                    // This is the result parameter
                    // Always const storage, only modifiable by `result` stmts
                    BindingKind::Storage(Mutability::Const)
                }
            })
        }
        Some(DefOwner::Stmt(stmt_id)) => {
            match &library.body(stmt_id.0).stmt(stmt_id.1).kind {
                stmt::StmtKind::Item(_) => {
                    unreachable!("item def owners shouldn't be stmt def owners")
                }
                // for-loop counter var is an immutable ref
                stmt::StmtKind::For(_) => Ok(BindingKind::Storage(Mutability::Const)),
                _ => Err(NotBinding::NotReference),
            }
        }
        // From an undeclared identifier, not purely a binding
        None => Err(NotBinding::Undeclared),
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
                body::BodyKind::Stmts(..) => Err(NotBinding::NotReference),
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
                _ => Err(NotBinding::NotReference),
            }
        }
    }
}

/// Library-local definition collector
struct DefCollector<'a> {
    def_table: RefCell<DefTable>,
    library: &'a Library,
}

impl DefCollector<'_> {
    fn add_owner(&self, def_id: LocalDefId, owner: DefOwner) {
        self.def_table.borrow_mut().add_owner(def_id, owner);
    }
}

impl HirVisitor for DefCollector<'_> {
    fn visit_item(&self, id: item::ItemId, item: &item::Item) {
        self.add_owner(item.def_id, DefOwner::Item(id));
    }

    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        if let Some(params) = &item.param_list {
            for name in &params.names {
                // Skip the filler args
                // They are placeholders, and can't be named anyways
                if !matches!(self.library.local_def(*name).kind, SymbolKind::Declared) {
                    continue;
                }

                self.add_owner(*name, DefOwner::ItemParam(id, *name));
            }
        }

        if let Some(name) = item.result.name {
            self.add_owner(name, DefOwner::ItemParam(id, name));
        }
    }

    fn visit_for(&self, id: stmt::BodyStmt, stmt: &stmt::For) {
        if let Some(def_id) = stmt.counter_def {
            self.add_owner(def_id, DefOwner::Stmt(id))
        }
    }
}

/// Library-local body collector
struct BodyCollector<'a> {
    body_table: RefCell<BodyTable>,
    _library: &'a Library,
}

impl BodyCollector<'_> {
    fn add_owner(&self, body_id: body::BodyId, owner: BodyOwner) {
        self.body_table.borrow_mut().add_owner(body_id, owner);
    }
}

impl HirVisitor for BodyCollector<'_> {
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        if let Some(init_expr) = item.init_expr {
            self.add_owner(init_expr, BodyOwner::Item(id));
        }
    }

    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        self.add_owner(item.body.body, BodyOwner::Item(id));

        match item.extra {
            item::SubprogramExtra::DeviceSpec(body) | item::SubprogramExtra::StackSize(body) => {
                self.add_owner(body, BodyOwner::Item(id));
            }
            _ => (),
        }
    }

    fn visit_module(&self, id: item::ItemId, item: &item::Module) {
        self.add_owner(item.body, BodyOwner::Item(id));
    }

    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {
        match ty {
            ty::Primitive::SizedChar(ty::SeqLength::Expr(body))
            | ty::Primitive::SizedString(ty::SeqLength::Expr(body)) => {
                self.add_owner(*body, BodyOwner::Type(id));
            }
            _ => (),
        }
    }
}
