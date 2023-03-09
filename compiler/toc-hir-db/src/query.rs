//! Query implementations

use std::{cell::RefCell, sync::Arc};

use toc_hir::{
    body::{self, BodyOwner, BodyTable},
    expr,
    item::{self, ModuleId, ModuleTree},
    library::{InLibrary, Library, LoweredLibrary},
    library_graph::SourceLibrary,
    stmt,
    symbol::{DefId, DefOwner, DefResolve, DefTable, LocalDefId, SymbolKind},
    ty::{self, TypeOwner, TypeOwners},
    visitor::HirVisitor,
};

use crate::{db::InsideModule, Db};

/// Get the library associated with the given id
#[salsa::tracked]
pub fn hir_library(db: &dyn Db, library: SourceLibrary) -> LoweredLibrary {
    toc_hir_lowering::lower_library(db.up(), library)
        .result()
        .clone()
}

/// Gets all of the definitions in the library,
/// providing a mapping between definitions and definition owners.
#[salsa::tracked]
pub fn defs_of(db: &dyn Db, library: SourceLibrary) -> Arc<DefTable> {
    use toc_hir::visitor::Walker;

    let library = hir_library(db, library);

    // Collect definitions
    let def_collector = DefCollector {
        def_table: Default::default(),
        library: &library,
    };
    Walker::from_library(&library).visit_preorder(&def_collector);

    Arc::new(def_collector.def_table.into_inner())
}

/// Gets the corresponding [`DefOwner`] to the given [`DefId`]
///
/// This does not perform any form of definition resolution.
pub fn def_owner(db: &dyn Db, def_id: DefId) -> Option<DefOwner> {
    db.defs_of(def_id.0).get_owner(def_id.1)
}

/// Gets all of the body owners in the library,
/// providing a mapping between bodies and body owners
#[salsa::tracked]
pub fn body_owners_of(db: &dyn Db, library: SourceLibrary) -> Arc<BodyTable> {
    use toc_hir::visitor::Walker;

    let library = hir_library(db, library);

    // Collect bodies
    let body_collector = BodyCollector {
        body_table: Default::default(),
        _library: &library,
    };
    Walker::from_library(&library).visit_preorder(&body_collector);

    Arc::new(body_collector.body_table.into_inner())
}

// FIXME: Make body_owner lookup infallible
/// Gets the corresponding [`BodyOwner`](body::BodyOwner) to the given [`BodyId`](body::BodyId)
pub fn body_owner(db: &dyn Db, body: InLibrary<body::BodyId>) -> Option<BodyOwner> {
    db.body_owners_of(body.0).get_owner(body.1)
}

/// Gets all of the type owners in the library,
/// providing a link between types and type owners.
#[salsa::tracked]
pub fn type_owners_of(db: &dyn Db, library: SourceLibrary) -> Arc<TypeOwners> {
    use toc_hir::visitor::Walker;

    let library = hir_library(db, library);

    // Collect types
    let type_collector = TypeCollector {
        type_table: Default::default(),
    };
    Walker::from_library(&library).visit_preorder(&type_collector);

    Arc::new(type_collector.type_table.into_inner())
}

/// Gets the corresponding [`TypeOwner`](ty::TypeOwner) for the given [`TypeId`](ty::TypeId)
pub fn type_owner(db: &dyn Db, ty: InLibrary<ty::TypeId>) -> TypeOwner {
    db.type_owners_of(ty.0).lookup_owner(ty.1)
}

/// Gets the module tree from the library,
/// providing a link from child modules to parent modules.
#[salsa::tracked]
pub fn module_tree_of(db: &dyn Db, library: SourceLibrary) -> Arc<ModuleTree> {
    use toc_hir::visitor::{WalkEvent, WalkNode, Walker};

    let library = hir_library(db, library);

    // Need an in-line collection since we're also concerned about exit events
    let mut walker = Walker::from_library(&library);
    let mut module_tree = ModuleTree::default();
    let mut module_path = vec![];

    while let Some(event) = walker.next_event() {
        match event {
            WalkEvent::Enter(WalkNode::Item(item_id, item)) => {
                if let item::ItemKind::Module(_) = &item.kind {
                    let child_mod = ModuleId::new(&library, item_id);

                    // Link to parent
                    if let Some(&parent_mod) = module_path.last() {
                        module_tree.link_modules(parent_mod, child_mod);
                    }

                    // Add to path
                    module_path.push(child_mod);
                } else {
                    // Attach items to the closest module
                    let module_id = *module_path.last().expect("only modules can be root items");
                    module_tree.link_declared_item(module_id, item_id);
                }
            }
            WalkEvent::Leave(WalkNode::Item(item_id, item)) => {
                if let item::ItemKind::Module(_) = &item.kind {
                    // Pop from path
                    let child_mod = module_path.pop();

                    // Stack discipline should mean that we pop off the correct module
                    assert_eq!(child_mod.map(|id| id.item_id()), Some(item_id));
                }
            }
            _ => {}
        }
    }

    Arc::new(module_tree)
}

/// Gets the parent module of the given module.
pub fn module_parent(db: &dyn Db, module: InLibrary<ModuleId>) -> Option<ModuleId> {
    db.module_tree_of(module.0).parent_of(module.1)
}

/// Tests if `parent` is an ancestor module of `child`.
/// All modules are their own ancestors.
#[salsa::tracked]
pub fn is_module_ancestor(
    db: &dyn Db,
    library: SourceLibrary,
    parent: ModuleId,
    child: ModuleId,
) -> bool {
    // All modules are their own ancestor
    if parent == child {
        return true;
    }

    // Don't need to route through query, would create redundant module tree clones
    let module_tree = db.module_tree_of(library.into());

    // Module is only an ancestor if it's anywhere on the hierarchy
    std::iter::successors(module_tree.parent_of(child), |id| {
        module_tree.parent_of(*id)
    })
    .any(|mod_id| mod_id == parent)
}

/// Looks up which module the given HIR node is in
pub fn inside_module(db: &dyn Db, inside_module: InsideModule) -> item::ModuleId {
    let inside_module = match inside_module {
        InsideModule::Item(library_id, item_id) => {
            // We're either at a module-like, or at a plain old item
            //
            // We stop at module-likes since we're directed here via bodies,
            // and we don't need to distinguish between at module decl and inside body,
            // at least right now.
            let library = db.library(library_id);

            let module_id = match item::ModuleId::try_new(&library, item_id) {
                Some(module_id) => module_id,
                None => db
                    .module_tree_of(library_id)
                    .module_of(item_id)
                    .expect("missing item link"),
            };

            return module_id;
        }
        // Type gets referred to the type's owner
        InsideModule::Type(library_id, type_id) => {
            let owner = db.type_owner(InLibrary(library_id, type_id));

            match owner {
                TypeOwner::Item(item_id) => (library_id, item_id).into(),
                TypeOwner::Type(type_id) => (library_id, type_id).into(),
            }
        }
        // Body gets referred to the body's owner
        InsideModule::Body(library_id, body_id) => {
            let owner = db
                .body_owner(InLibrary(library_id, body_id))
                .expect("missing owner");

            match owner {
                BodyOwner::Item(item_id) => (library_id, item_id).into(),
                BodyOwner::Type(type_id) => (library_id, type_id).into(),
                BodyOwner::Expr(expr_id) => (library_id, expr_id).into(),
            }
        }
        // Body{Stmt,Expr} gets referred to the owning body
        InsideModule::Stmt(library_id, stmt::BodyStmt(body_id, _))
        | InsideModule::Expr(library_id, expr::BodyExpr(body_id, _)) => {
            (library_id, body_id).into()
        }
    };

    db.inside_module(inside_module)
}

/// Looks up the corresponding item for the given [`DefId`],
/// or `None` if it doesn't exist.
///
/// This does not perform any form of definition resolution.
pub fn item_of(db: &dyn Db, def_id: DefId) -> Option<InLibrary<item::ItemId>> {
    db.def_owner(def_id).and_then(|owner| match owner {
        DefOwner::Item(id) => Some(InLibrary(def_id.0, id)),
        _ => None,
    })
}

/// Gets all of the bodies in the given library
#[salsa::tracked]
pub fn bodies_of(db: &dyn Db, library: SourceLibrary) -> Arc<Vec<body::BodyId>> {
    let library = hir_library(db, library);
    Arc::new(library.body_ids())
}

/// Resolved the given `def_id` to the canonical definition (i.e. beyond any exports),
/// or the last def before an undeclared was encountered.
pub fn resolve_def(db: &dyn Db, def_id: DefId) -> Result<DefId, DefId> {
    // ???: How does this interact with def collecting (i.e how are we redirected to the right library)?
    // Assuming it'd be done through `ItemExport` holding a library id to the real definition

    // Was doing: making def resolution fallible
    // Seems best to return the last def before we hit an undeclared

    let library = db.library(def_id.library());

    let next_def = match library.def_resolve(def_id.local()) {
        // Follow the indirection
        DefResolve::Local(local_def) => DefId(def_id.library(), local_def),
        DefResolve::External(def) => def,
        // This is the canonical def
        DefResolve::Canonical => return Ok(def_id),
        // Last def before failing
        DefResolve::Err => return Err(def_id),
    };

    // Poke through import chains
    db.resolve_def(next_def)
}

/// Gets the [`SymbolKind`] of the given `def_id`, or [`None`] if it's an undeclared definition.
///
/// This does not perform any form of definition resolution.
pub fn symbol_kind(db: &dyn Db, def_id: DefId) -> Option<SymbolKind> {
    // Take the binding kind from the def owner
    let library = db.library(def_id.0);
    let def_info = library.local_def(def_id.1);
    let kind = def_info.kind?;

    // Defs by this point should have been resolved by a prior
    // `resolve_def` query
    if matches!(kind, SymbolKind::Import | SymbolKind::Export) {
        unreachable!("defs should already be resolved defs to the canonical def")
    }

    Some(kind)
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

    fn visit_module(&self, id: item::ItemId, item: &item::Module) {
        // Add all imports and exports as being owned by this one
        for (idx, export) in item.exports.iter().enumerate() {
            self.add_owner(
                export.def_id,
                DefOwner::Export(item::ModuleId::new(self.library, id), item::ExportId(idx)),
            );
        }
    }

    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        if let Some(params) = &item.param_list {
            for name in &params.names {
                // Skip the filler args
                // They are placeholders, and can't be named anyways
                if self.library.local_def(*name).kind.is_none() {
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

    fn visit_enum(&self, id: ty::TypeId, ty: &ty::Enum) {
        for (idx, &variant) in ty.variants.iter().enumerate() {
            self.add_owner(variant, DefOwner::Field(id, ty::FieldId(idx)));
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

    fn visit_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        self.add_owner(item.bind_to, BodyOwner::Item(id));
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

    fn visit_init_expr(&self, id: expr::BodyExpr, expr: &expr::Init) {
        for &body_id in &expr.exprs {
            self.add_owner(body_id, BodyOwner::Expr(id));
        }
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

    fn visit_constrained(&self, id: ty::TypeId, ty: &ty::Constrained) {
        self.add_owner(ty.start, BodyOwner::Type(id));

        if let ty::ConstrainedEnd::Expr(end) = ty.end {
            self.add_owner(end, BodyOwner::Type(id))
        }
    }
}

/// Library-local type owner collector
struct TypeCollector {
    type_table: RefCell<TypeOwners>,
}

impl TypeCollector {
    fn add_owner(&self, type_id: ty::TypeId, owner: TypeOwner) {
        self.type_table.borrow_mut().add_owner(type_id, owner);
    }
}

impl HirVisitor for TypeCollector {
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        if let Some(ty_spec) = item.type_spec {
            self.add_owner(ty_spec, TypeOwner::Item(id));
        }
    }

    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {
        if let item::DefinedType::Alias(ty_id) = item.type_def {
            self.add_owner(ty_id, TypeOwner::Item(id));
        }
    }

    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        if let Some(param_list) = &item.param_list {
            for param in &param_list.tys {
                self.add_owner(param.param_ty, TypeOwner::Item(id));
            }
        }

        self.add_owner(item.result.ty, TypeOwner::Item(id));
    }

    fn visit_array(&self, id: ty::TypeId, ty: &ty::Array) {
        for &range_ty in &ty.ranges {
            self.add_owner(range_ty, TypeOwner::Type(id));
        }

        self.add_owner(ty.elem_ty, TypeOwner::Type(id));
    }

    fn visit_set(&self, id: ty::TypeId, ty: &ty::Set) {
        self.add_owner(ty.elem_ty, TypeOwner::Type(id));
    }

    fn visit_pointer(&self, id: ty::TypeId, ty: &ty::Pointer) {
        self.add_owner(ty.ty, TypeOwner::Type(id));
    }

    fn visit_subprogram_ty(&self, id: ty::TypeId, ty: &ty::Subprogram) {
        if let Some(param_list) = &ty.param_list {
            for param in param_list {
                self.add_owner(param.param_ty, TypeOwner::Type(id));
            }
        }

        self.add_owner(ty.result_ty, TypeOwner::Type(id));
    }
}
