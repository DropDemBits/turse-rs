//! Query implementations

use std::{cell::RefCell, sync::Arc};

use toc_hir::{
    body::{self, BodyOwner, BodyTable},
    expr,
    item::{self, ModuleId, ModuleTree},
    library::{InLibrary, Library, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    stmt,
    symbol::{DefId, DefOwner, DefTable, LocalDefId, SymbolKind},
    ty::{self, TypeOwner, TypeOwners},
    visitor::HirVisitor,
};

use crate::db::{HirDatabase, InsideModule};

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

    // Collect bodies
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

pub(crate) fn collect_type_owners(db: &dyn HirDatabase, library_id: LibraryId) -> Arc<TypeOwners> {
    use toc_hir::visitor::Walker;

    let library = db.library(library_id);

    // Collect types
    let type_collector = TypeCollector {
        type_table: Default::default(),
    };
    Walker::from_library(&library).visit_preorder(&type_collector);

    Arc::new(type_collector.type_table.into_inner())
}

pub(crate) fn lookup_type_owner(db: &dyn HirDatabase, ty: InLibrary<ty::TypeId>) -> TypeOwner {
    db.type_owners_of(ty.0).lookup_owner(ty.1)
}

pub(crate) fn collect_module_tree(db: &dyn HirDatabase, library: LibraryId) -> Arc<ModuleTree> {
    use toc_hir::visitor::{WalkEvent, WalkNode, Walker};

    let library = db.library(library);

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

pub(crate) fn lookup_module_parent(
    db: &dyn HirDatabase,
    module: InLibrary<ModuleId>,
) -> Option<ModuleId> {
    db.module_tree_of(module.0).parent_of(module.1)
}

pub(crate) fn is_module_ancestor(
    db: &dyn HirDatabase,
    parent: InLibrary<ModuleId>,
    child: InLibrary<ModuleId>,
) -> bool {
    // Must be from the same library
    if parent.0 != child.0 {
        return false;
    }

    // All modules are their own ancestor
    if parent == child {
        return true;
    }

    // Don't need to route through query, would create redundant module tree clones
    let module_tree = db.module_tree_of(parent.0);

    // Module is only an ancestor if it's anywhere on the hierarchy
    std::iter::successors(module_tree.parent_of(child.1), |id| {
        module_tree.parent_of(*id)
    })
    .any(|mod_id| mod_id == parent.1)
}

pub(crate) fn lookup_inside_module(
    db: &dyn HirDatabase,
    inside_module: InsideModule,
) -> item::ModuleId {
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

pub(crate) fn resolve_def(db: &dyn HirDatabase, def_id: DefId) -> DefId {
    // ???: How does this interact with def collecting (i.e how are we redirected to the right library)?
    // Assuming it'd be done through `ItemExport` holding a library id to the real definition

    let def_owner = if let Some(def_owner) = db.def_owner(def_id) {
        def_owner
    } else {
        return def_id;
    };

    // Poke through item exports
    match def_owner {
        DefOwner::Export(mod_id, export_id) => {
            let library = db.library(def_id.0);

            // Get associated item
            let module = library.module_item(mod_id);
            let export = module.export(export_id);
            let exported_item = library.item(export.item_id);

            DefId(def_id.0, exported_item.def_id)
        }
        // Already the canonical definition
        _ => def_id,
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

    fn visit_module(&self, id: item::ItemId, item: &item::Module) {
        // Add all exports as being owned by this one
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
