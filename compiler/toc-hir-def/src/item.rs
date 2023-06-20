use toc_ast_db::IntoAst;
use toc_hir_expand::{SemanticFile, SemanticLoc};
use toc_source_graph::Package;
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    body::{Body, BodyOrigin},
    item::item_loc_map::ItemLocMap,
    Db, IsMonitor, IsPervasive, IsRegister, ItemAttrs, Mutability, Symbol,
};

mod lower;
pub mod pretty;

pub mod item_loc_map {
    //! Mapping to go from [`SemanticLoc`]'s to [`Item`](super::Item)'s

    use toc_hir_expand::{ErasedSemanticLoc, SemanticLoc};
    use toc_syntax::ast::{self, AstNode};

    use crate::item::{ConstVar, Module};

    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct ItemLocMap {
        to_items: rustc_hash::FxHashMap<ErasedSemanticLoc, salsa::Id>,
    }

    impl ItemLocMap {
        pub(crate) fn new() -> Self {
            Self::default()
        }

        pub(crate) fn insert<N: ToHirItem>(&mut self, loc: SemanticLoc<N>, item: N::Item) {
            use salsa::AsId;

            let old = self.to_items.insert(loc.into_erased(), item.as_id());
            assert!(
                old.is_none(),
                "duplicate ItemLocMap entry (replacing {old:?} with {item:?})"
            )
        }

        pub(crate) fn shrink_to_fit(&mut self) {
            self.to_items.shrink_to_fit();
        }

        /// Looks up the stable location's item
        pub fn get<N: ToHirItem>(&self, loc: SemanticLoc<N>) -> Option<N::Item> {
            use salsa::AsId;

            self.to_items
                .get(&loc.into_erased())
                .copied()
                .map(N::Item::from_id)
        }
    }

    pub trait ToHirItem: AstNode<Language = toc_syntax::Lang> {
        type Item: salsa::AsId;
    }

    impl ToHirItem for ast::ConstVarDeclName {
        type Item = ConstVar;
    }

    impl ToHirItem for ast::ModuleDecl {
        type Item = Module;
    }
}

/// Immediately accessible child items of an item, pair with a semantic location map
/// to go from `SemanticLoc` to the original item.
#[salsa::tracked]
pub struct ItemCollection {
    #[return_ref]
    pub items: Box<[Item]>,
    #[return_ref]
    pub loc_map: ItemLocMap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Item {
    ConstVar(ConstVar),
    Module(Module),
}

#[salsa::tracked]
pub struct ConstVar {
    #[id]
    pub name: Symbol,
    origin: ConstVarOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstVarOrigin {
    /// Original name node
    loc: SemanticLoc<ast::ConstVarDeclName>,
}

impl ConstVarOrigin {
    fn to_parent_constvar(self, db: &dyn Db) -> SemanticLoc<ast::ConstVarDecl> {
        self.loc.map(db.up(), |name| {
            name.syntax()
                .ancestors()
                .find_map(ast::ConstVarDecl::cast)
                .unwrap()
        })
    }
}

#[salsa::tracked]
impl ConstVar {
    pub fn mutability(self, db: &dyn Db) -> Mutability {
        self.item_attrs(db).mutablity()
    }

    pub fn is_pervasive(self, db: &dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    pub fn is_register(self, db: &dyn Db) -> IsRegister {
        self.item_attrs(db).is_register()
    }

    #[salsa::tracked]
    pub(crate) fn item_attrs(self, db: &dyn Db) -> ItemAttrs {
        let ast = self.origin(db).to_parent_constvar(db).to_node(db.up());

        let mut attrs = ItemAttrs::NONE;
        attrs |= ast
            .var_token()
            .map_or(ItemAttrs::NONE, |_| ItemAttrs::MUTABLE);
        attrs |= ast
            .pervasive_attr()
            .map_or(ItemAttrs::NONE, |_| ItemAttrs::PERVASIVE);
        attrs |= ast
            .register_attr()
            .map_or(ItemAttrs::NONE, |_| ItemAttrs::REGISTER);

        attrs
    }
}

#[salsa::tracked]
pub struct Module {
    #[id]
    pub name: Symbol,
    origin: ModuleOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleOrigin {
    /// Root module of a package
    Root(Package),
    /// Separate outline modules that are put into distinct files.
    /// Always corresponds to a file on the file system
    Unit(SourceFile),
    /// Inline module within a file
    Item(SemanticLoc<ast::ModuleDecl>),
}

#[salsa::tracked]
pub fn root_module(db: &dyn Db, package: Package) -> Module {
    // Take the name from the package
    let name = Symbol::new(db, package.name(db.up()).to_owned());
    Module::new(db, name, ModuleOrigin::Root(package))
}

#[salsa::tracked]
impl Module {
    /// Executable portion of a module
    #[salsa::tracked]
    pub fn body(self, db: &dyn Db) -> Body {
        Body::new(db, BodyOrigin::ModuleBody(self.stmt_list(db)))
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &dyn Db) -> ItemCollection {
        lower::collect_items(db, self.stmt_list(db))
    }

    #[salsa::tracked]
    pub(crate) fn stmt_list(self, db: &dyn Db) -> SemanticLoc<ast::StmtList> {
        match self.origin(db) {
            ModuleOrigin::Root(package) => {
                let file = toc_vfs_db::source_of(db.up(), package.root(db.up()));
                let file = SemanticFile::from_source_file(db.up(), file);
                let root = ast::Source::cast(file.ast(db.up())).unwrap();

                file.ast_locations(db.up()).get(&root.stmt_list().unwrap())
            }
            ModuleOrigin::Unit(_) => {
                // not accessible right now, but will probably be implemented by units having a wrapper module around the real unit item
                unimplemented!()
            }
            ModuleOrigin::Item(module) => module.map(db.up(), |it| it.stmt_list().unwrap()),
        }
    }

    pub fn is_pervasive(self, db: &dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    pub fn is_monitor(self, db: &dyn Db) -> IsMonitor {
        self.item_attrs(db).is_monitor()
    }

    #[salsa::tracked]
    pub(crate) fn item_attrs(self, db: &dyn Db) -> ItemAttrs {
        match self.origin(db) {
            ModuleOrigin::Item(module) => {
                let ast = module.to_node(db.up());

                let mut attrs = ItemAttrs::NONE;
                attrs |= ast
                    .pervasive_attr()
                    .map_or(ItemAttrs::NONE, |_| ItemAttrs::PERVASIVE);

                attrs
            }
            ModuleOrigin::Root(_) | ModuleOrigin::Unit(_) => ItemAttrs::NONE,
        }
    }
}

/// Any item which has nested child items
pub trait HasItems {
    type Item;

    /// All immediate child items of an item
    ///
    /// Note: This does not include items that are in the top level but
    /// are hidden inside of scopes, nor does it include items hidden
    /// behind macro expansions.
    fn items(self, db: &dyn Db) -> &Box<[Self::Item]>;

    /// Map to go from semantic locations back to the immediate item
    fn loc_map(self, db: &dyn Db) -> &ItemLocMap;
}

impl HasItems for Module {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        Module::collect_items(self, db).items(db)
    }

    fn loc_map(self, db: &dyn Db) -> &ItemLocMap {
        Module::collect_items(self, db).loc_map(db)
    }
}

impl HasItems for crate::body::ModuleBlock {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        module_block_collect_items(db, self).items(db)
    }

    fn loc_map(self, db: &dyn Db) -> &ItemLocMap {
        module_block_collect_items(db, self).loc_map(db)
    }
}

#[salsa::tracked]
pub(crate) fn module_block_collect_items(
    db: &dyn Db,
    block: crate::body::ModuleBlock,
) -> ItemCollection {
    lower::collect_items(db, block.stmt_list(db))
}
