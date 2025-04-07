use toc_ast_db::IntoAst;
use toc_hir_expand::{ErasedSemanticLoc, HasSource, SemanticFile, SemanticLoc};
use toc_source_graph::Package;
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    Db, IsMonitor, IsPervasive, IsRegister, ItemAttrs, Mutability, Symbol,
    body::{Body, BodyOrigin},
    item::item_loc_map::ItemLocMap,
};

macro_rules! impl_into_conversions {
    ($($item:ident),+ for $wrapper:ident) => {
        $(
            impl From<$item> for $wrapper {
                fn from(value: $item) -> Self {
                    Self::$item(value)
                }
            }
        )+

        $(
            impl TryFrom<$wrapper> for $item {
                type Error = ();

                fn try_from(value: $wrapper) -> Result<Self, Self::Error> {
                    match value {
                        $wrapper::$item(it) => Ok(it),
                        _ => Err(()),
                    }
                }
            }
        )+
    };

    ($($item:ident),+ for $wrapper:ident<$lt:lifetime>) => {
        $(
            impl<$lt> From<$item<$lt>> for $wrapper<$lt> {
                fn from(value: $item<$lt>) -> Self {
                    Self::$item(value)
                }
            }
        )+

        $(
            impl<$lt> TryFrom<$wrapper<$lt>> for $item<$lt> {
                type Error = ();

                fn try_from(value: $wrapper<$lt>) -> Result<Self, Self::Error> {
                    match value {
                        $wrapper::$item(it) => Ok(it),
                        _ => Err(()),
                    }
                }
            }
        )+
    };
}

mod lower;
pub mod pretty;

pub mod item_loc_map {
    //! Mapping to go from [`SemanticLoc`]'s to [`Item`](super::Item)'s

    use toc_hir_expand::{ErasedSemanticLoc, SemanticLoc};
    use toc_syntax::ast::{self, AstNode};

    use crate::item::{ConstVar, Item, Module};

    #[derive(Debug, Default, Clone, PartialEq, Eq, salsa::Update)]
    pub struct ItemLocMap<'db> {
        to_items: rustc_hash::FxHashMap<ErasedSemanticLoc<'db>, Item<'db>>,
    }

    impl<'db> ItemLocMap<'db> {
        pub(crate) fn new() -> Self {
            Self::default()
        }

        pub(crate) fn insert<N: ToHirItem>(
            &mut self,
            loc: SemanticLoc<'db, N>,
            item: N::Item<'db>,
        ) {
            let old = self.to_items.insert(loc.into_erased(), item.into());
            assert!(
                old.is_none(),
                "duplicate ItemLocMap entry (replacing {old:?} with {item:?})"
            );
        }

        pub(crate) fn shrink_to_fit(&mut self) {
            self.to_items.shrink_to_fit();
        }

        /// Looks up the stable location's item
        pub fn get<N: ToHirItem>(&self, loc: SemanticLoc<N>) -> N::Item<'db> {
            self.to_items
                .get(&loc.into_erased())
                .copied()
                .and_then(|it| N::Item::<'db>::try_from(it).ok())
                .expect("should have an item for the given SemanticLoc")
        }
    }

    pub trait ToHirItem: AstNode<Language = toc_syntax::Lang> {
        type Item<'db>: std::fmt::Debug + TryFrom<Item<'db>> + Into<Item<'db>> + Copy;
    }

    impl ToHirItem for ast::ConstVarDeclName {
        type Item<'db> = ConstVar<'db>;
    }

    impl ToHirItem for ast::ModuleDecl {
        type Item<'db> = Module<'db>;
    }
}

/// Any item that can be owned by another item
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update, salsa::Supertype,
)]
pub enum Item<'db> {
    ConstVar(ConstVar<'db>),
    Module(Module<'db>),
}

impl_into_conversions!(ConstVar, Module for Item<'db>);

#[salsa::tracked(debug)]
pub struct ConstVar<'db> {
    pub name: Symbol,
    /// Original name node
    #[tracked]
    origin: SemanticLoc<'db, ast::ConstVarDeclName>,
}

#[salsa::tracked]
impl<'db> ConstVar<'db> {
    pub fn mutability(self, db: &'db dyn Db) -> Mutability {
        self.item_attrs(db).mutablity()
    }

    pub fn is_pervasive(self, db: &'db dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    pub fn is_register(self, db: &'db dyn Db) -> IsRegister {
        self.item_attrs(db).is_register()
    }

    #[salsa::tracked]
    pub(crate) fn item_attrs(self, db: &'db dyn Db) -> ItemAttrs {
        let ast = self.parent_constvar(db).to_node(db);

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

    #[salsa::tracked]
    pub(crate) fn parent_constvar(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::ConstVarDecl> {
        self.origin(db).map(db, |name| {
            name.syntax()
                .ancestors()
                .find_map(ast::ConstVarDecl::cast)
                .unwrap()
        })
    }
}

impl<'db> HasSource<'db> for ConstVar<'db> {
    type Db = dyn Db;
    type Ast = ast::ConstVarDeclName;

    fn as_ast(&self, db: &'db Self::Db) -> Self::Ast {
        self.origin(db).to_node(db)
    }
}

#[salsa::tracked]
pub fn root_module<'db>(db: &'db dyn Db, package: Package) -> RootModule<'db> {
    // Take the name from the package
    let name = Symbol::new(db, package.name(db).to_owned());
    RootModule::new(db, name, package)
}

/// Root module of a package
#[salsa::tracked(debug)]
pub struct RootModule<'db> {
    pub name: Symbol,
    #[tracked]
    origin: Package,
}

#[salsa::tracked]
impl<'db> RootModule<'db> {
    /// Executable portion of the root module
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn Db) -> Body<'db> {
        Body::new(db, BodyOrigin::ModuleBody(self.stmt_list(db)))
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &'db dyn Db) -> ItemCollection<'db> {
        lower::collect_items(db, self.stmt_list(db))
    }

    #[salsa::tracked]
    pub(crate) fn stmt_list(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::StmtList> {
        let file = toc_vfs_db::source_of(db, self.origin(db).root(db).raw_path(db));
        let file = SemanticFile::from_package_file(db, self.origin(db), file);
        let root = ast::Source::cast(file.ast(db)).unwrap();

        file.ast_locations(db).get(&root.stmt_list().unwrap())
    }
}

impl<'db> HasItems<'db> for RootModule<'db> {
    type Item = Item<'db>;

    fn items(self, db: &'db dyn Db) -> &'db Box<[Self::Item]> {
        RootModule::collect_items(self, db).items(db)
    }

    fn loc_map(self, db: &'db dyn Db) -> &'db ItemLocMap<'db> {
        RootModule::collect_items(self, db).loc_map(db)
    }
}

/// Separate outline modules that are put into distinct files.
/// Always corresponds to a file on the file system
#[salsa::tracked(debug)]
pub struct UnitModule<'db> {
    #[salsa::tracked]
    origin: SourceFile,
}

#[salsa::tracked]
impl<'db> UnitModule<'db> {
    #[salsa::tracked]
    pub(crate) fn _stmt_list(self, _db: &'db dyn Db) -> SemanticLoc<'db, ast::StmtList> {
        // not accessible right now, but will probably be implemented by units
        // being a wrapper module around the real unit item
        unimplemented!()
    }
}

/// Inline module within a file
#[salsa::tracked(debug)]
pub struct Module<'db> {
    pub name: Symbol,
    #[tracked]
    origin: SemanticLoc<'db, ast::ModuleDecl>,
}

#[salsa::tracked]
impl<'db> Module<'db> {
    /// Executable portion of a module
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn Db) -> Body<'db> {
        Body::new(db, BodyOrigin::ModuleBody(self.stmt_list(db)))
    }

    /// Is the module pervasive?
    pub fn is_pervasive(self, db: &'db dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    /// Is the module a monitor?
    pub fn is_monitor(self, db: &'db dyn Db) -> IsMonitor {
        self.item_attrs(db).is_monitor()
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &'db dyn Db) -> ItemCollection<'db> {
        lower::collect_items(db, self.stmt_list(db))
    }

    #[salsa::tracked]
    pub(crate) fn stmt_list(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::StmtList> {
        self.origin(db).map(db, |it| it.stmt_list().unwrap())
    }

    #[salsa::tracked]
    pub(crate) fn item_attrs(self, db: &'db dyn Db) -> ItemAttrs {
        let ast = self.origin(db).to_node(db);

        let mut attrs = ItemAttrs::NONE;
        attrs |= ast
            .pervasive_attr()
            .map_or(ItemAttrs::NONE, |_| ItemAttrs::PERVASIVE);

        attrs
    }
}

impl<'db> HasSource<'db> for Module<'db> {
    type Db = dyn Db;
    type Ast = ast::ModuleDecl;

    fn as_ast(&self, db: &Self::Db) -> Self::Ast {
        self.origin(db).to_node(db)
    }
}

impl<'db> HasItems<'db> for Module<'db> {
    type Item = Item<'db>;

    fn items(self, db: &'db dyn Db) -> &'db Box<[Self::Item]> {
        Module::collect_items(self, db).items(db)
    }

    fn loc_map(self, db: &'db dyn Db) -> &'db ItemLocMap<'db> {
        Module::collect_items(self, db).loc_map(db)
    }
}

#[salsa::tracked]
pub(crate) fn module_block_collect_items<'db>(
    db: &'db dyn Db,
    block: crate::body::ModuleBlock<'db>,
) -> ItemCollection<'db> {
    lower::collect_items(db, block.stmt_list(db))
}

impl<'db> HasItems<'db> for crate::body::ModuleBlock<'db> {
    type Item = Item<'db>;

    fn items(self, db: &'db dyn Db) -> &'db Box<[Self::Item]> {
        module_block_collect_items(db, self).items(db)
    }

    fn loc_map(self, db: &'db dyn Db) -> &'db ItemLocMap<'db> {
        module_block_collect_items(db, self).loc_map(db)
    }
}

/// Any item which has nested child items
pub trait HasItems<'db> {
    type Item;

    /// All immediate child items of an item
    ///
    /// Note: This does not include items that are in the top level but
    /// are hidden inside of scopes, nor does it include items hidden
    /// behind macro expansions.
    fn items(self, db: &'db dyn Db) -> &'db Box<[Self::Item]>;

    /// Map to go from semantic locations back to the immediate item
    fn loc_map(self, db: &'db dyn Db) -> &'db ItemLocMap<'db>;
}

/// Immediately accessible child items of an item, pair with a semantic location map
/// to go from `SemanticLoc` to the original item.
#[salsa::tracked(debug)]
pub struct ItemCollection<'db> {
    #[tracked]
    #[return_ref]
    pub items: Box<[Item<'db>]>,
    #[tracked]
    #[return_ref]
    pub loc_map: ItemLocMap<'db>,
}

/// Every possible item
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Supertype)]
pub enum AnyItem<'db> {
    ConstVar(ConstVar<'db>),
    RootModule(RootModule<'db>),
    UnitModule(UnitModule<'db>),
    Module(Module<'db>),
}

impl_into_conversions!(ConstVar, RootModule, UnitModule, Module for AnyItem<'db>);

impl<'db> From<Item<'db>> for AnyItem<'db> {
    fn from(value: Item<'db>) -> Self {
        match value {
            Item::ConstVar(it) => Self::ConstVar(it),
            Item::Module(it) => Self::Module(it),
        }
    }
}

#[allow(unused)]
pub(crate) fn containing_item<'db>(db: &'db dyn Db, loc: ErasedSemanticLoc<'db>) -> AnyItem<'db> {
    let ast_locs = loc.file(db).ast_locations(db);
    let parent_ast = loc.to_node(db).ancestors().find_map(ast::Item::cast);

    if let Some(parent_item) = parent_ast {
        let parent_item = ast_locs.get(&parent_item);
        let loc_map = match containing_item(db, parent_item.into_erased()) {
            AnyItem::RootModule(item) => item.loc_map(db),
            AnyItem::UnitModule(_) => unimplemented!(),
            AnyItem::Module(item) => item.loc_map(db),
            AnyItem::ConstVar(_) => unreachable!("got parent item which can't be a parent"),
        };

        let item = match parent_item.to_node(db) {
            ast::Item::ConstVarDeclName(item) => Item::from(loc_map.get(ast_locs.get(&item))),
            ast::Item::TypeDecl(_) => todo!(),
            ast::Item::BindItem(_) => todo!(),
            ast::Item::ProcDecl(_) => todo!(),
            ast::Item::FcnDecl(_) => todo!(),
            ast::Item::ProcessDecl(_) => todo!(),
            ast::Item::ExternalDecl(_) => todo!(),
            ast::Item::ForwardDecl(_) => todo!(),
            ast::Item::DeferredDecl(_) => todo!(),
            ast::Item::BodyDecl(_) => todo!(),
            ast::Item::ModuleDecl(item) => Item::from(loc_map.get(ast_locs.get(&item))),
            ast::Item::ClassDecl(_) => todo!(),
            ast::Item::MonitorDecl(_) => todo!(),
        };

        AnyItem::from(item)
    } else {
        // FIXME: figure out if the file is from a UnitModule or RootModule
        match loc.file(db).origin(db) {
            toc_hir_expand::SemanticSource::PackageFile(file) => {
                AnyItem::RootModule(root_module(db, file.package(db)))
            }
        }
    }
}
