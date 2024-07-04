use toc_ast_db::IntoAst;
use toc_hir_expand::{ErasedSemanticLoc, HasSource, SemanticFile, SemanticLoc};
use toc_source_graph::Package;
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    body::{Body, BodyOrigin},
    item::item_loc_map::{BlockItemsMap, ItemLocMap},
    Db, IsMonitor, IsPervasive, IsRegister, ItemAttrs, Mutability, Symbol,
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
    };
}

mod lower;
pub mod pretty;

pub mod item_loc_map {
    //! Mapping to go from [`SemanticLoc`]'s to [`Item`](super::Item)'s

    use salsa::AsId;
    use toc_hir_expand::{ErasedSemanticLoc, SemanticLoc};
    use toc_syntax::ast::{self, AstNode};

    use crate::item::{BlockItems, ConstVar, Module};

    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct ItemLocMap {
        to_items: rustc_hash::FxHashMap<ErasedSemanticLoc, salsa::Id>,
    }

    impl ItemLocMap {
        pub(crate) fn new() -> Self {
            Self::default()
        }

        pub(crate) fn insert<N: ToHirItem>(&mut self, loc: SemanticLoc<N>, item: N::Item) {
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
        pub fn get<N: ToHirItem>(&self, loc: SemanticLoc<N>) -> N::Item {
            self.to_items
                .get(&loc.into_erased())
                .copied()
                .map(N::Item::from_id)
                .expect("should have an item for the given SemanticLoc")
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

    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct BlockItemsMap {
        to_block_items: rustc_hash::FxHashMap<ErasedSemanticLoc, BlockItems>,
    }

    impl BlockItemsMap {
        pub(crate) fn new() -> Self {
            Default::default()
        }

        pub(crate) fn insert(&mut self, loc: SemanticLoc<ast::StmtList>, block_items: BlockItems) {
            let old = self.to_block_items.insert(loc.into_erased(), block_items);
            assert!(
                old.is_none(),
                "duplicate BlockItemsLocMap entry (replacing {old:?} with {:?})",
                block_items.as_id()
            )
        }

        pub(crate) fn shrink_to_fit(&mut self) {
            self.to_block_items.shrink_to_fit();
        }

        /// Looks up the stable location's block items
        pub fn get(&self, loc: SemanticLoc<ast::StmtList>) -> BlockItems {
            self.to_block_items
                .get(&loc.into_erased())
                .copied()
                .expect("should have a block item for the given SemanticLoc")
        }
    }
}

/// Any item that can be owned by another item
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    ConstVar(ConstVar),
    Module(Module),
}

impl_into_conversions!(ConstVar, Module for Item);

/// Items contained within a block scope
#[salsa::tracked]
pub struct BlockItems {
    /// Original list node
    origin: SemanticLoc<ast::StmtList>,
}

#[salsa::tracked]
impl BlockItems {
    pub(crate) fn stmt_list(self, db: &dyn Db) -> SemanticLoc<ast::StmtList> {
        self.origin(db)
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &dyn Db) -> ItemCollection {
        lower::to_immediate_items(db, self.collect_nested_items(db))
    }

    /// Collect the immediately nested items
    #[salsa::tracked]
    pub(crate) fn collect_nested_items(self, db: &dyn Db) -> NestedItemCollection {
        lower::collect_nested_items(db, self.stmt_list(db))
    }
}

impl HasItems for BlockItems {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        Self::collect_items(self, db).items(db)
    }

    fn nested_items(self, db: &dyn Db) -> &Box<[NestedItem<Self::Item>]> {
        Self::collect_nested_items(self, db).items(db)
    }

    fn loc_map(self, db: &dyn Db) -> &ItemLocMap {
        Self::collect_items(self, db).loc_map(db)
    }
}

#[salsa::tracked]
pub struct ConstVar {
    #[id]
    pub name: Symbol,
    /// Original name node
    origin: SemanticLoc<ast::ConstVarDeclName>,
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
        let ast = self.parent_constvar(db).to_node(db.up());

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
    pub(crate) fn parent_constvar(self, db: &dyn Db) -> SemanticLoc<ast::ConstVarDecl> {
        self.origin(db).map(db.up(), |name| {
            name.syntax()
                .ancestors()
                .find_map(ast::ConstVarDecl::cast)
                .unwrap()
        })
    }
}

impl HasSource for ConstVar {
    type Db<'db> = dyn Db + 'db;
    type Ast = ast::ConstVarDeclName;

    fn as_ast(&self, db: &Self::Db<'_>) -> Self::Ast {
        self.origin(db).to_node(db.up())
    }
}

#[salsa::tracked]
pub fn root_module(db: &dyn Db, package: Package) -> RootModule {
    // Take the name from the package
    let name = Symbol::new(db, package.name(db.up()).to_owned());
    RootModule::new(db, name, package)
}

/// Root module of a package
#[salsa::tracked]
pub struct RootModule {
    #[id]
    pub name: Symbol,
    origin: Package,
}

#[salsa::tracked]
impl RootModule {
    /// Executable portion of the root module
    #[salsa::tracked]
    pub fn body(self, db: &dyn Db) -> Body {
        Body::new(db, BodyOrigin::ModuleBody(self.stmt_list(db)))
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &dyn Db) -> ItemCollection {
        lower::to_immediate_items(db, self.collect_nested_items(db))
    }

    /// Collect the immediately nested items
    #[salsa::tracked]
    pub(crate) fn collect_nested_items(self, db: &dyn Db) -> NestedItemCollection {
        lower::collect_nested_items(db, self.stmt_list(db))
    }

    #[salsa::tracked]
    pub(crate) fn stmt_list(self, db: &dyn Db) -> SemanticLoc<ast::StmtList> {
        let file = toc_vfs_db::source_of(db.up(), self.origin(db).root(db.up()));
        let file = SemanticFile::from_package_file(db.up(), self.origin(db), file);
        let root = ast::Source::cast(file.ast(db.up())).unwrap();

        file.ast_locations(db.up()).get(&root.stmt_list().unwrap())
    }
}

impl HasItems for RootModule {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        Self::collect_items(self, db).items(db)
    }

    fn nested_items(self, db: &dyn Db) -> &Box<[NestedItem<Self::Item>]> {
        Self::collect_nested_items(self, db).items(db)
    }

    fn loc_map(self, db: &dyn Db) -> &ItemLocMap {
        Self::collect_items(self, db).loc_map(db)
    }
}

/// Separate outline modules that are put into distinct files.
/// Always corresponds to a file on the file system
#[salsa::tracked]
pub struct UnitModule {
    origin: SourceFile,
}

#[salsa::tracked]
impl UnitModule {
    #[salsa::tracked]
    pub(crate) fn _stmt_list(self, _db: &dyn Db) -> SemanticLoc<ast::StmtList> {
        // not accessible right now, but will probably be implemented by units
        // being a wrapper module around the real unit item
        unimplemented!()
    }
}

/// Inline module within a file
#[salsa::tracked]
pub struct Module {
    #[id]
    pub name: Symbol,
    origin: SemanticLoc<ast::ModuleDecl>,
}

#[salsa::tracked]
impl Module {
    /// Executable portion of a module
    #[salsa::tracked]
    pub fn body(self, db: &dyn Db) -> Body {
        Body::new(db, BodyOrigin::ModuleBody(self.stmt_list(db)))
    }

    /// Is the module pervasive?
    pub fn is_pervasive(self, db: &dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    /// Is the module a monitor?
    pub fn is_monitor(self, db: &dyn Db) -> IsMonitor {
        self.item_attrs(db).is_monitor()
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &dyn Db) -> ItemCollection {
        lower::to_immediate_items(db, self.collect_nested_items(db))
    }

    /// Collect the immediately nested items
    #[salsa::tracked]
    pub(crate) fn collect_nested_items(self, db: &dyn Db) -> NestedItemCollection {
        lower::collect_nested_items(db, self.stmt_list(db))
    }

    #[salsa::tracked]
    pub(crate) fn stmt_list(self, db: &dyn Db) -> SemanticLoc<ast::StmtList> {
        self.origin(db).map(db.up(), |it| it.stmt_list().unwrap())
    }

    #[salsa::tracked]
    pub(crate) fn item_attrs(self, db: &dyn Db) -> ItemAttrs {
        let ast = self.origin(db).to_node(db.up());

        let mut attrs = ItemAttrs::NONE;
        attrs |= ast
            .pervasive_attr()
            .map_or(ItemAttrs::NONE, |_| ItemAttrs::PERVASIVE);

        attrs
    }
}

impl HasSource for Module {
    type Db<'db> = dyn Db + 'db;
    type Ast = ast::ModuleDecl;

    fn as_ast(&self, db: &Self::Db<'_>) -> Self::Ast {
        self.origin(db).to_node(db.up())
    }
}

impl HasItems for Module {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        Self::collect_items(self, db).items(db)
    }

    fn nested_items(self, db: &dyn Db) -> &Box<[NestedItem<Self::Item>]> {
        Self::collect_nested_items(self, db).items(db)
    }

    fn loc_map(self, db: &dyn Db) -> &ItemLocMap {
        Self::collect_items(self, db).loc_map(db)
    }
}

/// Collect the immediately accessible items
#[salsa::tracked]
pub(crate) fn module_block_collect_items(
    db: &dyn Db,
    block: crate::body::ModuleBlock,
) -> ItemCollection {
    lower::to_immediate_items(db, module_block_collect_nested_items(db, block))
}

/// Collect the immediately nested items
#[salsa::tracked]
pub(crate) fn module_block_collect_nested_items(
    db: &dyn Db,
    block: crate::body::ModuleBlock,
) -> NestedItemCollection {
    lower::collect_nested_items(db, block.stmt_list(db))
}

impl HasItems for crate::body::ModuleBlock {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        module_block_collect_items(db, self).items(db)
    }

    fn loc_map(self, db: &dyn Db) -> &ItemLocMap {
        module_block_collect_items(db, self).loc_map(db)
    }

    fn nested_items(self, db: &dyn Db) -> &Box<[NestedItem<Self::Item>]> {
        module_block_collect_nested_items(db, self).items(db)
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

    /// All immediate child items of an item, plus the nested block item entities.
    ///
    /// Each nested [`BlockItems`] contains items hidden inside of scopes, but
    /// not any items hidden behind macro expansions.
    fn nested_items(self, db: &dyn Db) -> &Box<[NestedItem<Self::Item>]>;

    /// Map to go from semantic locations back to the immediate item
    fn loc_map(self, db: &dyn Db) -> &ItemLocMap;
}

/// Immediately accessible child items of an item (i.e. items not within any
/// block scopes), paired with a semantic location map to go from `SemanticLoc`
/// to the original item.
#[salsa::tracked]
pub struct ItemCollection {
    #[return_ref]
    pub items: Box<[Item]>,
    #[return_ref]
    pub loc_map: ItemLocMap,
}

/// Immediately nested child items of an item (i.e. items within any block
/// scopes but not within any child items), paired with a semantic location map
/// to go from `SemanticLoc` to the original item.
#[salsa::tracked]
pub struct NestedItemCollection {
    #[return_ref]
    pub items: Box<[NestedItem<Item>]>,
    #[return_ref]
    pub loc_map: ItemLocMap,
    #[return_ref]
    pub block_items_map: BlockItemsMap,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NestedItem<Item> {
    Item(Item),
    Nested(BlockItems),
}

/// Every possible item
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyItem {
    ConstVar(ConstVar),
    RootModule(RootModule),
    UnitModule(UnitModule),
    Module(Module),
}

impl_into_conversions!(ConstVar, RootModule, UnitModule, Module for AnyItem);

impl From<Item> for AnyItem {
    fn from(value: Item) -> Self {
        match value {
            Item::ConstVar(it) => Self::ConstVar(it),
            Item::Module(it) => Self::Module(it),
        }
    }
}

#[salsa::tracked]
pub(crate) fn containing_item(db: &dyn Db, loc: ErasedSemanticLoc) -> AnyItem {
    let ast_locs = loc.file(db.up()).ast_locations(db.up());
    let parent_ast = loc.to_node(db.up()).ancestors().find_map(ast::Item::cast);

    if let Some(parent_item) = parent_ast {
        let parent_item = ast_locs.get(&parent_item);
        let loc_map = match containing_item(db, parent_item.into_erased()) {
            AnyItem::RootModule(item) => item.loc_map(db),
            AnyItem::UnitModule(_) => unimplemented!(),
            AnyItem::Module(item) => item.loc_map(db),
            AnyItem::ConstVar(_) => unreachable!("got parent item which can't be a parent"),
        };

        let item = match parent_item.to_node(db.up()) {
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
        match loc.file(db.up()).origin(db.up()) {
            toc_hir_expand::SemanticSource::PackageFile(file) => {
                AnyItem::RootModule(root_module(db, file.package(db.up())))
            }
        }
    }
}
