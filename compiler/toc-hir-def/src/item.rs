use rustc_hash::FxHashMap;
use toc_ast_db::{SourceFileExt as _, ast_id::ErasedAstId};
use toc_hir_expand::{HasSource, SemanticFile, SemanticLoc, UnstableSemanticLoc};
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    Db, IsMonitor, IsPervasive, IsRegister, ItemAttrs, Mutability, Symbol,
    body::{Body, BodyOrigin},
};

macro_rules! impl_item_ast_id {
    (impl ItemAstId<$lt:lifetime> for $($item:ident => $ast:path),+ $(,)?) => {
        $(
            impl<$lt> ItemAstId<$lt> for $ast {
                type Item = $item<$lt>;
            }
        )+
    };
}

macro_rules! impl_has_items{
    (impl HasItems<$lt:lifetime> for $($item:ident => |$db:ident, $this:ident| $accessor:expr),+ $(,)?) => {
        $(
            impl<$lt> HasItems<$lt> for $item<$lt> {
                fn child_items(self, $db: &$lt dyn Db) -> ChildItems<$lt> {
                    let $this = self;
                    $accessor
                }
            }
        )+
    };
}

mod lower;
pub mod pretty;

/// Any item that can be owned by another item
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update, salsa::Supertype,
)]
pub enum Item<'db> {
    ConstVar(ConstVar<'db>),
    Module(Module<'db>),
}

impl_into_conversions!(ConstVar, Module for Item<'db>);

impl<'db> Item<'db> {
    pub fn name(self, db: &'db dyn Db) -> Symbol<'db> {
        match self {
            Item::ConstVar(const_var) => const_var.name(db),
            Item::Module(module) => module.name(db),
        }
    }
}

#[salsa::tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct ConstVar<'db> {
    pub name: Symbol<'db>,
    /// Original name node
    origin: SemanticLoc<'db, ast::ConstVarDeclName>,

    #[tracked]
    item_attrs: ItemAttrs,
}

#[salsa::tracked]
impl<'db> ConstVar<'db> {
    #[salsa::tracked]
    pub fn mutability(self, db: &'db dyn Db) -> Mutability {
        self.item_attrs(db).mutablity()
    }

    #[salsa::tracked]
    pub fn is_pervasive(self, db: &'db dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    #[salsa::tracked]
    pub fn is_register(self, db: &'db dyn Db) -> IsRegister {
        self.item_attrs(db).is_register()
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
pub fn root_module<'db>(db: &'db dyn Db, source_file: SourceFile) -> RootModule<'db> {
    // Take the name from the name of the file
    let name = Symbol::new(
        db,
        source_file
            .path(db)
            .as_path()
            .file_name()
            .unwrap_or("<root>")
            .to_owned(),
    );

    RootModule::new(db, name, source_file)
}

/// Root module of a package
#[salsa::interned(debug)]
#[derive(PartialOrd, Ord)]
pub struct RootModule<'db> {
    pub name: Symbol<'db>,
    origin: SourceFile,
}

#[salsa::tracked]
impl<'db> RootModule<'db> {
    /// Originating source node
    #[salsa::tracked]
    pub fn root(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::Source> {
        let file = self.origin(db);
        let root = file.ast_id_map(db).root();

        SemanticLoc::new(SemanticFile::from_source_file(db, file), root)
    }

    /// Executable portion of the root module
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn Db) -> Body<'db> {
        Body::new(db, BodyOrigin::ModuleBody(self.into()))
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &'db dyn Db) -> ChildItems<'db> {
        let stmt_list = self
            .root(db)
            .map_unstable(db, |this| this.stmt_list().unwrap());
        lower::collect_items(db, stmt_list)
    }
}

/// Separate outline modules that are put into distinct files.
/// Always corresponds to a file on the file system
#[salsa::tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct UnitModule<'db> {
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
#[derive(PartialOrd, Ord)]
pub struct Module<'db> {
    pub name: Symbol<'db>,
    origin: SemanticLoc<'db, ast::ModuleDecl>,

    #[tracked]
    item_attrs: ItemAttrs,
}

#[salsa::tracked]
impl<'db> Module<'db> {
    /// Originating semantic location
    pub fn into_loc(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::ModuleDecl> {
        self.origin(db)
    }

    /// Executable portion of a module
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn Db) -> Body<'db> {
        Body::new(db, BodyOrigin::ModuleBody(self.into()))
    }

    /// Is the module pervasive?
    #[salsa::tracked]
    pub fn is_pervasive(self, db: &'db dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    /// Is the module a monitor?
    #[salsa::tracked]
    pub fn is_monitor(self, db: &'db dyn Db) -> IsMonitor {
        self.item_attrs(db).is_monitor()
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &'db dyn Db) -> ChildItems<'db> {
        lower::collect_items(db, self.stmt_list(db))
    }

    #[salsa::tracked]
    pub(crate) fn stmt_list(self, db: &'db dyn Db) -> UnstableSemanticLoc<'db, ast::StmtList> {
        self.origin(db)
            .map_unstable(db, |it| it.stmt_list().unwrap())
    }
}

impl<'db> HasSource<'db> for Module<'db> {
    type Db = dyn Db;
    type Ast = ast::ModuleDecl;

    fn as_ast(&self, db: &Self::Db) -> Self::Ast {
        self.origin(db).to_node(db)
    }
}

/// A block that contains items
#[salsa::tracked(debug)]
pub struct ModuleBlock<'db> {
    origin: SemanticLoc<'db, ast::StmtList>,
}

impl<'db> ModuleBlock<'db> {
    pub(crate) fn stmt_list(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::StmtList> {
        self.origin(db)
    }
}

#[salsa::tracked]
impl<'db> ModuleBlock<'db> {
    #[salsa::tracked]
    fn collect_items(self, db: &'db dyn Db) -> ChildItems<'db> {
        lower::collect_items(db, self.stmt_list(db).into_unstable(db))
    }
}

/// Any module that acts like a module.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update, salsa::Supertype,
)]
pub enum ModuleLike<'db> {
    Module(Module<'db>),
    RootModule(RootModule<'db>),
}

impl_into_conversions!(RootModule, Module for ModuleLike<'db>);

/// Any item which has nested child items
pub trait HasItems<'db> {
    /// All immediate child items of an item
    ///
    /// Note: This does not include items that are in the top level but
    /// are hidden inside of scopes, nor does it include items hidden
    /// behind macro expansions.
    fn child_items(self, db: &'db dyn Db) -> ChildItems<'db>;
}

impl_has_items! {
    impl HasItems<'db> for
        Module => |db, this| this.collect_items(db),
        RootModule => |db, this| this.collect_items(db),
        ModuleBlock => |db, this| this.collect_items(db),
}

impl<'db> HasItems<'db> for ModuleLike<'db> {
    fn child_items(self, db: &'db dyn Db) -> ChildItems<'db> {
        match self {
            ModuleLike::Module(module) => module.child_items(db),
            ModuleLike::RootModule(root_module) => root_module.child_items(db),
        }
    }
}

/// Scope where an item is visible in.
#[salsa::tracked]
#[derive(PartialOrd, Ord)]
pub struct ItemScope<'db> {
    item: Item<'db>,
}

impl<'db> std::fmt::Debug for ItemScope<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = if f.alternate() {
            salsa::with_attached_database(|db| {
                use salsa::plumbing::AsId;

                let id_fmt = format!("[{}]", self.as_id().index());

                match self.item(db) {
                    Item::ConstVar(_) => f.write_fmt(format_args!("ConstVar{id_fmt}")),
                    Item::Module(_) => f.write_fmt(format_args!("Module{id_fmt}")),
                }
            })
        } else {
            None
        };

        res.unwrap_or_else(|| Self::default_debug_fmt(*self, f))
    }
}

/// Any AST node that can be looked up in a [`ChildItems`]
pub trait ItemAstId<'db>: AstNode<Language = toc_syntax::Lang> {
    type Item: TryFrom<Item<'db>>;
}

impl_item_ast_id! {
    impl ItemAstId<'db> for
        ConstVar => ast::ConstVarDeclName,
        Module => ast::ModuleDecl,
}

/// Immediately accessible child items of an item, pair with a semantic location map
/// to go from `SemanticLoc` to the original item.
#[salsa::tracked(debug)]
pub struct ChildItems<'db> {
    file: SemanticFile<'db>,

    #[tracked]
    #[returns(ref)]
    pub items: Box<[Item<'db>]>,

    #[tracked]
    #[returns(ref)]
    to_items: FxHashMap<ErasedAstId, Item<'db>>,

    #[tracked]
    #[returns(ref)]
    to_scopes: FxHashMap<Item<'db>, ItemScope<'db>>,
}

#[salsa::tracked]
impl<'db> ChildItems<'db> {
    #[salsa::tracked]
    fn get_item_erased(self, db: &'db dyn Db, ast_id: ErasedAstId) -> Item<'db> {
        *self
            .to_items(db)
            .get(&ast_id)
            .unwrap_or_else(|| unreachable!("{ast_id:?} is not part of item collection {self:?}"))
    }

    pub fn get_item<T: ItemAstId<'db>>(self, db: &'db dyn Db, loc: SemanticLoc<'db, T>) -> T::Item {
        assert_eq!(
            self.file(db),
            loc.file(),
            "casting node from a different file"
        );

        let item = self.get_item_erased(db, loc.into_erased().ast_id());
        T::Item::try_from(item).unwrap_or_else(|_| panic!("incorrect AstId, casting from {loc:#?}"))
    }

    #[salsa::tracked]
    pub fn item_scope(self, db: &'db dyn Db, item: Item<'db>) -> ItemScope<'db> {
        *self
            .to_scopes(db)
            .get(&item)
            .unwrap_or_else(|| panic!("{item:?} does not have an associated item scope"))
    }
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
