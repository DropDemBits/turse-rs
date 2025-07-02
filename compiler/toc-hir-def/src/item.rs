use rustc_hash::FxHashMap;
use toc_ast_db::{SourceFileExt as _, ast_id::ErasedAstId};
use toc_hir_expand::{HasSource, SemanticFile, SemanticLoc, UnstableSemanticLoc};
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    Db, IsMonitor, IsPervasive, IsRegister, ItemAttrs, Mutability, Symbol,
    body::{Body, BodyOrigin},
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

#[salsa::tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct ConstVar<'db> {
    pub name: Symbol<'db>,
    /// Original name node
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

    pub(crate) fn parent_constvar(
        self,
        db: &'db dyn Db,
    ) -> UnstableSemanticLoc<'db, ast::ConstVarDecl> {
        self.origin(db).map_unstable(db, |name| {
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
    pub fn is_pervasive(self, db: &'db dyn Db) -> IsPervasive {
        self.item_attrs(db).is_pervasive()
    }

    /// Is the module a monitor?
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
