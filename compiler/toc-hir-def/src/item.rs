use toc_ast_db::SourceFileExt as _;
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

#[salsa::interned(debug)]
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
    /// Executable portion of the root module
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn Db) -> Body<'db> {
        Body::new(db, BodyOrigin::ModuleBody(self.root(db).into_erased()))
    }

    /// Collect the immediately accessible items
    #[salsa::tracked]
    pub(crate) fn collect_items(self, db: &'db dyn Db) -> ItemCollection<'db> {
        let stmt_list = self
            .root(db)
            .map_unstable(db, |this| this.stmt_list().unwrap());
        lower::collect_items(db, stmt_list)
    }

    #[salsa::tracked]
    pub(crate) fn root(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::Source> {
        let file = self.origin(db);
        let root = file.ast_id_map(db).root();

        SemanticLoc::new(SemanticFile::from_source_file(db, file), root)
    }
}

impl<'db> HasItems<'db> for RootModule<'db> {
    type Item = Item<'db>;

    fn items(self, db: &'db dyn Db) -> &'db [Self::Item] {
        &**RootModule::collect_items(self, db).items(db)
    }
}

/// Separate outline modules that are put into distinct files.
/// Always corresponds to a file on the file system
#[salsa::interned(debug)]
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
#[salsa::interned(debug)]
#[derive(PartialOrd, Ord)]
pub struct Module<'db> {
    pub name: Symbol<'db>,
    origin: SemanticLoc<'db, ast::ModuleDecl>,
}

#[salsa::tracked]
impl<'db> Module<'db> {
    /// Executable portion of a module
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn Db) -> Body<'db> {
        Body::new(db, BodyOrigin::ModuleBody(self.origin(db).into_erased()))
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

impl<'db> HasItems<'db> for Module<'db> {
    type Item = Item<'db>;

    fn items(self, db: &'db dyn Db) -> &'db [Self::Item] {
        &**Module::collect_items(self, db).items(db)
    }
}

#[salsa::tracked]
pub(crate) fn module_block_collect_items<'db>(
    db: &'db dyn Db,
    block: crate::body::ModuleBlock<'db>,
) -> ItemCollection<'db> {
    lower::collect_items(db, block.stmt_list(db).into_unstable(db))
}

impl<'db> HasItems<'db> for crate::body::ModuleBlock<'db> {
    type Item = Item<'db>;

    fn items(self, db: &'db dyn Db) -> &'db [Self::Item] {
        &**module_block_collect_items(db, self).items(db)
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
    fn items(self, db: &'db dyn Db) -> &'db [Self::Item];
}

/// Immediately accessible child items of an item, pair with a semantic location map
/// to go from `SemanticLoc` to the original item.
#[salsa::tracked(debug)]
pub struct ItemCollection<'db> {
    #[tracked]
    #[returns(ref)]
    pub items: Box<[Item<'db>]>,
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
