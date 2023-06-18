use toc_ast_db::IntoAst;
use toc_hir_expand::{SemanticFile, SemanticLoc};
use toc_source_graph::Package;
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    body::{Body, BodyOrigin},
    Db, IsMonitor, IsPervasive, IsRegister, ItemAttrs, Mutability, Symbol,
};

mod lower;
pub mod pretty;

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
    /// Original parent node
    loc: SemanticLoc<ast::ConstVarDecl>,
    /// Which name in the original declaration list the name comes from
    index: usize,
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
        let ast = self.origin(db).loc.to_node(db.up());

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
    /// All immediate items of a module
    ///
    /// Note: This does not include items that are in the top level but
    /// are hidden inside of scopes
    #[salsa::tracked(return_ref)]
    pub(crate) fn items(self, db: &dyn Db) -> Box<[Item]> {
        lower::collect_items(db, self.stmt_list(db)).into()
    }

    /// Executable portion of a module
    #[salsa::tracked]
    pub fn body(self, db: &dyn Db) -> Body {
        Body::new(db, BodyOrigin::ModuleBody(self.stmt_list(db)))
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
    /// are hidden inside of scopes
    fn items(self, db: &dyn Db) -> &Box<[Self::Item]>;
}

impl HasItems for Module {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        Module::items(self, db)
    }
}

impl HasItems for crate::body::ModuleBlock {
    type Item = Item;

    fn items(self, db: &dyn Db) -> &Box<[Self::Item]> {
        module_block_items(db, self)
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn module_block_items(db: &dyn Db, block: crate::body::ModuleBlock) -> Box<[Item]> {
    lower::collect_items(db, block.stmt_list(db))
}
