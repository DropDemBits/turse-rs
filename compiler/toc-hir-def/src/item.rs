use toc_ast_db::IntoAst;
use toc_hir_expand::{SemanticFile, SemanticLoc};
use toc_source_graph::Package;
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{
    body::{Body, BodyOrigin},
    Db, Mutability, Symbol,
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
    #[salsa::tracked]
    pub fn mutability(self, db: &dyn Db) -> Mutability {
        let ast = self.origin(db).loc.to_node(db.up());
        Mutability::from_is_mutable(ast.var_token().is_some())
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
    pub fn items(self, db: &dyn Db) -> Vec<Item> {
        lower::collect_items(db, self.stmt_list(db))
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
}
