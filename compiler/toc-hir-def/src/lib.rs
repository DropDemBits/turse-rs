//! Items and code bodies

use body::Body;
use toc_ast_db::IntoAst;
use toc_hir_expand::{self as expand, SemanticFile, SemanticLoc};
use toc_source_graph::Package;
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;
use upcast::{Upcast, UpcastFrom};

use crate::body::BodyOrigin;

pub(crate) mod internals {
    /// Helper for creating wrapper types of [`la_arena::Idx`].
    ///
    /// Only to be used inside of this crate.
    #[macro_export]
    macro_rules! arena_id_wrapper {
        // Just a newtype for the index
        (
            $(#[$attrs:meta])*
            $vis:vis struct $id:ident($wrap:path);
        ) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            $(#[$attrs])*
            $vis struct $id(pub(crate) ::la_arena::Idx<$wrap>);

            $crate::arena_id_wrapper!(@impl_rest, $id, $wrap);
        };
        // Newtype + type alias for the index
        (
            $(#[$attrs_wrap:meta])*
            $vis_wrap:vis struct $id:ident($wrap:path);
            $(#[$attrs_alias:meta])*
            $vis_alias:vis type $index_alias:ident = Index;
        ) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            $(#[$attrs_wrap])*
            $vis_wrap struct $id(pub(crate) $index_alias);

            $(#[$attrs_alias])*
            $vis_alias type $index_alias = ::la_arena::Idx<$wrap>;

            $crate::arena_id_wrapper!(@impl_rest, $id, $wrap);
        };
        // Other impls
        (
            @impl_rest, $id:ident, $wrap:path
        ) => {
            impl From<$id> for ::la_arena::Idx<$wrap> {
                fn from(id: $id) -> Self {
                    id.0
                }
            }

            impl From<&$id> for ::la_arena::Idx<$wrap> {
                fn from(id: &$id) -> Self {
                    id.0
                }
            }

            impl ::std::fmt::Debug for $id {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    let raw: u32 = self.0.into_raw().into();
                    f.debug_tuple(stringify!($id))
                        .field(&raw)
                        .finish()
                }
            }
        };
    }

    /// Simple named boolean
    #[macro_export]
    macro_rules! make_named_bool {
        (
            $(#[$attrs:meta])*
            $vis:vis enum $ident:ident $(;)?
        ) => {
            $(#[$attrs])*
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            $vis enum $ident {
                No,
                Yes
            }

            impl ::std::convert::From<bool> for $ident {
                fn from(v: bool) -> Self {
                    match v {
                        false => Self::No,
                        true => Self::Yes,
                    }
                }
            }

            impl ::std::convert::From<$ident> for bool {
                fn from(v: $ident) -> bool {
                    matches!(v, $ident::Yes)
                }
            }
        };
    }
}

pub mod body;
pub mod expr;
pub mod stmt;

mod lower {
    //! Lowering from AST nodes to semantic representations

    use toc_syntax::ast;

    use crate::expand::{AstLocations, SemanticLoc};

    use super::{ConstVar, ConstVarOrigin, Db, Item, Module, ModuleOrigin, Symbol};

    /// Collects the immediately accessible items from a [`ast::StmtList`]
    pub(crate) fn collect_items(db: &dyn Db, stmt_list: SemanticLoc<ast::StmtList>) -> Vec<Item> {
        let ast_locations = stmt_list.file(db.up()).ast_locations(db.up());
        let stmt_list = stmt_list.to_node(db.up());

        stmt_list
            .stmts()
            .filter_map(|stmt| item(db, stmt, ast_locations))
            .flatten()
            .collect()
    }

    /// Lowers a potential item, and returns either the new item, or `None`
    /// if we don't support lowering it yet or it's not a well-formed item
    /// (mainly because it doesn't have a name).
    ///
    /// Note that this means that we'll drop errors when a module doesn't
    /// have a name, but the more pressing fix anyway should be to focus
    /// on the error that a module doesn't have a name.
    pub(crate) fn item(
        db: &dyn Db,
        stmt: ast::Stmt,
        ast_locations: &AstLocations,
    ) -> Option<Vec<Item>> {
        Some(match stmt {
            ast::Stmt::ConstVarDecl(constvar) => {
                // uhhhhhhh
                let loc = ast_locations.get(&constvar);
                let names = constvar.decl_list().unwrap();
                let items = names
                    .names()
                    .enumerate()
                    .map(|(index, name)| {
                        let name =
                            Symbol::new(db, name.identifier_token().unwrap().text().to_owned());

                        Item::ConstVar(ConstVar::new(db, name, ConstVarOrigin { loc, index }))
                    })
                    .collect::<Vec<_>>();

                items
            }
            // ast::Stmt::TypeDecl(_) => todo!(),
            // ast::Stmt::BindDecl(_) => todo!(),
            // ast::Stmt::ProcDecl(_) => todo!(),
            // ast::Stmt::FcnDecl(_) => todo!(),
            // ast::Stmt::ProcessDecl(_) => todo!(),
            // ast::Stmt::ExternalDecl(_) => todo!(),
            // ast::Stmt::ForwardDecl(_) => todo!(),
            // ast::Stmt::DeferredDecl(_) => todo!(),
            // ast::Stmt::BodyDecl(_) => todo!(),
            ast::Stmt::ModuleDecl(module) => {
                let name = module.name()?.identifier_token().unwrap();
                let name = Symbol::new(db, name.text().to_owned());

                vec![Item::Module(Module::new(
                    db,
                    name,
                    ModuleOrigin::Item(ast_locations.get(&module)),
                ))]
            }
            // ast::Stmt::ClassDecl(_) => todo!(),
            // ast::Stmt::MonitorDecl(_) => todo!(),
            // ast::Stmt::ImportStmt(_) => todo!(),
            // ast::Stmt::ExportStmt(_) => todo!(),
            // ast::Stmt::PreprocGlob(_) => todo!(),
            _ => return None,
        })
    }
}

pub trait Db: salsa::DbWithJar<Jar> + expand::Db + Upcast<dyn expand::Db> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + expand::Db + Upcast<dyn expand::Db> {}

impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}

#[salsa::jar(db = Db)]
pub struct Jar(
    Symbol,
    root_module,
    ConstVar,
    ConstVar_mutability,
    Module,
    Module_items,
    Module_body,
    Module_stmt_list,
    body::Body,
    body::Body_top_level_stmts,
    body::Body_contents,
    body::Body_lower_contents,
);

#[salsa::interned]
pub struct Symbol {
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Var,
}

impl Mutability {
    pub fn from_is_mutable(is_var: bool) -> Mutability {
        match is_var {
            true => Mutability::Var,
            false => Mutability::Const,
        }
    }
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
    fn stmt_list(self, db: &dyn Db) -> SemanticLoc<ast::StmtList> {
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
