//! Crate containing all of the HIR node representations
//!
//! All `expr`, `stmt`, `item`, `ty`, and `body` entities are to be used with
//! the module's prefix, e.g. `expr::Name` instead of importing the node
//! directly.
//!
//! While all HIR entities implement `PartialEq` and `Eq`, they are only used
//! to see if the bit representation actually changed, which may diverge from
//! the actual equality semantics (e.g. see [`expr::Literal`] for such a case).

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

pub(crate) mod ids;

pub mod builder;
pub mod span;

pub mod item;
pub mod package;
pub mod package_graph;
pub mod symbol;

pub mod body;
pub mod expr;
pub mod stmt;
pub mod ty;
pub mod visitor;

pub mod expand {
    //! Expansion of source files, as well as referring to specific source and semantic locations

    // We have a SemanticLoc, which is a SemanticSource + SourceLoc,
    // A SourceLoc is a semi-stable reference to a node in a SourceFile,
    // a SourceFile can have notable locations, which is a list of a:
    //
    // ```rust
    // enum NotableLocation {
    //     ConstVar(SourceLoc<ast::ConstVar>, SourceLoc<ast::Name>),
    //     Type(SourceLoc<ast::Type>, SourceLoc<ast::Name>),
    //     // etc...
    //     Include(SourceLoc<ast::Include>),
    // }
    // ```
    //
    // Note that both `SourceLoc` and `SemanticLoc` aren't stable ways to refer to semantic items,
    // since semantic item definitions get dedicated salsa entities
    //

    use std::marker::PhantomData;

    use toc_ast_db::IntoAst;
    use toc_syntax::{
        ast::{self, AstNode},
        SyntaxNode, SyntaxNodePtr,
    };
    use toc_vfs_db::SourceFile;
    use upcast::{Upcast, UpcastFrom};

    pub trait Db:
        salsa::DbWithJar<Jar>
        + toc_ast_db::Db
        + toc_source_graph::Db
        + Upcast<dyn toc_ast_db::Db>
        + Upcast<dyn toc_source_graph::Db>
    {
    }

    impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
        fn up_from(value: &DB) -> &Self {
            value
        }
        fn up_from_mut(value: &mut DB) -> &mut Self {
            value
        }
    }

    #[salsa::jar(db = Db)]
    pub struct Jar(ErasedSemanticLoc, SemanticFile, SemanticFile_ast_locations);

    /// An untyped reference to a location in a [`SemanticFile`]
    #[salsa::tracked(jar = Jar)]
    pub(crate) struct ErasedSemanticLoc {
        pub(crate) file: SemanticFile,
        pub(crate) ptr: SyntaxNodePtr,
    }

    /// A semi-stable reference to an AST node in a semantic file
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct SemanticLoc<T: AstNode> {
        loc: ErasedSemanticLoc,
        _node: PhantomData<T>,
    }

    impl<T: AstNode> Clone for SemanticLoc<T> {
        fn clone(&self) -> Self {
            Self {
                loc: self.loc,
                _node: PhantomData,
            }
        }
    }

    impl<T: AstNode> Copy for SemanticLoc<T> {}

    impl<T: AstNode<Language = toc_syntax::Lang>> SemanticLoc<T> {
        /// Yields the underlying AST node at this location
        pub fn to_node(self, db: &dyn Db) -> T {
            let ast = self.loc.file(db).ast(db);
            let node = self.loc.ptr(db).to_node(&ast);
            T::cast(node).unwrap()
        }

        /// Gets which [`SemanticFile`] this location comes from
        pub fn file(self, db: &dyn Db) -> SemanticFile {
            self.loc.file(db)
        }

        /// Projects from `T` into `U`.
        ///
        /// `U`'s node must have a corresponding semantic location in the file
        pub fn map<U: AstNode<Language = toc_syntax::Lang>>(
            self,
            db: &dyn Db,
            f: impl FnOnce(T) -> U,
        ) -> SemanticLoc<U> {
            let t = self.to_node(db);
            let u = f(t);
            self.file(db).ast_locations(db).get(&u)
        }

        /// Projects from `T` into `U`, without worrying about location stability.
        ///
        /// `U`'s node does not need to have a corresponding semantic location in the file,
        /// but this comes with the caveat that this location is unstable
        pub fn map_unstable<U: AstNode<Language = toc_syntax::Lang>>(
            self,
            db: &dyn Db,
            f: impl FnOnce(T) -> U,
        ) -> UnstableSemanticLoc<U> {
            let t = self.to_node(db);
            let u = f(t);
            UnstableSemanticLoc::new(self.file(db), &u)
        }
    }

    /// An unstable reference to an AST node in a semantic file
    ///
    /// Can refer to any node, but must be referred to stabily
    /// by another node.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct UnstableSemanticLoc<T: AstNode> {
        file: SemanticFile,
        ptr: SyntaxNodePtr,
        _node: PhantomData<T>,
    }

    impl<T: AstNode> Clone for UnstableSemanticLoc<T> {
        fn clone(&self) -> Self {
            Self {
                file: self.file,
                ptr: self.ptr.clone(),
                _node: PhantomData,
            }
        }
    }

    impl<T: AstNode<Language = toc_syntax::Lang>> UnstableSemanticLoc<T> {
        /// Creates a new unstable location pointer
        ///
        /// `file` must be the actual [`SemanticFile`] that contains `node`
        pub fn new(file: SemanticFile, node: &T) -> Self {
            Self {
                file,
                ptr: SyntaxNodePtr::new(node.syntax()),
                _node: PhantomData,
            }
        }

        /// Yields the underlying AST node at this location
        pub fn to_node(&self, db: &dyn Db) -> T {
            let ast = self.file.ast(db);
            let node = self.ptr.to_node(&ast);
            T::cast(node).unwrap()
        }

        /// Gets which [`SemanticFile`] this unstable location comes from
        pub fn file(&self) -> SemanticFile {
            self.file
        }

        /// Projects from `T` into `U`.
        pub fn map<U: AstNode<Language = toc_syntax::Lang>>(
            self,
            db: &dyn Db,
            f: impl FnOnce(T) -> U,
        ) -> UnstableSemanticLoc<U> {
            let t = self.to_node(db);
            let u = f(t);
            UnstableSemanticLoc::new(self.file, &u)
        }
    }

    /// An untyped reference to a syntax node in a semantic file.
    ///
    /// Primarily used for diagnostic reporting
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct SemanticNodePtr {
        file: SemanticFile,
        ptr: SyntaxNodePtr,
    }

    impl<T: AstNode> From<UnstableSemanticLoc<T>> for SemanticNodePtr {
        fn from(value: UnstableSemanticLoc<T>) -> Self {
            Self {
                file: value.file,
                ptr: value.ptr,
            }
        }
    }

    /// Semantic representation of a source file
    ///
    /// This can either point to a real file, or an expansion of an include directive.
    /// Even though a file may be included multiple times in a file, we want to still
    /// consider them distinct for name resolution purposes (i.e. we want them to collide).
    //
    // Why interned? Since we want to go from any SourceFile -> SemanticFile
    //
    #[salsa::interned(jar = Jar)]
    pub struct SemanticFile {
        pub origin: SemanticSource,
    }

    impl SemanticFile {
        pub fn from_source_file(db: &dyn Db, source: SourceFile) -> Self {
            Self::new(db, SemanticSource::SourceFile(source))
        }
    }

    impl IntoAst for SemanticFile {
        type Db<'db> = dyn Db + 'db;
        fn ast(self, db: &Self::Db<'_>) -> SyntaxNode {
            match self.origin(db) {
                SemanticSource::SourceFile(source) => {
                    toc_ast_db::parse_file(db.up(), source).result().syntax()
                }
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum SemanticSource {
        SourceFile(SourceFile),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct AstLocations {
        to_locations: rustc_hash::FxHashMap<SyntaxNodePtr, ErasedSemanticLoc>,
    }

    impl AstLocations {
        /// Looks up the node's stable location
        pub fn get<N: AstNode<Language = toc_syntax::Lang>>(&self, node: &N) -> SemanticLoc<N> {
            let ptr = SyntaxNodePtr::new(node.syntax());
            let loc = *self
                .to_locations
                .get(&ptr)
                .expect("can't find node in AstLocations map");

            SemanticLoc {
                loc,
                _node: PhantomData,
            }
        }
    }

    #[salsa::tracked(jar = Jar)]
    impl SemanticFile {
        #[salsa::tracked(jar = Jar, return_ref)]
        pub fn ast_locations(self, db: &dyn Db) -> AstLocations {
            let root = self.ast(db);
            let mut locs = vec![];

            sparse_bfs(&root, |node| {
                let kind = node.kind();

                if ast::Item::can_cast(kind)
                    || ast::Name::can_cast(kind)
                    || ast::StmtList::can_cast(kind)
                    || ast::UnionVariant::can_cast(kind)
                    || ast::RecordField::can_cast(kind)
                {
                    locs.push(SyntaxNodePtr::new(&node));
                    true
                } else {
                    false
                }
            });

            let to_locations = locs
                .into_iter()
                .map(|ptr| (ptr.clone(), ErasedSemanticLoc::new(db, self, ptr)))
                .collect();

            AstLocations { to_locations }
        }
    }

    /// Walks the subtree in sparse bfs order, calling `filter` for each node.
    ///
    /// Sparse bfs order is like bfs order, except that nodes that are in the
    /// same layer in sparse bfs order aren't necessarily at the same depth.
    ///
    /// Nodes for which `filter` return true are put in the same layer, and
    /// the children of those nodes form the candidates for the next layer.
    /// All other nodes are explored depth-first.
    ///
    /// The size of the bfs queue is bound by the number of "true" nodes.
    fn sparse_bfs(node: &SyntaxNode, mut filter: impl FnMut(SyntaxNode) -> bool) {
        // borrowed from:
        // https://github.com/rust-lang/rust-analyzer/blob/fc848495f45e4741849940a2be437a46b742ce53/crates/hir-expand/src/ast_id_map.rs#L126
        let mut curr_layer = vec![node.clone()];
        let mut next_layer = vec![];
        while !curr_layer.is_empty() {
            curr_layer.drain(..).for_each(|node| {
                let mut preorder = node.preorder();
                while let Some(event) = preorder.next() {
                    match event {
                        toc_syntax::WalkEvent::Enter(node) => {
                            if filter(node.clone()) {
                                next_layer.extend(node.children());
                                preorder.skip_subtree();
                            }
                        }
                        toc_syntax::WalkEvent::Leave(_) => {}
                    }
                }
            });
            std::mem::swap(&mut curr_layer, &mut next_layer);
        }
    }
}

pub mod def {
    //! Items and code bodies
    use toc_ast_db::IntoAst;
    use toc_source_graph::Package;
    use toc_syntax::ast::{self, AstNode};
    use toc_vfs_db::SourceFile;
    use upcast::{Upcast, UpcastFrom};

    use crate::expand::{self, SemanticFile, SemanticLoc};

    mod lower {
        //! Lowering from AST nodes to semantic representations

        use toc_syntax::ast;

        use crate::expand::SemanticLoc;

        use super::{Db, Item, Module, ModuleOrigin};

        /// Collects the immediately accessible items from a [`ast::StmtList`]
        pub(crate) fn collect_items(
            db: &dyn Db,
            stmt_list: SemanticLoc<ast::StmtList>,
        ) -> Vec<Item> {
            let ast_locations = stmt_list.file(db.up()).ast_locations(db.up());
            let stmt_list = stmt_list.to_node(db.up());

            stmt_list
                .stmts()
                .filter_map(|stmt| {
                    Some(match stmt {
                        // ast::Stmt::ConstVarDecl(_) => todo!(),
                        // ast::Stmt::TypeDecl(_) => todo!(),
                        // ast::Stmt::BindDecl(_) => todo!(),
                        // ast::Stmt::ProcDecl(_) => todo!(),
                        // ast::Stmt::FcnDecl(_) => todo!(),
                        // ast::Stmt::ProcessDecl(_) => todo!(),
                        // ast::Stmt::ExternalDecl(_) => todo!(),
                        // ast::Stmt::ForwardDecl(_) => todo!(),
                        // ast::Stmt::DeferredDecl(_) => todo!(),
                        // ast::Stmt::BodyDecl(_) => todo!(),
                        ast::Stmt::ModuleDecl(module) => Item::Module(Module::new(
                            db,
                            ModuleOrigin::Item(ast_locations.get(&module)),
                        )),
                        // ast::Stmt::ClassDecl(_) => todo!(),
                        // ast::Stmt::MonitorDecl(_) => todo!(),
                        // ast::Stmt::ImportStmt(_) => todo!(),
                        // ast::Stmt::ExportStmt(_) => todo!(),
                        // ast::Stmt::PreprocGlob(_) => todo!(),
                        _ => return None,
                    })
                })
                .collect()
        }
    }

    pub trait Db: salsa::DbWithJar<Jar> + expand::Db + Upcast<dyn expand::Db> {}

    impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
        fn up_from(value: &DB) -> &Self {
            value
        }
        fn up_from_mut(value: &mut DB) -> &mut Self {
            value
        }
    }

    #[salsa::jar(db = Db)]
    pub struct Jar(Module, root_module, Body);

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Item {
        Module(Module),
    }

    #[salsa::tracked(jar = Jar)]
    pub struct Module {
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

    #[salsa::tracked(jar = Jar)]
    pub fn root_module(db: &dyn Db, package: Package) -> Module {
        Module::new(db, ModuleOrigin::Root(package))
    }

    impl Module {
        /// All immediate items of a module
        ///
        /// Note: This does not include items that are in the top level but
        /// are hidden inside of scopes
        pub fn items(self, db: &dyn Db) -> Vec<Item> {
            lower::collect_items(db, self.stmt_list(db))
        }

        /// Executable portion of a module
        pub fn body(self, db: &dyn Db) -> Body {
            Body::new(db, BodyOrigin::StmtList(self.stmt_list(db)))
        }

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

    #[salsa::tracked(jar = Jar)]
    pub struct Body {
        origin: BodyOrigin,
    }

    // either: attached to stmtlist, or something else?
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum BodyOrigin {
        StmtList(SemanticLoc<ast::StmtList>),
    }

    // Lowered body contents
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(crate) struct BodyData {}
}

/// Helper trait equivalent to `Option::map_or(predicate)`
pub trait OrMissingExt<T> {
    fn is_missing_or(&self, is_predicate: impl FnOnce(T) -> bool) -> bool;
}

impl<T> OrMissingExt<T> for Option<T>
where
    T: Copy,
{
    fn is_missing_or(&self, is_predicate: impl FnOnce(T) -> bool) -> bool {
        self.map_or(true, is_predicate)
    }
}
