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

use std::{collections::BTreeSet, marker::PhantomData};

use toc_ast_db::{
    IntoAst, SourceFileExt,
    ast_id::{AstId, AstIdMap, AstIdNode, ErasedAstId},
};
use toc_source_graph::Package;
use toc_span::TextRange;
use toc_syntax::{SyntaxNode, SyntaxNodePtr, ast::AstNode};
use toc_vfs_db::SourceFile;

#[salsa::db]
pub trait Db: toc_ast_db::Db + toc_source_graph::Db {}

#[salsa::db]
impl<DB> Db for DB where DB: toc_ast_db::Db + toc_source_graph::Db {}

/// A type-erased reference to a stable [`ErasedAstId`] location in a [`SemanticFile`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct ErasedSemanticLoc<'db> {
    // Note: in order to keep consistency with the visibilities while also
    // allowing external access to the semantic file (as salsa generates the
    // getter visibility to match with the visibility of the field), this field
    // is named `in_file` rather than `file` like in `UnstableSemanticLoc` so
    // that we can expose a pub method named `file`.
    pub(crate) in_file: SemanticFile<'db>,
    pub(crate) ast_id: ErasedAstId,
}

impl<'db> ErasedSemanticLoc<'db> {
    pub fn new(in_file: SemanticFile<'db>, ast_id: ErasedAstId) -> Self {
        Self { in_file, ast_id }
    }

    /// Yields the underlying syntax node at this location.
    pub fn to_node(self, db: &'db dyn Db) -> toc_syntax::SyntaxNode {
        let root = self.file().ast(db);
        self.file()
            .ast_id_map(db)
            .get_for_erased(self.ast_id)
            .to_node(&root)
    }

    /// Gets which [`SemanticFile`] this erased location comes from
    pub fn file(self) -> SemanticFile<'db> {
        self.in_file
    }

    /// Gets which [`ErasedAstId`] this erased location is associated with
    pub fn ast_id(self) -> ErasedAstId {
        self.ast_id
    }
}

/// A semi-stable reference to an AST node in a semantic file
#[derive(PartialEq, Eq, Hash, salsa::Update)]
pub struct SemanticLoc<'db, T: AstNode> {
    loc: ErasedSemanticLoc<'db>,
    // `to_node` is what yields the actual AST node, rather than a `SemanticLoc`
    // owning the AST node. This also allows `SemanticLoc` to be `Send + Sync`.
    _node: PhantomData<fn() -> T>,
}

impl<'db, T: AstNode> std::fmt::Debug for SemanticLoc<'db, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SemanticLoc")
            .field("loc", &self.loc)
            .finish()
    }
}

impl<T: AstNode> Clone for SemanticLoc<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: AstNode> Copy for SemanticLoc<'_, T> {}

impl<'db, T: AstNode<Language = toc_syntax::Lang>> SemanticLoc<'db, T> {
    pub fn new(in_file: SemanticFile<'db>, ast_id: AstId<T>) -> Self {
        let loc = ErasedSemanticLoc::new(in_file, ast_id.erased());

        Self {
            loc,
            _node: PhantomData,
        }
    }

    /// Yields the underlying AST node at this location.
    pub fn to_node(self, db: &dyn Db) -> T {
        let node = self.loc.to_node(db);
        T::cast(node).unwrap()
    }

    /// Gets which [`SemanticFile`] this location comes from
    pub fn file(self) -> SemanticFile<'db> {
        self.loc.file()
    }

    /// Yields the erased version of the location.
    ///
    /// Primarily used so that maps can use `SemanticLocs` as a key,
    /// without having to worry about matching the exact node type.
    pub fn into_erased(self) -> ErasedSemanticLoc<'db> {
        self.loc
    }

    /// Converts into an equivalent [`UnstableSemanticLoc<T>`] node.
    pub fn into_unstable(self, db: &'db dyn Db) -> UnstableSemanticLoc<'db, T> {
        let file = self.file();
        let ast_id_map = file.ast_id_map(db);
        let ast_id = self.loc.ast_id;
        let ptr = ast_id_map.get_for_erased(ast_id);

        UnstableSemanticLoc {
            file,
            ptr,
            _node: PhantomData,
        }
    }

    /// Projects from `T` into `U`.
    ///
    /// `U`'s node must have a corresponding semantic location in the file
    pub fn map<U: AstIdNode>(self, db: &'db dyn Db, f: impl FnOnce(T) -> U) -> SemanticLoc<'db, U> {
        let t = self.to_node(db);
        let u = f(t);
        let file = self.file();
        SemanticLoc::new(file, file.ast_id_map(db).lookup(&u))
    }

    /// Fallibly projects from `T` into `U`.
    ///
    /// `U`'s node must have a corresponding semantic location in the file
    pub fn try_map<U: AstIdNode>(
        self,
        db: &'db dyn Db,
        f: impl FnOnce(T) -> Option<U>,
    ) -> Option<SemanticLoc<'db, U>> {
        let t = self.to_node(db);
        let u = f(t)?;
        let file = self.file();
        Some(SemanticLoc::new(file, file.ast_id_map(db).lookup(&u)))
    }

    /// Projects from `T` into `U`, without worrying about location stability.
    ///
    /// `U`'s node does not need to have a corresponding semantic location in the file,
    /// but this comes with the caveat that this location is unstable
    pub fn map_unstable<U: AstNode<Language = toc_syntax::Lang>>(
        self,
        db: &'db dyn Db,
        f: impl FnOnce(T) -> U,
    ) -> UnstableSemanticLoc<'db, U> {
        let t = self.to_node(db);
        let u = f(t);
        UnstableSemanticLoc::new(self.file(), &u)
    }

    /// Fallibly Projects from `T` into `U`, without worrying about location stability.
    ///
    /// `U`'s node does not need to have a corresponding semantic location in the file,
    /// but this comes with the caveat that this location is unstable
    pub fn try_map_unstable<U: AstNode<Language = toc_syntax::Lang>>(
        self,
        db: &'db dyn Db,
        f: impl FnOnce(T) -> Option<U>,
    ) -> Option<UnstableSemanticLoc<'db, U>> {
        let t = self.to_node(db);
        let u = f(t)?;
        Some(UnstableSemanticLoc::new(self.file(), &u))
    }

    /// Equivalent to `Into<SemanticLoc<U>> for SemanticLoc<T>`, since
    /// actually implementing the trait resulting in overlapping implementations.
    pub fn into<U: AstNode<Language = toc_syntax::Lang> + From<T>>(self) -> SemanticLoc<'db, U> {
        SemanticLoc::from(self)
    }

    /// Equivalent to `From<SemanticLoc<T>> for SemanticLoc<U>`, since
    /// acutally implementing the trait resulting in overlapping implementations.
    pub fn from<U>(value: SemanticLoc<T>) -> SemanticLoc<U>
    where
        U: AstNode<Language = toc_syntax::Lang> + From<T>,
    {
        // The underlying location doesn't change, since we're just changing what it's
        // wrapped by
        SemanticLoc {
            loc: value.loc,
            _node: PhantomData,
        }
    }
}

/// An unstable reference to an AST node in a semantic file
///
/// Can refer to any node, but must be referred to stabily
/// by another node.
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct UnstableSemanticLoc<'db, T: AstNode> {
    file: SemanticFile<'db>,
    ptr: SyntaxNodePtr,
    // `to_node` is what yields the actual AST node, rather than an
    // `UnstableSemanticLoc` owning the AST node. This also allows
    // `UnstableSemanticLoc` to be `Send + Sync`.
    _node: PhantomData<fn() -> T>,
}

impl<'db, T: AstNode> Clone for UnstableSemanticLoc<'db, T> {
    fn clone(&self) -> Self {
        Self {
            file: self.file,
            ptr: self.ptr,
            _node: PhantomData,
        }
    }
}

impl<'db, T: AstNode<Language = toc_syntax::Lang>> UnstableSemanticLoc<'db, T> {
    /// Creates a new unstable location pointer
    ///
    /// `file` must be the actual [`SemanticFile`] that contains `node`
    pub fn new(file: SemanticFile<'db>, node: &T) -> Self {
        Self {
            file,
            ptr: SyntaxNodePtr::new(node.syntax()),
            _node: PhantomData,
        }
    }

    /// Yields the underlying AST node at this location
    pub fn to_node(&self, db: &'db dyn Db) -> T {
        let ast = self.file.ast(db);
        let node = self.ptr.to_node(&ast);
        T::cast(node).unwrap()
    }

    /// Gets which [`SemanticFile`] this unstable location comes from
    pub fn file(&self) -> SemanticFile<'db> {
        self.file
    }

    /// Gets the absolute span within the originating [`SemanticFile`].
    pub fn absolute_text_range(&self) -> TextRange {
        self.ptr.text_range()
    }

    /// Projects from `T` into `U`.
    pub fn map<U: AstNode<Language = toc_syntax::Lang>>(
        self,
        db: &dyn Db,
        f: impl FnOnce(T) -> U,
    ) -> UnstableSemanticLoc<'db, U> {
        let t = self.to_node(db);
        let u = f(t);
        UnstableSemanticLoc::new(self.file, &u)
    }

    /// Fallibly projects from `T` into `U`.
    pub fn try_map<U: AstNode<Language = toc_syntax::Lang>>(
        self,
        db: &'db dyn Db,
        f: impl FnOnce(T) -> Option<U>,
    ) -> Option<UnstableSemanticLoc<'db, U>> {
        let t = self.to_node(db);
        let u = f(t)?;
        Some(UnstableSemanticLoc::new(self.file, &u))
    }
}

/// An untyped reference to a syntax node in a semantic file.
///
/// Primarily used for diagnostic reporting
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct SemanticNodePtr<'db> {
    file: SemanticFile<'db>,
    ptr: SyntaxNodePtr,
}

impl<'db, T: AstNode> From<UnstableSemanticLoc<'db, T>> for SemanticNodePtr<'db> {
    fn from(value: UnstableSemanticLoc<'db, T>) -> Self {
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
#[salsa::interned(debug)]
pub struct SemanticFile<'db> {
    pub origin: SemanticSource,
}

impl<'db> SemanticFile<'db> {
    pub fn from_source_file(db: &'db dyn Db, file: SourceFile) -> Self {
        Self::new(db, SemanticSource::SourceFile(file))
    }
}

impl<'db> IntoAst for SemanticFile<'db> {
    type Db = dyn Db;
    fn ast(self, db: &Self::Db) -> SyntaxNode {
        match self.origin(db) {
            SemanticSource::SourceFile(file) => toc_ast_db::parse_file(db, file).result().syntax(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum SemanticSource {
    SourceFile(SourceFile),
}

/// All files that are part of a specific package
#[salsa::tracked(returns(ref))]
pub fn all_files(db: &dyn Db, package: Package) -> BTreeSet<SourceFile> {
    let source = toc_vfs_db::source_of(db, package.root(db).raw_path(db));
    let mut files = (*toc_ast_db::reachable_files(db, source)).clone();
    // `reachable_files` right now excludes the root from the reachable files list
    files.insert(source);

    files
}

#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct AstLocations<'db> {
    to_locations: rustc_hash::FxHashMap<NodePtrWrapper, ErasedSemanticLoc<'db>>,
}

/// Wrapper to ensure that `SyntaxNodePtr` can be updated through salsa::Update.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
#[repr(transparent)]
struct NodePtrWrapper(SyntaxNodePtr);

impl std::borrow::Borrow<SyntaxNodePtr> for NodePtrWrapper {
    fn borrow(&self) -> &SyntaxNodePtr {
        &self.0
    }
}

impl<'db> AstLocations<'db> {
    /// Looks up the node's stable location
    pub fn get<N: AstNode<Language = toc_syntax::Lang>>(&self, node: &N) -> SemanticLoc<'db, N> {
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

#[salsa::tracked]
impl<'db> SemanticFile<'db> {
    pub fn ast_id_map(self, db: &'db dyn Db) -> &'db AstIdMap {
        match self.origin(db) {
            SemanticSource::SourceFile(source_file) => source_file.ast_id_map(db),
        }
    }
}

/// Maps from the semantic entity back to the original ast node
pub trait HasSource<'db> {
    type Db: Db + ?Sized;
    type Ast;

    fn as_ast(&self, db: &'db Self::Db) -> Self::Ast;
}
