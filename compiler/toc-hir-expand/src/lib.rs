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

use std::{collections::BTreeMap, marker::PhantomData};

use toc_ast_db::IntoAst;
use toc_source_graph::Package;
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

impl<DB> Db for DB where
    DB: salsa::DbWithJar<Jar>
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
pub struct Jar(
    all_files,
    ErasedSemanticLoc,
    PackageFile,
    SemanticFile,
    SemanticFile_ast_locations,
);

/// An untyped reference to a location in a [`SemanticFile`]
#[salsa::tracked]
pub struct ErasedSemanticLoc {
    pub(crate) file: SemanticFile,
    pub(crate) ptr: SyntaxNodePtr,
}

/// A semi-stable reference to an AST node in a semantic file
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SemanticLoc<T: AstNode> {
    loc: ErasedSemanticLoc,
    // `to_node` is what yields the actual AST node, rather than a `SemanticLoc`
    // owning the AST node. This also allows `SemanticLoc` to be `Send + Sync`.
    _node: PhantomData<fn() -> T>,
}

impl<T: AstNode> Clone for SemanticLoc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: AstNode> Copy for SemanticLoc<T> {}

impl<T: AstNode<Language = toc_syntax::Lang>> SemanticLoc<T> {
    /// Yields the underlying AST node at this location.
    pub fn to_node(self, db: &dyn Db) -> T {
        let ast = self.loc.file(db).ast(db);
        let node = self.loc.ptr(db).to_node(&ast);
        T::cast(node).unwrap()
    }

    /// Gets which [`SemanticFile`] this location comes from
    pub fn file(self, db: &dyn Db) -> SemanticFile {
        self.loc.file(db)
    }

    /// Yields the erased version of the location.
    ///
    /// Primarily used so that maps can use `SemanticLocs` as a key,
    /// without having to worry about matching the exact node type.
    pub fn into_erased(self) -> ErasedSemanticLoc {
        self.loc
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

    /// Equivalent to `Into<SemanticLoc<U>> for SemanticLoc<T>`, since
    /// actually implementing the trait resulting in overlapping implementations.
    pub fn into<U: AstNode<Language = toc_syntax::Lang> + From<T>>(self) -> SemanticLoc<U> {
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
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct UnstableSemanticLoc<T: AstNode> {
    file: SemanticFile,
    ptr: SyntaxNodePtr,
    // `to_node` is what yields the actual AST node, rather than an
    // `UnstableSemanticLoc` owning the AST node. This also allows
    // `UnstableSemanticLoc` to be `Send + Sync`.
    _node: PhantomData<fn() -> T>,
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
#[salsa::interned]
pub struct SemanticFile {
    pub origin: SemanticSource,
}

impl SemanticFile {
    pub fn from_package_file(db: &dyn Db, package: Package, file: SourceFile) -> Self {
        Self::new(
            db,
            SemanticSource::PackageFile(PackageFile::new(db, package, file)),
        )
    }
}

impl IntoAst for SemanticFile {
    type Db<'db> = dyn Db + 'db;
    fn ast(self, db: &Self::Db<'_>) -> SyntaxNode {
        match self.origin(db) {
            SemanticSource::PackageFile(file) => toc_ast_db::parse_file(db.up(), file.source(db))
                .result()
                .syntax(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticSource {
    PackageFile(PackageFile),
}

/// A [`SourceFile`] in a specific [`Package`].
///
/// Typically, a [`SourceFile`] tends to be part of one package only.
/// However, including a single source file from multiple packages results in the source file
/// being shared by multiple packages.
#[salsa::interned]
pub struct PackageFile {
    pub package: Package,
    pub source: SourceFile,
}

/// All files that are part of a specific package
#[salsa::tracked(return_ref)]
pub fn all_files(db: &dyn Db, package: Package) -> BTreeMap<SourceFile, PackageFile> {
    let source = toc_vfs_db::source_of(db.up(), package.root(db.up()));
    let mut files = (*toc_ast_db::reachable_files(db.up(), source)).clone();
    // `reachable_files` right now excludes the root from the reachable files list
    files.insert(source);

    files
        .into_iter()
        .map(|source| (source, PackageFile::new(db, package, source)))
        .collect::<BTreeMap<_, _>>()
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

#[salsa::tracked]
impl SemanticFile {
    #[salsa::tracked(return_ref)]
    pub fn ast_locations(self, db: &dyn Db) -> AstLocations {
        let root = self.ast(db);
        let mut locs = vec![];

        topo_visit(&root, |node| {
            let kind = node.kind();

            if ast::Item::can_cast(kind)
                || ast::ConstVarDecl::can_cast(kind)
                || ast::StmtList::can_cast(kind)
                || ast::UnionVariant::can_cast(kind)
                || ast::RecordFieldName::can_cast(kind)
                || ast::RecordField::can_cast(kind)
                || ast::EnumVariant::can_cast(kind)
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

/// Walks the subtree in topological order, calling `filter` for each node.
///
/// Nodes for which `filter` return true are put in the same layer, and
/// the children of those nodes form the candidates for the next layer.
/// All other nodes are explored depth-first.
///
/// The size of the expand queue is bound by the number of filtered nodes.
fn topo_visit(node: &SyntaxNode, mut filter: impl FnMut(SyntaxNode) -> bool) {
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

/// Maps from the semantic entity back to the original ast node
pub trait HasSource {
    type Db<'db>: Db + ?Sized + 'db;
    type Ast;

    fn as_ast(&self, db: &Self::Db<'_>) -> Self::Ast;
}
