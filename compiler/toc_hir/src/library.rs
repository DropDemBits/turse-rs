//! Library structure definitions

use std::sync::Arc;

use indexmap::IndexMap;
use la_arena::{Arena, Idx};
use toc_span::FileId;

use crate::{
    body, item,
    span::{HasSpanTable, SpanTable, Spanned},
    symbol::{self, ResolutionMap},
    ty,
};

pub use crate::library_graph::LibraryId;

/// A reference to a library node in a specific library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InLibrary<T>(pub LibraryId, pub T);

impl<T> InLibrary<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> InLibrary<U> {
        InLibrary(self.0, f(self.1))
    }

    pub fn library(self) -> LibraryId {
        self.0
    }

    pub fn item(self) -> T {
        self.1
    }
}

pub trait WrapInLibrary: Copy {
    type Output: Copy;

    /// Wraps self in the context of a library
    fn in_library(self, library: LibraryId) -> InLibrary<Self::Output>;
}

impl<T> WrapInLibrary for T
where
    T: Copy,
{
    type Output = T;

    fn in_library(self, library: LibraryId) -> InLibrary<Self::Output> {
        InLibrary(library, self)
    }
}

/// A `Library` represents a logical collection of files.
///
/// It is a conceptual group of files / units that are accessible from a
/// specific root file. For example, all of the standard library files are
/// grouped together under one `Library`. Similarly, the file provided during
/// compilation serves as a root for a `Library`.
///
/// `Library` stores the arena for all library local entities
/// (`Item`, `Body`, `DefInfo`, and `Type`).
/// `Library` also stores a mapping between the library files, and the
/// associated root `ItemId`s.
#[derive(PartialEq, Eq)]
pub struct Library {
    /// Map between library files and root items
    pub root_items: IndexMap<FileId, item::ItemId>,
    /// Table of all interned spans
    pub(crate) span_map: SpanTable,
    /// Table of all interned types
    pub(crate) type_map: ty::TypeTable,
    pub(crate) items: Arena<item::Item>,
    pub(crate) defs: symbol::DefInfoTable,
    pub(crate) bodies: Arena<body::Body>,
    pub(crate) resolve_map: ResolutionMap,
}

impl std::fmt::Debug for Library {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Library")
            .field("root_items", &self.root_items)
            .finish_non_exhaustive()
    }
}

impl Library {
    pub fn body(&self, body_id: body::BodyId) -> &body::Body {
        &self.bodies[Idx::from(body_id)]
    }

    pub fn item(&self, item_id: item::ItemId) -> &item::Item {
        &self.items[Idx::from(item_id)]
    }

    pub fn module_item(&self, module_id: item::ModuleId) -> item::ModuleLike<'_> {
        match &self.item(module_id.item_id()).kind {
            item::ItemKind::Module(module) => item::ModuleLike::Module(module),
            _ => unreachable!("not a module-like"),
        }
    }

    pub fn local_def(&self, local_def: symbol::LocalDefId) -> &symbol::DefInfo {
        self.defs.get_info(local_def)
    }

    pub fn lookup_type(&self, type_id: ty::TypeId) -> &ty::Type {
        self.type_map.lookup_type(type_id)
    }

    pub fn binding_resolve(&self, binding: Spanned<symbol::Symbol>) -> symbol::Resolve {
        self.resolve_map
            .binding_resolves
            .get(&binding)
            .copied()
            .unwrap_or(symbol::Resolve::Err)
    }

    pub fn def_resolve(&self, local_def: symbol::LocalDefId) -> symbol::DefResolve {
        self.resolve_map
            .def_resolves
            .get(local_def)
            .copied()
            .unwrap_or(symbol::DefResolve::Canonical)
    }

    pub fn local_defs(&self) -> impl Iterator<Item = symbol::LocalDefId> + '_ {
        self.defs.iter().map(|(id, _)| id)
    }

    pub fn body_ids(&self) -> Vec<body::BodyId> {
        self.bodies
            .iter()
            .map(|(idx, _)| body::BodyId(idx))
            .collect()
    }
}

impl HasSpanTable for Library {
    fn span_table(&self) -> &SpanTable {
        &self.span_map
    }
}

/// Lowered [`Library`].
///
/// Data is wrapped inside of an [`Arc`], so it is trivially cloneable.
///
/// [`Library`]: crate::library::Library
pub type LoweredLibrary = Arc<Library>;
