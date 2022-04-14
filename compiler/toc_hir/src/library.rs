//! Library structure definitions

use std::sync::Arc;

use indexmap::IndexMap;
use la_arena::Arena;
use toc_span::{FileId, HasSpanTable, Span, SpanId, SpanTable};

use crate::{body, item, symbol, ty};

pub use crate::ids::LibraryId;

/// A reference to a library node in a specific library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InLibrary<T>(pub LibraryId, pub T);

impl<T> InLibrary<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> InLibrary<U> {
        InLibrary(self.0, f(self.1))
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
    pub(crate) defs: Arena<symbol::DefInfo>,
    pub(crate) bodies: Arena<body::Body>,
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
        &self.bodies[body_id.into()]
    }

    pub fn item(&self, item_id: item::ItemId) -> &item::Item {
        &self.items[item_id.into()]
    }

    pub fn local_def(&self, def_id: symbol::LocalDefId) -> &symbol::DefInfo {
        &self.defs[def_id.into()]
    }

    pub fn lookup_type(&self, type_id: ty::TypeId) -> &ty::Type {
        self.type_map.lookup_type(type_id)
    }

    pub fn lookup_span(&self, span_id: SpanId) -> Span {
        self.span_map.lookup_span(span_id)
    }

    pub fn local_defs(&self) -> impl Iterator<Item = symbol::LocalDefId> + '_ {
        self.defs.iter().map(|(id, _)| symbol::LocalDefId(id))
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
