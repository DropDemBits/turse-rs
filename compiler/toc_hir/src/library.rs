//! Library structure definitions

use indexmap::IndexMap;
use la_arena::Arena;
use toc_span::{FileId, Span, SpanId, SpanTable};

use crate::{body, item, symbol, ty};

/// A reference to a library in the library graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(pub(crate) LibraryIndex);
pub(crate) type LibraryIndex = u32;

/// A `Library` represents a logical collection of files.
///
/// It is a conceptual group of files / units that are accesible from a
/// specific root file. For example, all of the standard library files are
/// grouped together under one `Library`. Similarly, the file provided during
/// compilation serves as a root for a `Library`.
///
/// `Library` stores the arena for all library local entities
/// (`Item`, `Body`, `DefInfo`, and `Type`).
/// `Library` also stores a mapping between the library files, and the
/// associated root `ItemId`s.
pub struct Library {
    /// Map between library files and root items
    pub root_items: IndexMap<FileId, item::ItemId>,
    /// Table of all interned spans
    pub span_map: SpanTable,
    /// Table of all interened types
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
}

/// Lowered [`Library`].
///
/// Data is wrapped inside of an [`Arc`], so it is trivially cloneable.
///
/// [`Library`]: crate::library::Library
pub type LoweredLibrary = std::sync::Arc<Library>;
/*#[derive(Debug, Clone)]
pub struct SpannedLibrary {
    res: Arc<(Library, SpanTable)>,
}

impl std::ops::Deref for SpannedLibrary {
    type Target = Library;

    fn deref(&self) -> &Self::Target {
        &self.res.0
    }
}

impl SpannedLibrary {
    pub fn new(library: Library, span: SpanTable) -> SpannedLibrary {
        Self {
            res: Arc::new((library, span)),
        }
    }

    pub fn library(&self) -> &Library {
        &self.res.0
    }

    pub fn span_map(&self) -> &SpanTable {
        &self.res.1
    }
}
*/
