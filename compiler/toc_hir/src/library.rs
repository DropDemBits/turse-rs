//! Library structure definitions

use indexmap::IndexMap;
use la_arena::Arena;
use toc_span::FileId;

use crate::{body, item, symbol};

/// A reference to a library in the library graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(LibraryIndex);
pub(crate) type LibraryIndex = u32;

/// A `Library` represents a logical collection of files.
///
/// It is a conceptual group of files / units that are accesible from a
/// specific root file. For example, all of the standard library files are
/// grouped together under one `Library`. Similarly, the file provided during
/// compilation serves as a root for a `Library`.
///
/// `Library` stores the arena for all library local entities
/// (`Item`, `Body`, and `DefInfo`).
/// `Library` also stores a mapping between the library files, and the
/// associated root `ItemId`s.
pub struct Library {
    /// Map between library files and root items
    pub root_items: IndexMap<FileId, item::ItemId>,
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
}
