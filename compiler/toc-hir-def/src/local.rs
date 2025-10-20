//! Local-variable stuff

use toc_salsa_collections::arena::SalsaArenaMap;

use crate::Symbol;

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to a local variable.
    ///
    /// [`Body`]: crate::body::Body
    pub struct LocalId<'db>(Local<'db>);
    /// Alias for the local arena index
    pub(crate) type LocalIndex<'db> = Index;
);

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Local<'db> {
    pub name: Symbol<'db>,
}

pub type LocalMap<'db, V> = SalsaArenaMap<LocalIndex<'db>, V>;
