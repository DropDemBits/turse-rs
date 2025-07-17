//! Common scoping types

use rustc_hash::FxHashMap;

use crate::{
    Symbol,
    body::BlockScope,
    item::{self, ItemScope},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update, salsa::Supertype)]
pub enum Scope<'db> {
    ItemScope(ItemScope<'db>),
    BlockScope(BlockScope<'db>),
}

impl<'db> std::fmt::Debug for Scope<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            // Pass through formatting for default items.
            match self {
                Self::ItemScope(arg0) => arg0.fmt(f),
                Self::BlockScope(arg0) => arg0.fmt(f),
            }
        } else {
            match self {
                Self::ItemScope(arg0) => f.debug_tuple("ItemScope").field(arg0).finish(),
                Self::BlockScope(arg0) => f.debug_tuple("BlockScope").field(arg0).finish(),
            }
        }
    }
}

impl<'db> scope_trees::Region for Scope<'db> {}

pub type ScopeSet<'db> = scope_trees::ScopeSet<Scope<'db>>;
pub type ItemBindings<'db> = scope_trees::DomainBindings<Symbol<'db>, ScopeSet<'db>, Binding<'db>>;
pub type BodyBindings<'db> = scope_trees::DomainBindings<Symbol<'db>, ScopeSet<'db>, Binding<'db>>;
pub type ScopeQueries<'db> = scope_trees::DomainQueries<Symbol<'db>, ScopeSet<'db>>;
pub type QueryKey<'db> = scope_trees::QueryKey<Symbol<'db>, ScopeSet<'db>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum Binding<'db, Local = core::convert::Infallible>
where
    Local: salsa::Update,
{
    Item(item::Item<'db>),
    Local(Local),
}

impl_into_conversions!(ItemScope, BlockScope for Scope<'db>);

pub(crate) fn try_resolve<'db, L: std::fmt::Debug + salsa::Update>(
    queries: &'db ScopeQueries<'db>,
    bindings: &'db scope_trees::DomainBindings<Symbol<'db>, ScopeSet<'db>, Binding<'db, L>>,
) -> (
    FxHashMap<QueryKey<'db>, &'db Binding<'db, L>>,
    Vec<QueryKey<'db>>,
    (),
) {
    let mut resolved = FxHashMap::default();
    let mut unresolved = vec![];

    for (key, (identifier, domain)) in queries.queries() {
        match bindings.resolve(identifier, domain) {
            Ok(entry) => _ = resolved.insert(key, entry.binding()),
            Err(scope_trees::ResolveError::Unbound) => unresolved.push(key),
            Err(scope_trees::ResolveError::Ambiguous(candidates)) => {
                todo!("err {identifier:?} candidates {candidates:#?}")
            }
        }
    }

    (resolved, unresolved, ())
}
