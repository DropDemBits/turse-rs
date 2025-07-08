//! Scope-sets based name resolution.
use std::{cmp::Ordering, collections::BTreeMap, hash::Hash, marker::PhantomData, ops::Index};

/// Region within a specific [`Domain`] where bindings and queries can be scoped to.
pub trait Region: Ord + Hash {}

/// A specific intersection of [`Region`]s where a binding or query can live in.
pub trait Domain: PartialOrd + Eq + Hash {
    type Region: Region;

    /// Regions associated with this domain.
    fn regions(&self) -> impl Iterator<Item = &Self::Region>;
}

pub mod scope {
    use std::num::NonZeroU32;

    use crate::Region;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa_macros::Update)]
    #[repr(transparent)]
    pub struct Scope(NonZeroU32);

    impl Region for Scope {}

    /// Generator of [`Scope`] entities.
    ///
    /// Keys between scope generators are not considered distinct.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    pub struct ScopeGen(NonZeroU32);

    impl Default for ScopeGen {
        fn default() -> Self {
            const ONE: NonZeroU32 = NonZeroU32::new(1).unwrap();
            Self(ONE)
        }
    }

    impl ScopeGen {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn next_scope(&mut self) -> Scope {
            let next = self.0;
            self.0 = self.0.saturating_add(1);
            Scope(next)
        }
    }
}

pub use scope::{Scope, ScopeGen};

pub mod scope_set {
    use std::{cmp::Ordering, collections::BTreeSet};

    use crate::{Domain, Region};

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa_macros::Update)]
    pub struct ScopeSet<S: Region + salsa::Update> {
        scopes: BTreeSet<S>,
    }

    impl<S: Region + salsa::Update + std::fmt::Debug> std::fmt::Debug for ScopeSet<S> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if f.alternate() {
                write!(f, "Scopes[")?;
                for (pos, scope) in self.scopes.iter().enumerate() {
                    if pos > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{scope:#?}")?;
                }
                write!(f, "]")
            } else {
                f.debug_struct("ScopeSet")
                    .field("scopes", &self.scopes)
                    .finish()
            }
        }
    }

    impl<S: Region + salsa::Update> Default for ScopeSet<S> {
        fn default() -> Self {
            Self::empty()
        }
    }

    impl<S> ScopeSet<S>
    where
        S: Region + salsa::Update,
    {
        pub fn empty() -> Self {
            Self {
                scopes: Default::default(),
            }
        }

        pub fn add_scope(&mut self, scope: S)
        where
            S: Ord,
        {
            self.scopes.insert(scope);
        }

        pub fn remove_scope(&mut self, scope: &S) -> bool
        where
            S: Ord,
        {
            self.scopes.remove(scope)
        }

        pub fn is_lt(&self, other: &ScopeSet<S>) -> bool {
            self.partial_cmp(other).is_some_and(Ordering::is_lt)
        }

        pub fn is_gt(&self, other: &ScopeSet<S>) -> bool {
            self.partial_cmp(other).is_some_and(Ordering::is_gt)
        }
    }

    impl<S> Domain for ScopeSet<S>
    where
        S: Region + salsa::Update,
    {
        type Region = S;

        fn regions(&self) -> impl Iterator<Item = &Self::Region> {
            self.scopes.iter()
        }
    }
}

pub use scope_set::ScopeSet;

/// Identifier to name bindings within a particular [`Domain`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa_macros::Update)]
pub struct DomainBindings<I, D, B>
where
    I: Ord + salsa::Update,
    D: salsa::Update,
    B: salsa::Update,
{
    bindings: BTreeMap<I, Vec<(D, B)>>,
    // desired indexes
    // - ident -> domain + binding
    //   speeding up name resolutions
    // - domain -> ident + domain + binding
    //   finding within specific domains
    //   can do with querying each identifier
}

impl<I, D, B> Default for DomainBindings<I, D, B>
where
    I: Ord + salsa::Update,
    D: salsa::Update,
    B: salsa::Update,
{
    fn default() -> Self {
        Self {
            bindings: Default::default(),
        }
    }
}

impl<I, D, B> DomainBindings<I, D, B>
where
    I: Ord + salsa::Update,
    D: Domain + salsa::Update,
    B: salsa::Update,
{
    pub fn new() -> Self {
        Self::default()
    }

    /// Associates an identifer in a given domain with the provided binding information.
    pub fn add_binding(&mut self, identifier: I, domain: D, binding: B) {
        let domains = self.bindings.entry(identifier).or_default();
        domains.push((domain, binding));
    }

    pub fn resolve(
        &self,
        identifier: &I,
        domain: &D,
    ) -> Result<ResolveEntry<'_, D, B>, ResolveError<'_, D, B>> {
        let Some(domains) = self.bindings.get(identifier) else {
            return Err(ResolveError::Unbound);
        };

        // Find greatest-lower-bound of domain
        let mut candidate_domain = domain;
        let mut candidates = Vec::with_capacity(1);

        for (domain, binding) in domains {
            // cases:
            // domain < candidate: accept, in a weaker-bound scope
            // domain = candidate: accept, in same scope
            // domain > candidate: reject, in a tighter-bound scope
            // domain <> candidate: accept, in a sibling scope
            if domain
                .partial_cmp(candidate_domain)
                .is_some_and(Ordering::is_gt)
            {
                // Ignore tighter-bound scopes.
                continue;
            }

            if domain.regions().count() > candidate_domain.regions().count() {
                // Take candidate domain as the larger scope.
                // ???: This feels like a union-find?
                candidate_domain = domain;
                candidates.clear();
            }

            candidates.push(ResolveEntry { domain, binding });
        }

        match candidates.as_slice() {
            [] => Err(ResolveError::Unbound),
            [entry] => Ok(*entry),
            [..] => Err(ResolveError::Ambiguous(candidates.into_boxed_slice())),
        }
    }
}

/// Aggregation of a set of name resolution queries to resolve.
#[derive(Debug, Clone, PartialEq, Eq, salsa_macros::Update)]
pub struct DomainQueries<I, D>
where
    I: Eq + Hash + salsa::Update,
    D: Eq + Hash + salsa::Update,
{
    queries: indexmap::IndexSet<(I, D)>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
#[repr(transparent)]
pub struct QueryKey<I, D>(usize, PhantomData<fn() -> (I, D)>);

impl<I, D> Clone for QueryKey<I, D> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I, D> Copy for QueryKey<I, D> {}

impl<I, D> Index<QueryKey<I, D>> for DomainQueries<I, D>
where
    I: Eq + Hash + salsa::Update,
    D: Eq + Hash + salsa::Update,
{
    type Output = (I, D);

    fn index(&self, index: QueryKey<I, D>) -> &Self::Output {
        self.queries.get_index(index.0).unwrap()
    }
}

impl<I, D> Default for DomainQueries<I, D>
where
    I: Eq + Hash + salsa::Update,
    D: Eq + Hash + salsa::Update,
{
    fn default() -> Self {
        Self {
            queries: Default::default(),
        }
    }
}
impl<I, D> DomainQueries<I, D>
where
    I: Eq + Hash + salsa::Update,
    D: Eq + Hash + salsa::Update,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_query(&mut self, identifier: I, domain: D) -> QueryKey<I, D> {
        let (index, _) = self.queries.insert_full((identifier, domain));
        QueryKey(index, PhantomData)
    }

    pub fn queries(&self) -> impl Iterator<Item = (QueryKey<I, D>, (&I, &D))> {
        self.queries
            .iter()
            .enumerate()
            .map(|(key, value)| (QueryKey(key, PhantomData), (&value.0, &value.1)))
    }
}

#[derive(Debug)]
pub struct ResolveEntry<'res, D, B> {
    domain: &'res D,
    binding: &'res B,
}

impl<'res, D, B> Clone for ResolveEntry<'res, D, B> {
    fn clone(&self) -> Self {
        Self {
            domain: self.domain,
            binding: self.binding,
        }
    }
}

impl<'res, D, B> Copy for ResolveEntry<'res, D, B> {}

impl<'res, D, B> ResolveEntry<'res, D, B> {
    pub fn domain(&self) -> &D {
        &self.domain
    }

    pub fn binding(&self) -> &B {
        &self.binding
    }
}

/// Error while resolving an identifier within a [`DomainBindings`].
pub enum ResolveError<'res, D, B> {
    /// Multiple bindings are candidates for the given identifier.
    Ambiguous(Box<[ResolveEntry<'res, D, B>]>),
    /// No bindings were found for the given identifier.
    Unbound,
}
