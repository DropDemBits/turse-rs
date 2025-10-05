//! Scope-sets based name resolution.
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    hash::Hash,
    iter,
    marker::PhantomData,
    ops::Index,
};

/// Region within a specific [`Domain`] where bindings and queries can be scoped to.
pub trait Region: Ord + Hash {}

/// A specific intersection of [`Region`]s where a binding or query can live in.
pub trait Domain: PartialOrd + Eq + Hash {
    type Region: Region;

    /// Regions associated with this domain.
    fn regions(&self) -> impl Iterator<Item = &Self::Region>;
}

pub type ResolveResult<'r, D, B> = Result<ResolveEntry<'r, D, B>, ResolveError<'r, D, B>>;

/// Anything that allows resolving an identifier-domain pair to some binding.
pub trait Resolver<I, D, B>
where
    D: Domain,
{
    /// Resolves a single identifier
    fn resolve(&self, identifier: &I, domain: &D) -> ResolveResult<'_, D, B>;
}

const _: () = {
    #[allow(dead_code)]
    fn resolver_as_dyn<I, D: Domain, B>(_: &dyn Resolver<I, D, B>) {}
};

impl<I, D, B, R> Resolver<I, D, B> for &R
where
    R: Resolver<I, D, B>,
    D: Domain,
{
    fn resolve(&self, identifier: &I, domain: &D) -> ResolveResult<'_, D, B> {
        R::resolve(self, identifier, domain)
    }
}

/// Resolution result, referring to the resolved domain and binding.
#[derive(Debug)]
pub struct ResolveEntry<'res, D, B> {
    pub domain: &'res D,
    pub binding: B,
}

impl<'res, D, B> Clone for ResolveEntry<'res, D, B>
where
    B: Clone,
{
    fn clone(&self) -> Self {
        Self {
            domain: self.domain,
            binding: self.binding.clone(),
        }
    }
}

impl<'res, D, B> Copy for ResolveEntry<'res, D, B> where B: Copy {}

/// Error while resolving an identifier within a [`Resolver`].
#[derive(Debug, Clone)]
pub enum ResolveError<'res, D, B> {
    /// Multiple bindings are candidates for the given identifier.
    Ambiguous(Box<[ResolveEntry<'res, D, B>]>),
    /// No bindings were found for the given identifier.
    Unbound,
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

pub use scope::{Scope, ScopeGen};
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

    /// Resolves a single identifier-domain pair to a potential binding.
    pub fn resolve(&self, identifier: &I, domain: &D) -> ResolveResult<'_, D, B>
    where
        B: Clone,
    {
        let Some(domains) = self.bindings.get(identifier) else {
            return Err(ResolveError::Unbound);
        };

        // Find greatest-lower-bound of domain
        glb_domains(domain, domains.iter().map(|(k, v)| (k, v)))
    }

    /// Map resolved bindings into a different binding type.
    pub fn map_bindings<B2>(&self, f: impl Fn(B) -> B2) -> impl Resolver<I, D, B2>
    where
        B: Clone,
    {
        MapBinding::new(self, f)
    }
}

fn glb_domains<'i, 'r: 'i, D, B>(
    mut candidate_domain: &'i D,
    domains: impl Iterator<Item = (&'r D, &'i B)>,
) -> ResolveResult<'r, D, B>
where
    D: Domain,
    B: Clone + 'i,
{
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

        candidates.push(ResolveEntry {
            domain,
            binding: binding.clone(),
        });
    }

    match candidates.as_slice() {
        [] => Err(ResolveError::Unbound),
        [entry] => Ok(entry.clone()),
        [..] => Err(ResolveError::Ambiguous(candidates.into_boxed_slice())),
    }
}

impl<I, D, B> Resolver<I, D, B> for DomainBindings<I, D, B>
where
    I: Ord + salsa::Update,
    D: Domain + salsa::Update,
    B: Clone + salsa::Update,
{
    fn resolve(&self, identifier: &I, domain: &D) -> ResolveResult<'_, D, B> {
        self.resolve(identifier, domain)
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

    pub fn resolve_all<'r, B>(
        &self,
        resolvers: &[&'r (dyn Resolver<I, D, B> + 'r)],
    ) -> ResolveList<'r, I, D, B>
    where
        D: Domain,
        B: Clone,
    {
        /// An identifier can only move greater in the resolution ordering:
        ///
        /// Unbound < Resolved < Ambiguous
        ///
        /// Thus, for a best (row) and next (column) candidate:
        ///
        /// |           | Unbound   | Resolved  | Ambiguous |
        /// |-----------|-----------|-----------|-----------|
        /// | Unbound   | Unbound   | Resolved  | Ambiguous |
        /// | Resolved  | Resolved  | Ambiguous | Ambiguous |
        /// | Ambiguous | Ambiguous | Ambiguous | Ambiguous |
        ///
        fn pick_candidate<'r, D, B>(
            best_candidate: ResolveResult<'r, D, B>,
            next_candidate: ResolveResult<'r, D, B>,
        ) -> ResolveResult<'r, D, B>
        where
            D: Domain,
            B: Clone,
        {
            fn to_pair<'e, 'r: 'e, D, B>(entry: &'e ResolveEntry<'r, D, B>) -> (&'r D, &'e B) {
                (entry.domain, &entry.binding)
            }

            match (best_candidate, next_candidate) {
                // Prefer non-unbound over unbound errors
                (best, Err(ResolveError::Unbound)) => best,
                (Err(ResolveError::Unbound), next) => next,
                // Pick glb of the domains
                (Ok(best), Ok(next)) => glb_domains(
                    best.domain,
                    iter::once(to_pair(&best)).chain(iter::once(to_pair(&next))),
                ),
                (Ok(best), Err(ResolveError::Ambiguous(next_candidates))) => glb_domains(
                    best.domain,
                    iter::once((best.domain, &best.binding))
                        .chain(next_candidates.iter().map(to_pair)),
                ),
                (Err(ResolveError::Ambiguous(best_candidates)), Ok(next)) => glb_domains(
                    best_candidates[0].domain,
                    best_candidates
                        .iter()
                        .map(to_pair)
                        .chain(iter::once(to_pair(&next))),
                ),
                (
                    Err(ResolveError::Ambiguous(best_candidates)),
                    Err(ResolveError::Ambiguous(next_candidates)),
                ) => glb_domains(
                    best_candidates[0].domain,
                    best_candidates
                        .iter()
                        .map(to_pair)
                        .chain(next_candidates.iter().map(to_pair)),
                ),
            }
        }

        let mut list = ResolveList::default();

        for (key, (identifier, domain)) in self.queries() {
            let mut candidate = Err(ResolveError::Unbound);

            for &resolver in resolvers {
                let next_candidate = resolver.resolve(identifier, domain);
                candidate = pick_candidate(candidate, next_candidate);
            }

            match candidate {
                Ok(resolved) => {
                    list.resolved.insert(key, resolved);
                }
                Err(ResolveError::Unbound) => {
                    list.unresolved.push(key);
                }
                Err(ResolveError::Ambiguous(candidates)) => {
                    list.ambiguous.insert(key, candidates);
                }
            }
        }

        list
    }
}

pub struct ResolveList<'res, I, D, B> {
    /// Resolved identifers.
    pub resolved: HashMap<QueryKey<I, D>, ResolveEntry<'res, D, B>>,
    /// Unresolved identifiers.
    pub unresolved: Vec<QueryKey<I, D>>,
    /// Identifiers resolving to multiple resolutions.
    pub ambiguous: HashMap<QueryKey<I, D>, Box<[ResolveEntry<'res, D, B>]>>,
}

impl<'res, I, D, B> Default for ResolveList<'res, I, D, B> {
    fn default() -> Self {
        Self {
            resolved: Default::default(),
            unresolved: Default::default(),
            ambiguous: Default::default(),
        }
    }
}

pub struct MapBinding<'r, R, F, B1, B2> {
    resolver: &'r R,
    map: F,
    _binding: PhantomData<fn(B1) -> B2>,
}

impl<'r, R, F, B1, B2> MapBinding<'r, R, F, B1, B2> {
    fn new(resolver: &'r R, map: F) -> Self {
        Self {
            resolver,
            map,
            _binding: PhantomData,
        }
    }
}

impl<'r, I, D, B1, B2, R, F> Resolver<I, D, B2> for MapBinding<'r, R, F, B1, B2>
where
    R: Resolver<I, D, B1>,
    D: Domain + 'r,
    F: Fn(B1) -> B2,
{
    fn resolve(&self, identifier: &I, domain: &D) -> ResolveResult<'r, D, B2> {
        let result = self.resolver.resolve(identifier, domain);

        match result {
            Ok(resolved) => Ok(ResolveEntry {
                domain: resolved.domain,
                binding: (self.map)(resolved.binding),
            }),
            Err(ResolveError::Unbound) => Err(ResolveError::Unbound),
            Err(ResolveError::Ambiguous(candidates)) => Err(ResolveError::Ambiguous(
                candidates
                    .into_iter()
                    .map(|ResolveEntry { domain, binding }| ResolveEntry {
                        domain,
                        binding: (self.map)(binding),
                    })
                    .collect(),
            )),
        }
    }
}
