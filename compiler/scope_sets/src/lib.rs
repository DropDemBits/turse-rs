//! Simple Set of Scopes implementation, based on the [Binding as Sets of Scopes](scope-sets) paper.
//!
//! [scope-sets]: https://www.cs.utah.edu/plt/scope-sets/index.html

use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    fmt,
    hash::Hash,
    num::NonZeroU32,
};

/// Generates unique [`Scope`]s
#[derive(Debug)]
pub struct ScopeGen(NonZeroU32);

impl ScopeGen {
    pub fn new() -> Self {
        Self(NonZeroU32::new(1).unwrap())
    }

    pub fn gen(&mut self) -> Scope {
        let id = self.0;
        self.0 = self.0.checked_add(1).expect("generated too many scopes");
        Scope(id)
    }
}

impl Default for ScopeGen {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents the visibility  of one or more bindings
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Scope(NonZeroU32);

/// Set of visible scopes
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ScopeSet {
    scopes: BTreeSet<Scope>,
}

impl ScopeSet {
    /// Creates an empty set of scopes
    pub fn empty() -> Self {
        Self::default()
    }

    /// Add a scope to the set
    pub fn add(&mut self, scope: Scope) {
        self.scopes.insert(scope);
    }

    /// Removes a scope from the set
    pub fn remove(&mut self, scope: Scope) {
        self.scopes.remove(&scope);
    }

    /// Tests if this scope set has a scope
    pub fn has_scope(&self, scope: Scope) -> bool {
        self.scopes.contains(&scope)
    }

    /// Tests if this scope is a subset of another scope
    pub fn is_subset(&self, other: &Self) -> bool {
        self.scopes.is_subset(&other.scopes)
    }
}

impl From<&[Scope]> for ScopeSet {
    fn from(scope: &[Scope]) -> Self {
        Self {
            scopes: BTreeSet::from_iter(scope.iter().copied()),
        }
    }
}

impl<const N: usize> From<[Scope; N]> for ScopeSet {
    fn from(scope: [Scope; N]) -> Self {
        Self {
            scopes: BTreeSet::from_iter(scope.iter().copied()),
        }
    }
}

impl PartialOrd for ScopeSet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.scopes == other.scopes {
            Some(Ordering::Equal)
        } else if self.scopes.is_subset(&other.scopes) {
            Some(Ordering::Less)
        } else if self.scopes.is_superset(&other.scopes) {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}

pub trait BindingContext: PartialOrd + Eq {
    fn scopes(&self) -> &ScopeSet;
}

impl BindingContext for ScopeSet {
    fn scopes(&self) -> &ScopeSet {
        self
    }
}

pub struct BindingTable<Sym: Eq, Data, Ctx: BindingContext = ScopeSet> {
    // Since a binding can only be accessed by all of its scopes in a scope set,
    // we can reduce the search space by breaking up the table by a specific scope
    bind_tables: HashMap<Option<Scope>, Vec<(Sym, Ctx, Data)>>,
}

impl<Sym: Eq, Data, Ctx: BindingContext> Default for BindingTable<Sym, Data, Ctx> {
    fn default() -> Self {
        Self {
            bind_tables: Default::default(),
        }
    }
}

impl<Sym: Eq, Data, Ctx: BindingContext> fmt::Debug for BindingTable<Sym, Data, Ctx>
where
    Sym: fmt::Debug,
    Data: fmt::Debug,
    Ctx: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Resolver")
            .field("bind_tables", &self.bind_tables)
            .finish()
    }
}

impl<I: Hash + Eq, B, C: BindingContext> BindingTable<I, B, C> {
    pub fn new() -> Self {
        Default::default()
    }

    /// Binds `id` in the given `context` to `binding`
    pub fn bind(&mut self, id: I, context: C, binding: B) {
        // insert into the last scope, since that's most likely
        // the scope that most likely implies the binding
        // FIXME(rust-1.66): use BTreeSet::last
        let bind_table = self
            .bind_tables
            .entry(context.scopes().scopes.iter().last().copied())
            .or_default();

        bind_table.push((id, context, binding));
    }

    /// Resolves `id` in the context of `ctx`
    pub fn resolve(&self, id: &I, ctx: &C) -> Result<&B, ResolveError<'_, B, C>> {
        // include the empty scope if it isn't already there
        let maybe_empty_scope = if ctx.scopes().scopes.is_empty() {
            None
        } else {
            Some(None)
        };

        // go over the relevant scopes
        // start from last to first, since we're more likely to find it in there
        let relevant_scopes = ctx
            .scopes()
            .scopes
            .iter()
            .rev()
            .map(|sc| Some(*sc))
            .chain(maybe_empty_scope);

        // get the relevant binding tables
        let binding_tables = relevant_scopes
            .flat_map(|scope| self.bind_tables.get(&scope))
            .flatten();

        let all_subsets = binding_tables
            .filter_map(|(i, c, b)| {
                // only keep subsets of ctx & those with a matching identifier
                (i == id && c.partial_cmp(ctx).unwrap_or(Ordering::Equal).is_le()).then_some((c, b))
            })
            .collect::<Vec<_>>();

        // find the largest subset
        let Some(&(largest_subset, _)) = all_subsets.iter().max_by(|(lhs_ctx, _), (rhs_ctx, _)| {
            lhs_ctx.partial_cmp(rhs_ctx).unwrap_or(Ordering::Equal)
        }) else {
            // none found? it's an unbound identifier
            return Err(ResolveError::Unbound);
        };

        // keep if eq or disjoint
        let subsets = all_subsets
            .into_iter()
            .filter(|(c, _)| {
                c.partial_cmp(&largest_subset)
                    .unwrap_or(Ordering::Equal)
                    .is_eq()
            })
            .collect::<Vec<_>>();

        match &*subsets {
            [(_, binding)] => Ok(binding),
            [..] => Err(ResolveError::Ambiguous(subsets)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolveError<'a, B, C: BindingContext> {
    Unbound,
    Ambiguous(Vec<(&'a C, &'a B)>),
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    struct Binding(u32);
    type Resolver = super::BindingTable<String, Binding>;

    #[test]
    fn test_from_empty() {
        let mut scope_gen = ScopeGen::new();
        let mut resolver = Resolver::new();

        let a = scope_gen.gen();
        let main_scope = ScopeSet::from([a]);

        resolver.bind("a".into(), [].into(), Binding(0));

        assert_eq!(
            Ok(&Binding(0)),
            resolver.resolve(&"a".to_string(), &main_scope)
        );
    }

    #[test]
    fn test_sample() {
        let mut scope_gen = ScopeGen::new();
        let mut resolver = Resolver::new();

        // against this example:
        // (1) let x{a} = 1
        // (2) macro get_x{a,b} = x{a}
        // (3) let y{a,b} =
        // (4)   let x{a,b,c} = x{a,b} in
        // (5)     x{a,d}
        let a = scope_gen.gen();
        let b = scope_gen.gen();
        let c = scope_gen.gen();
        let d = scope_gen.gen();

        let outer_x = Binding(0);
        let get_x = Binding(1);
        let y = Binding(2);
        let expanded_x = Binding(3);

        resolver.bind("x".into(), [a].into(), outer_x);
        resolver.bind("get_x".into(), [a, b].into(), get_x);
        resolver.bind("y".into(), [a, b].into(), y);
        resolver.bind("x".into(), [a, b, c].into(), expanded_x);

        // resolve!
        // - in macro
        assert_eq!(
            Ok(&Binding(0)),
            resolver.resolve(&"x".to_string(), &ScopeSet::from([a])),
        );
        // - in body of y
        assert_eq!(
            Ok(&Binding(0)),
            resolver.resolve(&"x".to_string(), &ScopeSet::from([a, b])),
        );
        // - in body of macro
        assert_eq!(
            Ok(&Binding(0)),
            resolver.resolve(&"x".to_string(), &ScopeSet::from([a, d]))
        );
    }

    #[test]
    fn test_ambiguous_disjoint() {
        let mut scope_gen = ScopeGen::new();
        let mut resolver = Resolver::new();

        // for this example:
        // import A1 -- exports a
        // import A2 -- exports a
        // let _ = a

        let a1 = scope_gen.gen();
        let a2 = scope_gen.gen();

        resolver.bind("a".into(), [a1].into(), Binding(0));
        resolver.bind("a".into(), [a2].into(), Binding(1));

        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&ScopeSet::from([a2]), &Binding(1)),
                (&ScopeSet::from([a1]), &Binding(0)),
            ],)),
            resolver.resolve(&"a".to_string(), &ScopeSet::from([a1, a2]))
        );
    }

    #[test]
    fn test_ambiguous_duplicate() {
        let mut scope_gen = ScopeGen::new();
        let mut resolver = Resolver::new();

        let a = scope_gen.gen();

        resolver.bind("a".into(), [a].into(), Binding(0));
        resolver.bind("a".into(), [a].into(), Binding(1));

        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&ScopeSet::from([a]), &Binding(0)),
                (&ScopeSet::from([a]), &Binding(1))
            ],)),
            resolver.resolve(&"a".to_string(), &ScopeSet::from([a]))
        );
    }

    #[test]
    fn test_unbound_no_bindings() {
        let resolver = Resolver::new();

        assert_eq!(
            Err(ResolveError::Unbound),
            resolver.resolve(&"a".to_string(), &ScopeSet::empty())
        );
    }

    #[test]
    fn test_unbound_wrong_context() {
        let mut scope_gen = ScopeGen::default();
        let mut resolver = Resolver::new();

        resolver.bind("a".into(), [scope_gen.gen()].into(), Binding(0));

        assert_eq!(
            Err(ResolveError::Unbound),
            resolver.resolve(&"a".to_string(), &ScopeSet::empty())
        );
    }

    #[test]
    fn test_unbound_wrong_name() {
        let mut scope_gen = ScopeGen::default();
        let mut resolver = Resolver::new();

        let main_scope = ScopeSet::from([scope_gen.gen()]);

        resolver.bind("a".into(), main_scope.clone(), Binding(0));

        assert_eq!(
            Err(ResolveError::Unbound),
            resolver.resolve(&"b".to_string(), &main_scope)
        );
    }

    #[test]
    fn test_gradual_scoping() {
        // a b c => a
        // a _ c => a
        // a b _ => a
        // a b c => a

        // should be:
        // {} => {a _ c} \
        //                => {a b c}
        //    => {a b _} /
        //

        let mut scope_gen = ScopeGen::default();
        let mut resolver = Resolver::new();

        let a = scope_gen.gen();
        let b = scope_gen.gen();
        let c = scope_gen.gen();

        let base_scope: ScopeSet = [a, b, c].into();
        let mut derived_scope = base_scope.clone();
        derived_scope.remove(b);
        let mut rederived_base = derived_scope.clone();
        rederived_base.add(b);

        resolver.bind("a".into(), base_scope.clone(), Binding(0));
        resolver.bind("a".into(), derived_scope.clone(), Binding(1));
        resolver.bind("a".into(), rederived_base.clone(), Binding(2));

        // rederived_base & base scope should be equivalent

        // - from base
        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&base_scope, &Binding(0)),
                (&base_scope, &Binding(2))
            ])),
            resolver.resolve(&"a".to_string(), &base_scope)
        );

        // - from re-derived
        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&base_scope, &Binding(0)),
                (&base_scope, &Binding(2))
            ])),
            resolver.resolve(&"a".to_string(), &rederived_base)
        );

        assert_eq!(
            Ok(&Binding(1)),
            resolver.resolve(&"a".to_string(), &derived_scope)
        )
    }

    #[test]
    fn test_consistent_order() {
        let mut scope_gen = ScopeGen::default();
        let mut resolver = Resolver::new();

        let a = scope_gen.gen();
        let b = scope_gen.gen();
        let c = scope_gen.gen();

        let base_scope: ScopeSet = [a, b, c].into();
        let reverse_scope: ScopeSet = [c, b, a].into();
        let swapped_scope: ScopeSet = [a, c, b].into();

        resolver.bind("a".into(), base_scope.clone(), Binding(0));
        resolver.bind("a".into(), swapped_scope.clone(), Binding(1));
        resolver.bind("a".into(), reverse_scope.clone(), Binding(2));

        // base scope, swapped scope, and reversed scope should be equivalent

        // - from base
        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&base_scope, &Binding(0)),
                (&base_scope, &Binding(1)),
                (&base_scope, &Binding(2))
            ])),
            resolver.resolve(&"a".to_string(), &base_scope)
        );

        // - from swapped
        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&base_scope, &Binding(0)),
                (&base_scope, &Binding(1)),
                (&base_scope, &Binding(2))
            ])),
            resolver.resolve(&"a".to_string(), &swapped_scope)
        );

        // - from reversed
        assert_eq!(
            Err(ResolveError::Ambiguous(vec![
                (&base_scope, &Binding(0)),
                (&base_scope, &Binding(1)),
                (&base_scope, &Binding(2))
            ])),
            resolver.resolve(&"a".to_string(), &reverse_scope)
        );
    }
}
