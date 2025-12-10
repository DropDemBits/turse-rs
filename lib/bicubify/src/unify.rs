//! Implementation of a biunification algorithm for relating between
//! type variables and type values (specific instances of a type).
//!
//! [`BiunificationTable`] forms the core of the biunification algorithm,
//! responsible for both storing type variables & values, as well as relating
//! between them. In order for type variables to participate in biunification it
//! must implement [`UnifyKey`] and have an associated value type.
//!
//! In order to maintain good average case performance in the face of invariant
//! relates (i.e. bounding a variable with the same value on both the upper and
//! lower sides), we instead choose to break invariant cycles by:
//!
//! - only detecting cycles when a variable is bounded on the other side with
//!   the same value, and
//! - preventing invariant bounds from emitting new relate edges.
//!
//! It is expected that the invariant bounds of a variable must be inspected
//! after initial constraint solving, as invariant bounds necessarily force a
//! variable into one specific value.

use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use either::Either;
use ordermap::OrderSet;

use crate::unify::{
    bounds::{Bounds, Polarity},
    index::{BoundsState, InferNode, InferValue, InferVar},
};

mod bounds;
mod index;

/// Used by anything that can be a type variable. This can be thought of as a
/// superset of [`ena::UnifyKey`](ena::unify::UnifyKey), but instead with the
/// intent of using this with [`BiunificationTable`].
///
/// Because biunification uses the dedicated `lub_var` and `glb_var` methods,
/// keys can only be unified if their value bounds are equal or empty, and
/// there is no ordering of values outside of unification.
pub trait UnifyKey: Copy + Clone + Debug + PartialEq {
    type Value: Clone + Eq;

    /// Converts the unification key into a raw `u32` to work with.
    fn index(self) -> u32;

    /// Creates a unfication key from a raw `u32`.
    ///
    /// ## Note
    ///
    /// The top bit is always guaranteed to be constant so it can be used as a
    /// bit niche as long as the original bit value is preserved when returning
    /// it via [`index`].
    fn from_index(u: u32) -> Self;

    // Name used for debugging and logging purposes.
    fn tag() -> &'static str;
}

/// Skeleton needed to generate fresh type variables e.g. during constraint
/// collection. This is a lighter-weight type to make it cheaper to store in
/// something like a `salsa` tracked struct.
#[derive(derive_more::Debug, Clone, PartialEq, Eq, Hash)]
pub struct BiunificationVariables<K> {
    next: u32,
    _key: PhantomData<K>,
}

impl<K> BiunificationVariables<K>
where
    K: UnifyKey,
{
    /// Gets the number of variables allocated so far.
    pub fn len(&self) -> usize {
        self.next as usize
    }

    /// Allocates a fresh variable reference.
    pub fn fresh_var(&mut self) -> K {
        let var = InferVar::new(self.next);
        self.next += 1;
        var.to_key()
    }

    /// Clones the table for use during constraint solving, allowing for variable and value relates.
    pub fn clone_for_solving(&self) -> BiunificationTable<K> {
        let mut unify = ena::unify::InPlaceUnificationTable::new();

        // Add keys added so far.
        unify.reserve(self.len());
        for _ in 0..self.len() {
            let _ = unify.new_key(BoundsState::Empty);
        }

        BiunificationTable {
            unify,
            bounds: Bounds::new_up_to(self.len()),
            ..Default::default()
        }
    }
}

// FIXME(perfect-derive): Subsume when perfect-derive is implementable
impl<K> Default for BiunificationVariables<K> {
    fn default() -> Self {
        Self {
            next: Default::default(),
            _key: Default::default(),
        }
    }
}

/// Compact representation of a solution to a [`BiunificationTable`].
/// This is a lighter-weight type to make it cheaper to store in something like
/// a `salsa` tracked struct.
#[derive(derive_more::Debug, Clone, PartialEq, Eq)]
pub struct BiunificationSolution<K: UnifyKey> {
    root_vars: Box<[InferVar]>,
    values: Box<[K::Value]>,
    bounds: Bounds,
}

// FIXME(perfect-derive): Subsume when perfect-derive is implementable
impl<K: UnifyKey> Default for BiunificationSolution<K> {
    fn default() -> Self {
        Self {
            root_vars: Default::default(),
            values: Default::default(),
            bounds: Default::default(),
        }
    }
}

impl<K: UnifyKey> BiunificationSolution<K> {
    /// Gets the number of variables allocated so far.
    pub fn len(&self) -> usize {
        self.root_vars.len()
    }

    /// Returns an iterator over all of the inference variables.
    pub fn vars(&self) -> impl Iterator<Item = K> + use<'_, K> {
        (0..self.root_vars.len()).map(|index| InferVar::new(index as u32).to_key())
    }

    /// Finds the root or representative variable of `var`.
    pub fn root_var(&self, var: K) -> K {
        let var = InferVar::from_key(var);
        let var = self.root_vars[var.to_node().index()];

        var.to_key()
    }

    /// Gets the value bounds of the given variable.
    pub fn get_var_bounds(&self, var: K) -> VarBounds<'_, K> {
        let var = InferVar::from_key(var);
        let var = self.root_vars[var.to_node().index()];

        VarBounds {
            values: Either::Right(&self.values),
            bounds: self.bounds.bounds(var),
        }
    }
}

/// Core of the biunification algorithm. This permits unifying type variables in
/// a polar manner, representing flows from the upper bounds of a type variable
/// to its lower bounds.
///
/// See the [module documentation](crate::unify) for more information.
#[derive(derive_more::Debug, Clone)]
pub struct BiunificationTable<K: UnifyKey> {
    unify: ena::unify::InPlaceUnificationTable<InferVar>,
    values: InternTable<K::Value>,
    bounds: Bounds,
    // Ensures that we'll only emit new relates, instead of previously emitted ones.
    emitted_relates: OrderSet<(InferValue, InferValue)>,
}

// FIXME(perfect-derive): Subsume when perfect-derive is implementable
impl<K> Default for BiunificationTable<K>
where
    K: UnifyKey,
{
    fn default() -> Self {
        Self {
            unify: Default::default(),
            bounds: Default::default(),
            values: Default::default(),
            emitted_relates: Default::default(),
        }
    }
}

/// Non-value based functions.
impl<K> BiunificationTable<K>
where
    K: UnifyKey,
{
    /// Gets the number of variables allocated so far.
    pub fn len(&self) -> usize {
        self.unify.len()
    }

    /// Returns an iterator over all of the inference variables allocated so far.
    pub fn vars(&self) -> impl Iterator<Item = K> + use<'_, K> {
        (0..self.unify.len()).map(|index| InferVar::new(index as u32).to_key())
    }

    /// Allocates a fresh variable reference.
    pub fn fresh_var(&mut self) -> K {
        let var = self.unify.new_key(BoundsState::Empty);
        self.bounds.reserve_one();
        var.to_key()
    }

    /// Compacts the biunification table into an immutable [`BiunificationSolution`].
    pub fn into_solution(self) -> BiunificationSolution<K> {
        let Self {
            mut unify,
            values,
            mut bounds,
            emitted_relates: _,
        } = self;
        assert!(
            unify.len() < InferNode::MAX_INDEX,
            "too many unification keys"
        );
        assert!(
            values.values.len() < InferNode::MAX_INDEX,
            "too many interned values"
        );

        let root_vars: Box<[_]> = (0..unify.len())
            .into_iter()
            .map(|index| {
                let var = InferVar::new(index as u32);
                unify.find(var)
            })
            .collect();

        let values: Box<[_]> = values.values.iter().cloned().collect();

        bounds.shrink_to_fit();

        BiunificationSolution {
            root_vars,
            values,
            bounds,
        }
    }

    /// Finds the root or representative variable of `var`.
    pub fn root_var(&mut self, var: K) -> K {
        self.root_key(var).to_key()
    }

    /// Shrinks the capacity of the biunification table as much as possible.
    pub fn shrink_to_fit(&mut self) {
        self.values.values.shrink_to_fit();
        self.bounds.shrink_to_fit();
    }

    /// Unions two variables, assuming both or either variables have not been bounded.
    /// If both variables have been bounded, this fails.
    pub fn shallow_unify_vars(&mut self, left: K, right: K) -> Result<(), ()> {
        let root_left = self.root_key(left);
        let root_right = self.root_key(right);

        if root_left == root_right {
            return Ok(());
        }

        // We can only unify root_left and root_right if the corresponding bounds are non-empty...
        let left_empty = self.get_emptiness(root_left)?;
        let right_empty = self.get_emptiness(root_right)?;

        // ...or if they're the exact same
        if matches!(
            (left_empty, right_empty),
            (BoundsState::NonEmpty, BoundsState::NonEmpty)
        ) && self.bounds.bounds(root_left) != self.bounds.bounds(root_right)
        {
            return Err(());
        }

        // Perform a unification attempt
        self.unify.unify_var_var(root_left, root_right)
    }

    /// Gets the latest bound fullness state from the bounds list.
    ///
    /// Invariant: var must be a root variable.
    fn get_emptiness(&mut self, var: InferVar) -> Result<BoundsState, ()> {
        let Some(state) = self.unify.try_probe_value(var) else {
            panic!(
                "variable {tag}({index}) is not a root variable",
                tag = K::tag(),
                index = var.to_node().index()
            );
        };

        if matches!(state, BoundsState::NonEmpty) {
            // In non-empty state. Because we can only append terms, this is the final state.
            return Ok(*state);
        }

        if !self.bounds.bounds(var).is_empty() {
            self.unify.unify_var_value(var, BoundsState::NonEmpty)?;
        }

        Ok(self.unify.probe_value(var))
    }

    fn root_key(&mut self, var: K) -> InferVar {
        self.unify.find(InferVar::from_key(var))
    }
}

/// Variable-variable and variable-value unification functions.
impl<K> BiunificationTable<K>
where
    K: UnifyKey,
    // rust-analyzer oopsie: Hash in type position should include ty namespace
    K::Value: Hash,
{
    /// Gets the value bounds of the given variable.
    ///
    /// This must only be used on root variables, use [`root_var`](Self::root_var) to get to it.
    pub fn get_var_bounds(&self, var: K) -> VarBounds<'_, K> {
        let var = InferVar::from_key(var);

        let Some(_) = self.unify.try_probe_value(var) else {
            panic!(
                "variable {tag}({index}) is not a root variable",
                tag = K::tag(),
                index = var.to_node().index()
            );
        };

        VarBounds {
            values: Either::Left(&self.values),
            bounds: self.bounds.bounds(var),
        }
    }

    /// Performs a meet + join/lub + glb between two variables.
    ///
    /// `relates` emits newly discovered pairs from any elements in the lower or upper bounds to of each variable.
    pub fn full_unify_var_var(
        &mut self,
        left: K,
        right: K,
        mut relates: impl FnMut(Either<LowerBound<'_, K>, UpperBound<'_, K>>),
    ) {
        // Use the left var as the representative var
        let root = self.root_key(left);
        let redirect = self.root_key(right);

        if root == redirect {
            // Bounds are already unified
            return;
        }

        let [root_bounds, redirect_bounds] = self.bounds.bounds_disjoint_pair_mut([root, redirect]);

        let mut redirect_bounds = redirect_bounds.take();

        // Only keep unique values (we'll have already related them in the root's side).
        redirect_bounds
            .upper
            .retain(|value| !root_bounds.upper.contains(value));
        redirect_bounds
            .invariant
            .retain(|value, _| !root_bounds.invariant.contains_key(value));
        redirect_bounds
            .lower
            .retain(|value| !root_bounds.lower.contains(value));

        // Split out invariant upper bounds...
        {
            let old_invariant = redirect_bounds.invariant.len();

            redirect_bounds.invariant.extend(
                redirect_bounds
                    .upper
                    .intersection(root_bounds.lower)
                    .map(|value| (*value, Polarity::Upper)),
            );

            if old_invariant != redirect_bounds.invariant.len() {
                redirect_bounds
                    .upper
                    .retain(|value| !redirect_bounds.invariant.contains_key(value))
            }
        }

        // ...and lower invariant bounds
        {
            let old_invariant = redirect_bounds.invariant.len();

            redirect_bounds.invariant.extend(
                redirect_bounds
                    .lower
                    .intersection(root_bounds.upper)
                    .map(|value| (*value, Polarity::Lower)),
            );

            // If new invariant bounds were discovered, remove them from the originating bounds list
            if old_invariant != redirect_bounds.invariant.len() {
                redirect_bounds
                    .lower
                    .retain(|value| !redirect_bounds.invariant.contains_key(value))
            }
        }

        {
            let redirect_upper_bounds = redirect_bounds.upper.iter().copied().chain(
                redirect_bounds
                    .invariant
                    .iter()
                    .flat_map(|(value, polarity)| (polarity == &Polarity::Upper).then_some(*value)),
            );

            let redirect_lower_bounds = redirect_bounds.lower.iter().copied().chain(
                redirect_bounds
                    .invariant
                    .iter()
                    .flat_map(|(value, polarity)| (polarity == &Polarity::Lower).then_some(*value)),
            );

            // Perform lubs first...
            for redirect_upper in redirect_upper_bounds {
                let right = redirect_upper;
                let redirect_upper = self.values.lookup(redirect_upper);

                for &lower in root_bounds.lower.iter() {
                    if !self.emitted_relates.insert_full((lower, right)).1 {
                        continue;
                    }

                    let lower = self.values.lookup(lower);

                    relates(Either::Left(LowerBound {
                        lower,
                        term: redirect_upper,
                    }));
                }
            }

            // ...then glbs afterwards
            for redirect_lower in redirect_lower_bounds {
                let left = redirect_lower;
                let redirect_lower = self.values.lookup(redirect_lower);

                for &upper in root_bounds.upper.iter() {
                    if !self.emitted_relates.insert_full((left, upper)).1 {
                        continue;
                    }

                    let upper = self.values.lookup(upper);

                    relates(Either::Right(UpperBound {
                        term: redirect_lower,
                        upper,
                    }));
                }
            }
        }

        // Since we're guaranteed to not observe either the upper or lower
        // bounds of a type variable during a join, we can defer extending the
        // bounds here so as to not perform excessively duplicate relates (since
        // by construction, the redirect bounds would have already been fully
        // saturated).
        root_bounds.upper.extend(redirect_bounds.upper.into_iter());
        root_bounds
            .invariant
            .extend(redirect_bounds.invariant.into_iter());
        root_bounds.lower.extend(redirect_bounds.lower.into_iter());

        // Preserve fast-eq term ordering invariant
        root_bounds.upper.sort_unstable();
        root_bounds.invariant.sort_unstable_keys();
        root_bounds.lower.sort_unstable();

        // Unification makes root & redirect equivalent, unify in the var table as well.
        let try_unify = self.unify.unify_var_var(root, redirect);
        debug_assert_eq!(try_unify, Ok(()));
    }

    /// Performs a meet/lub between a variable and a value.
    ///
    /// `relates` emits newly discovered pairs from any elements in the lower bounds to `value`.
    pub fn lub_var(&mut self, var: K, value: K::Value, mut relates: impl FnMut(LowerBound<'_, K>)) {
        let var = self.root_key(var);
        let value_index = self.values.intern(value);

        let var_bounds = self.bounds.bounds_mut(var);

        if var_bounds.invariant.contains_key(&value_index) {
            // Already covered by a prior bounding call
            return;
        } else if var_bounds.lower.contains(&value_index) {
            // In the lower bounds, graduate to an invariant value bound
            var_bounds.lower.remove(&value_index);
            var_bounds.lower.sort_unstable();
            var_bounds
                .invariant
                .insert_sorted(value_index, Polarity::Upper);

            // We don't need to issue any relates since they've been made during glb_var
            return;
        } else {
            // Preserve fast-eq term ordering invariant
            var_bounds.upper.insert_sorted(value_index);
        }

        // Emit new relates
        let value = self.values.lookup(value_index);
        for &lower in var_bounds.lower.iter() {
            if !self.emitted_relates.insert_full((lower, value_index)).1 {
                continue;
            }

            let lower = self.values.lookup(lower);
            relates(LowerBound { lower, term: value })
        }
    }

    /// Performs a glb/join between a value and a variable.
    ///
    /// `relates` emits newly discovered pairs from `value` to any elements in the upper bounds.
    pub fn glb_var(&mut self, value: K::Value, var: K, mut relates: impl FnMut(UpperBound<'_, K>)) {
        let var = self.root_key(var);
        let value_index = self.values.intern(value);

        let var_bounds = self.bounds.bounds_mut(var);

        if var_bounds.invariant.contains_key(&value_index) {
            // Already covered by a prior bounding call
            return;
        } else if var_bounds.upper.contains(&value_index) {
            // In the lower bounds, graduate to an invariant value bound
            var_bounds.upper.remove(&value_index);
            var_bounds.upper.sort_unstable();
            var_bounds
                .invariant
                .insert_sorted(value_index, Polarity::Lower);

            // We don't need to issue any relates since they've been made during glb_var
            return;
        } else {
            // Preserve fast-eq term ordering invariant
            var_bounds.lower.insert_sorted(value_index);
        }

        // Emit new relates
        let value = self.values.lookup(value_index);
        for &upper in var_bounds.upper.iter() {
            if !self.emitted_relates.insert_full((value_index, upper)).1 {
                continue;
            }

            let upper = self.values.lookup(upper);
            relates(UpperBound { term: value, upper })
        }
    }
}

/// Represents a relation between a lower bound and a given term.
#[derive(Debug, Clone, Copy)]
pub struct LowerBound<'u, K: UnifyKey> {
    pub lower: &'u K::Value,
    pub term: &'u K::Value,
}

/// Represents a relation between a given term and an upper bound.
#[derive(Debug, Clone, Copy)]
pub struct UpperBound<'u, K: UnifyKey> {
    pub term: &'u K::Value,
    pub upper: &'u K::Value,
}

/// The upper, lower, and invariant bounds of a variable.
#[derive(derive_more::Debug, Clone, Copy)]
pub struct VarBounds<'u, K: UnifyKey> {
    values: Either<&'u InternTable<K::Value>, &'u [K::Value]>,
    bounds: bounds::VarBoundsRef<'u>,
}

impl<'u, K: UnifyKey> VarBounds<'u, K>
where
    K::Value: Eq + Hash,
{
    /// Gets the upper bounds of the given variable.
    pub fn upper_bounds(&self) -> impl Iterator<Item = &'_ K::Value> + use<'_, K> {
        self.bounds
            .upper
            .iter()
            .map(|value_index| self.lookup_value(*value_index))
    }

    /// Gets the lower bounds of the given variable.
    pub fn lower_bounds(&self) -> impl Iterator<Item = &'_ K::Value> + use<'_, K> {
        self.bounds
            .lower
            .iter()
            .map(|value_index| self.lookup_value(*value_index))
    }

    /// Gets the invariant bounds of the given variable.
    pub fn invariant_bounds(&self) -> impl Iterator<Item = &'_ K::Value> + use<'_, K> {
        self.bounds
            .invariant
            .iter()
            .map(|(value_index, _)| self.lookup_value(*value_index))
    }

    fn lookup_value(&self, value: InferValue) -> &'u K::Value {
        match self.values {
            Either::Left(intern) => intern.lookup(value),
            Either::Right(slice) => &slice[value.to_node().index()],
        }
    }
}

#[derive(Debug, Clone)]
struct InternTable<V> {
    values: OrderSet<V>,
}

// FIXME(perfect-derive): Subsume when perfect-derive is implementable
impl<V> Default for InternTable<V> {
    fn default() -> Self {
        Self {
            values: Default::default(),
        }
    }
}

impl<V> InternTable<V>
where
    V: Eq + Hash,
{
    fn intern(&mut self, value: V) -> InferValue {
        let (index, _exists) = self.values.insert_full(value);
        InferValue::from_index(index)
    }

    fn lookup(&self, value: InferValue) -> &V {
        &self.values[value.to_node().index()]
    }
}

#[cfg(test)]
mod tests;
