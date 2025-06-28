//! Random useful collection types, which may or may not support salsa::Update

use indexmap::{IndexMap, IndexSet};

pub type FxIndexMap<K, V> = IndexMap<K, V, rustc_hash::FxBuildHasher>;
pub type FxIndexSet<V> = IndexSet<V, rustc_hash::FxBuildHasher>;

pub mod arena {
    use std::ops::{Deref, DerefMut};

    use la_arena::{Arena, ArenaMap, Idx};

    /// Wrapper around [`la_arena::Arena`] that implements [`salsa::Update`].
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    pub struct SalsaArena<V>(Arena<V>);

    impl<V> SalsaArena<V> {
        /// Creates a new empty arena.
        ///
        /// ```
        /// let arena: la_arena::Arena<i32> = la_arena::Arena::new();
        /// assert!(arena.is_empty());
        /// ```
        pub const fn new() -> Self {
            Self(Arena::new())
        }

        /// Create a new empty arena with specific capacity.
        ///
        /// ```
        /// let arena: la_arena::Arena<i32> = la_arena::Arena::with_capacity(42);
        /// assert!(arena.is_empty());
        /// ```
        pub fn with_capacity(capacity: usize) -> Self {
            Self(Arena::with_capacity(capacity))
        }
    }

    impl<V> From<Arena<V>> for SalsaArena<V> {
        fn from(value: Arena<V>) -> Self {
            Self(value)
        }
    }

    impl<V> Default for SalsaArena<V> {
        fn default() -> Self {
            Self(Arena::default())
        }
    }

    impl<V> Deref for SalsaArena<V> {
        type Target = la_arena::Arena<V>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<V> DerefMut for SalsaArena<V> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    unsafe impl<V> salsa::Update for SalsaArena<V>
    where
        V: salsa::Update,
    {
        unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
            let new_arena = new_value.0;
            // SAFETY: old pointer given to us is required to meet both the safety & validity invariants.
            let old_arena: &mut Self = unsafe { &mut *old_pointer };

            // Must be the same length to have the chance of being considered equal.
            if old_arena.len() != new_arena.len() {
                old_arena.clear();
                old_arena.extend(new_arena.into_iter().map(|(_, it)| it));
                return true;
            }

            // Update values recursively.
            let mut changed = false;
            for (old_element, new_element) in old_arena
                .values_mut()
                .zip(new_arena.into_iter().map(|(_, it)| it))
            {
                changed |= unsafe { V::maybe_update(old_element, new_element) };
            }

            changed
        }
    }

    /// Wrapper around [`la_arena::ArenaMap`] that implements [`salsa::Update`].
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct SalsaArenaMap<IDX, V>(ArenaMap<IDX, V>);

    impl<T, V> SalsaArenaMap<Idx<T>, V> {
        /// Creates a new empty map.
        pub const fn new() -> Self {
            Self(ArenaMap::new())
        }

        /// Create a new empty map with specific capacity.
        pub fn with_capacity(capacity: usize) -> Self {
            Self(ArenaMap::with_capacity(capacity))
        }
    }

    impl<T, V> From<ArenaMap<Idx<T>, V>> for SalsaArenaMap<Idx<T>, V> {
        fn from(value: ArenaMap<Idx<T>, V>) -> Self {
            Self(value)
        }
    }

    impl<T, V> Default for SalsaArenaMap<Idx<T>, V> {
        fn default() -> Self {
            Self(ArenaMap::default())
        }
    }

    impl<T, V> Deref for SalsaArenaMap<Idx<T>, V> {
        type Target = la_arena::ArenaMap<Idx<T>, V>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T, V> DerefMut for SalsaArenaMap<Idx<T>, V> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    unsafe impl<T, V> salsa::Update for SalsaArenaMap<Idx<T>, V>
    where
        T: salsa::Update,
        V: salsa::Update,
    {
        unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
            'function: {
                let old_pointer = old_pointer;
                let new_map = new_value.0;
                let old_map: &mut Self = unsafe { &mut *old_pointer };

                // To be considered "equal", the set of keys
                // must be the same between the two maps.
                let same_keys = old_map.iter().count() == new_map.iter().count()
                    && old_map.iter().all(|(k, _)| new_map.contains_idx(k));

                // If the set of keys has changed, then just pull in the new values
                // from new_map and discard the old ones.
                if !same_keys {
                    old_map.clear();
                    old_map.extend(new_map);
                    break 'function true;
                }

                // Otherwise, recursively descend to the values.
                // We do not invoke `K::update` because we assume
                // that if the values are `Eq` they must not need
                // updating (see the trait criteria).
                let mut changed = false;
                for (key, new_value) in new_map.into_iter() {
                    let old_value = old_map.get_mut(key).unwrap();
                    changed |= unsafe { V::maybe_update(old_value, new_value) };
                }
                changed
            }
        }
    }
}
