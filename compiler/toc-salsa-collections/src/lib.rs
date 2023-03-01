//! Collections that depend on salsa structs

use std::marker::PhantomData;

/// A map from salsa [`Ids`](salsa::Id) to some other type.
/// Space requirement is O(highest index).
///
/// Heavily inspired by `la_arena`'s `ArenaMap`.
#[derive(Debug, Clone)]
pub struct IdMap<K, V> {
    v: Vec<Option<V>>,
    _idx: PhantomData<K>,
}

impl<K, V> IdMap<K, V> {
    /// Creates a new map
    pub const fn new() -> Self {
        Self {
            v: Vec::new(),
            _idx: PhantomData,
        }
    }

    /// Creates a new map with the given capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            v: Vec::with_capacity(capacity),
            _idx: Default::default(),
        }
    }

    /// Reserves capacity for at least additional more elements to be inserted in the map.
    pub fn reserve(&mut self, additional: usize) {
        self.v.reserve(additional);
    }

    /// Clears the map, removing all elements.
    pub fn clear(&mut self) {
        self.v.clear();
    }

    /// Shrinks the capacity of the map as much as possible.
    pub fn shrink_to_fit(&mut self) {
        let min_len = self
            .v
            .iter()
            .rposition(|slot| slot.is_some())
            .map_or(0, |i| i + 1);
        self.v.truncate(min_len);
        self.v.shrink_to_fit();
    }
}
impl<K: salsa::AsId, V> IdMap<K, V> {
    /// Returns whether the map contains a value for the specified id.
    pub fn contains_id(&self, id: K) -> bool {
        matches!(self.v.get(Self::to_idx(id)), Some(Some(_)))
    }

    /// Removes an id from the map, returning the value at the id if the id was previously in the map.
    pub fn remove(&mut self, id: K) -> Option<V> {
        self.v.get_mut(Self::to_idx(id))?.take()
    }

    /// Inserts a value associated with a given salsa id into the map.
    ///
    /// If the map did not have this id present, None is returned.
    /// Otherwise, the value is updated, and the old value is returned.
    pub fn insert(&mut self, id: K, value: V) -> Option<V> {
        let idx = Self::to_idx(id);

        // resize truncates, so make sure that we don't go below the source length
        // add one since we want the number of elements instead of the index
        let len = self.v.len();
        self.v.resize_with((idx + 1).max(len), || None);
        self.v[idx].replace(value)
    }

    /// Returns a reference to the value associated with the provided id
    /// if it is present.
    pub fn get(&self, id: K) -> Option<&V> {
        let idx = Self::to_idx(id);
        self.v.get(idx)?.as_ref()
    }

    /// Returns a mutable reference to the value associated with the provided id
    /// if it is present.
    pub fn get_mut(&mut self, id: K) -> Option<&mut V> {
        self.v.get_mut(Self::to_idx(id)).and_then(|it| it.as_mut())
    }

    /// Returns an iterator over the values in the map.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.v.iter().filter_map(|o| o.as_ref())
    }

    /// Returns an iterator over mutable references to the values in the map.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.v.iter_mut().filter_map(|o| o.as_mut())
    }

    /// Returns an iterator over the salsa ids and values in the map.
    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.v
            .iter()
            .enumerate()
            .filter_map(|(idx, o)| Some((Self::from_idx(idx), o.as_ref()?)))
    }

    /// Returns an iterator over the salsa ids and values in the map.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        self.v
            .iter_mut()
            .enumerate()
            .filter_map(|(idx, o)| Some((Self::from_idx(idx), o.as_mut()?)))
    }

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, id: K) -> Entry<'_, K, V> {
        let idx = Self::to_idx(id);
        self.v.resize_with((idx + 1).max(self.v.len()), || None);
        match &mut self.v[idx] {
            slot @ Some(_) => Entry::Occupied(OccupiedEntry {
                slot,
                _ty: PhantomData,
            }),
            slot @ None => Entry::Vacant(VacantEntry {
                slot,
                _ty: PhantomData,
            }),
        }
    }

    fn to_idx(id: K) -> usize {
        usize::from(id.as_id())
    }

    fn from_idx(idx: usize) -> K {
        K::from_id(salsa::Id::from(idx))
    }
}

impl<K: salsa::AsId, V> std::ops::Index<K> for IdMap<K, V> {
    type Output = V;
    fn index(&self, idx: K) -> &V {
        self.v[Self::to_idx(idx)].as_ref().unwrap()
    }
}

impl<K: salsa::AsId, V> std::ops::IndexMut<K> for IdMap<K, V> {
    fn index_mut(&mut self, idx: K) -> &mut V {
        self.v[Self::to_idx(idx)].as_mut().unwrap()
    }
}

impl<K, V> Default for IdMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: salsa::AsId, V> Extend<(K, V)> for IdMap<K, V> {
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        iter.into_iter().for_each(move |(k, v)| {
            self.insert(k, v);
        });
    }
}

impl<K: salsa::AsId, V> FromIterator<(K, V)> for IdMap<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut this = Self::new();
        this.extend(iter);
        this
    }
}

/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`IdMap`].
///
/// [`entry`]: IdMap::entry
pub enum Entry<'a, IDX, V> {
    /// A vacant entry.
    Vacant(VacantEntry<'a, IDX, V>),
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, IDX, V>),
}

impl<'a, K, V> Entry<'a, K, V> {
    /// Ensures a value is in the entry by inserting the default if empty, and returns a mutable reference to
    /// the value in the entry.
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Self::Vacant(ent) => ent.insert(default),
            Self::Occupied(ent) => ent.into_mut(),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the default function if empty, and returns
    /// a mutable reference to the value in the entry.
    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
        match self {
            Self::Vacant(ent) => ent.insert(default()),
            Self::Occupied(ent) => ent.into_mut(),
        }
    }

    /// Provides in-place mutable access to an occupied entry before any potential inserts into the map.
    pub fn and_modify<F: FnOnce(&mut V)>(mut self, f: F) -> Self {
        if let Self::Occupied(ent) = &mut self {
            f(ent.get_mut());
        }
        self
    }
}

impl<'a, K, V> Entry<'a, K, V>
where
    V: Default,
{
    /// Ensures a value is in the entry by inserting the default value if empty, and returns a mutable reference
    /// to the value in the entry.
    pub fn or_default(self) -> &'a mut V {
        self.or_insert_with(Default::default)
    }
}

/// A view into an vacant entry in an [`IdMap`]. It is part of the [`Entry`] enum.
pub struct VacantEntry<'a, K, V> {
    slot: &'a mut Option<V>,
    _ty: PhantomData<K>,
}

impl<'a, IDX, V> VacantEntry<'a, IDX, V> {
    /// Sets the value of the entry with the `VacantEntry`’s key, and returns a mutable reference to it.
    pub fn insert(self, value: V) -> &'a mut V {
        self.slot.insert(value)
    }
}

/// A view into an occupied entry in an [`IdMap`]. It is part of the [`Entry`] enum.
pub struct OccupiedEntry<'a, IDX, V> {
    slot: &'a mut Option<V>,
    _ty: PhantomData<IDX>,
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
    /// Gets a reference to the value in the entry.
    pub fn get(&self) -> &V {
        self.slot.as_ref().expect("Occupied")
    }

    /// Gets a mutable reference to the value in the entry.
    pub fn get_mut(&mut self) -> &mut V {
        self.slot.as_mut().expect("Occupied")
    }

    /// Converts the entry into a mutable reference to its value.
    pub fn into_mut(self) -> &'a mut V {
        self.slot.as_mut().expect("Occupied")
    }

    /// Sets the value of the entry with the `OccupiedEntry`’s key, and returns the entry’s old value.
    pub fn insert(&mut self, value: V) -> V {
        self.slot.replace(value).expect("Occupied")
    }

    /// Takes the value of the entry out of the map, and returns it.
    pub fn remove(self) -> V {
        self.slot.take().expect("Occupied")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct Entity(salsa::Id);

    impl Entity {
        fn new(id: u32) -> Self {
            Self(salsa::Id::from_u32(id))
        }
    }

    impl salsa::AsId for Entity {
        fn as_id(self) -> salsa::Id {
            self.0
        }

        fn from_id(id: salsa::Id) -> Self {
            Self(id)
        }
    }

    #[test]
    fn id_map_reverse_insert() {
        // Don't accidentally truncate the map
        let mut map = IdMap::new();

        map.insert(Entity::new(3), 3);
        map.insert(Entity::new(2), 2);
        map.insert(Entity::new(1), 1);

        assert_eq!(map.get(Entity::new(3)), Some(&3));
    }
}
