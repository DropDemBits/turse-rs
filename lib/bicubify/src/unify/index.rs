//! Internal index-like definitions used during biunification

pub(crate) use infer_node::InferNode;

use crate::unify::UnifyKey;

/// Represents a flexible type variable, which forms an intermediary between the
/// `ena` implementation and the actual [`UnifyKey`] definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct InferVar(InferNode);

impl InferVar {
    pub(crate) fn new(index: u32) -> Self {
        assert!(index < InferNode::MAX_INDEX_U32, "index is too big");
        // Unwrap discharge: We assert that it's always smaller than the max index above.
        Self(InferNode::new_var(index).unwrap())
    }

    pub(crate) fn to_node(self) -> InferNode {
        self.0
    }

    pub(crate) fn from_key<K: UnifyKey>(key: K) -> Self {
        let index = <K as UnifyKey>::index(key);
        assert!(index < InferNode::MAX_INDEX_U32, "index is too big");
        InferVar(InferNode::new_var(index).unwrap())
    }

    pub(crate) fn to_key<K: UnifyKey>(self) -> K {
        let index = self.0.index_u32();
        K::from_index(index)
    }
}

impl ena::unify::UnifyKey for InferVar {
    type Value = BoundsState;

    fn index(&self) -> u32 {
        self.0.index_u32()
    }

    fn from_index(u: u32) -> Self {
        assert!(u < InferNode::MAX_INDEX_U32, "index is too big");
        // Unwrap discharge: We assert that it's always smaller than the max index above.
        Self(InferNode::new_var(u).unwrap())
    }

    fn tag() -> &'static str {
        "InferVar"
    }

    fn order_roots(
        a: Self,
        a_value: &Self::Value,
        b: Self,
        b_value: &Self::Value,
    ) -> Option<(Self, Self)> {
        match (a_value, b_value) {
            // Either is empty, preserve the ordering.
            (BoundsState::Empty, BoundsState::Empty) => Some((a, b)),
            // Pick whichever one is non-empty.
            (BoundsState::NonEmpty, BoundsState::Empty) => Some((a, b)),
            (BoundsState::Empty, BoundsState::NonEmpty) => Some((b, a)),
            // Both bounds are non-empty, this is only reachable when the bounds are equal.
            (BoundsState::NonEmpty, BoundsState::NonEmpty) => Some((a, b)),
        }
    }
}

/// Whether or not an [`InferVar`] has been bounded by any values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BoundsState {
    Empty,
    NonEmpty,
}

impl ena::unify::UnifyValue for BoundsState {
    type Error = ();

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        match (left, right) {
            // Empty bounds preserve emptyness.
            (BoundsState::Empty, BoundsState::Empty) => Ok(BoundsState::Empty),
            // Either one is non-empty is a trivial unification to empty bounds.
            (BoundsState::Empty, BoundsState::NonEmpty) => Ok(BoundsState::NonEmpty),
            (BoundsState::NonEmpty, BoundsState::Empty) => Ok(BoundsState::NonEmpty),
            // Both bounds are non-empty, it's implied that the bounds are the same.
            (BoundsState::NonEmpty, BoundsState::NonEmpty) => Ok(BoundsState::Empty),
        }
    }
}

/// Represents a type term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct InferValue(InferNode);

impl InferValue {
    pub(crate) fn to_node(self) -> InferNode {
        self.0
    }

    pub(crate) fn from_index(index: usize) -> Self {
        assert!(index < InferNode::MAX_INDEX, "index is too big");
        // Unwrap discharge: We assert that it's always smaller than the max index above.
        InferValue(InferNode::new_value(index).unwrap())
    }
}

mod infer_node {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub(crate) enum IndexSpace {
        Var,
        Value,
    }

    /// An arbitrary inference participant. This forms a dense allocation
    /// of type terms and type variables.
    ///
    /// Library Invariant: By construction, an `InferNode` always either points
    /// to the variable or the value space. This may be leveraged by unsafe
    /// code internally to perform more optimizations.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    // Invariant: u32 always points to either the variable or value index spaces.
    //
    // We can't trust that this always refers to a valid entry in the var or value
    // spaces as we don't tie this to a specific BiunificationTable instance.
    pub(crate) struct InferNode(u32);

    impl InferNode {
        const TAG_MASK: u32 = 1u32 << 31;
        const INDEX_MASK: u32 = !Self::TAG_MASK;

        /// Maximum value of an index (as a u32)
        pub(crate) const MAX_INDEX_U32: u32 = Self::INDEX_MASK;
        /// Maximum value of an index
        pub(crate) const MAX_INDEX: usize = Self::INDEX_MASK as usize;

        const VAR_TAG: u32 = 0;
        const VALUE_TAG: u32 = Self::TAG_MASK;

        /// Constructs a new variable node index. Always guaranteed to point into
        /// the variable index space.
        ///
        /// Returns `None` if the index is larger than [`MAX_INDEX`](Self::MAX_INDEX).
        pub(crate) fn new_var(index: u32) -> Option<Self> {
            if index < Self::MAX_INDEX_U32 {
                let raw = index | Self::VAR_TAG;
                Some(Self(raw))
            } else {
                None
            }
        }

        /// Constructs a new value node index. Always guaranteed to point into
        /// the value index space.
        ///
        /// Returns `None` if the index is larger than [`MAX_INDEX`](Self::MAX_INDEX).
        pub(crate) fn new_value(index: usize) -> Option<Self> {
            if index < Self::MAX_INDEX {
                let raw = (index as u32) | Self::VALUE_TAG;
                Some(Self(raw))
            } else {
                None
            }
        }

        /// Gets the raw index (as a `u32`) into either the variable or the value index space.
        pub(crate) fn index_u32(self) -> u32 {
            self.0 & Self::INDEX_MASK
        }

        /// Gets the index into either the variable or the value index space.
        pub(crate) fn index(self) -> usize {
            (self.0 & Self::INDEX_MASK) as usize
        }

        pub(crate) fn tag(self) -> IndexSpace {
            if (self.0 & Self::TAG_MASK) == Self::VALUE_TAG {
                IndexSpace::Value
            } else {
                IndexSpace::Var
            }
        }
    }

    impl std::fmt::Debug for InferNode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if f.alternate() {
                return f.debug_tuple("InferNode").field(&self.0).finish();
            }

            let tag = match self.tag() {
                IndexSpace::Var => "var",
                IndexSpace::Value => "value",
            };
            let index = self.index_u32();

            write!(f, "InferNode({tag}, {index})")
        }
    }
}
