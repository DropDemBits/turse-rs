//! Internal struct-of-arrays impl for bounded variables.

use std::mem;

use ordermap::{OrderMap, OrderSet};

use crate::unify::index::{InferValue, InferVar};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct Bounds {
    up_sets: Vec<PolarSet>,
    invariant_sets: Vec<InvariantSet>,
    down_sets: Vec<PolarSet>,
}

impl Bounds {
    pub(crate) fn new_up_to(len: usize) -> Self {
        let polar_bounds = vec![Default::default(); len];

        Self {
            up_sets: polar_bounds.clone(),
            invariant_sets: vec![Default::default(); len],
            down_sets: polar_bounds,
        }
    }

    pub(crate) fn reserve_one(&mut self) {
        self.up_sets.push(Default::default());
        self.invariant_sets.push(Default::default());
        self.down_sets.push(Default::default());
    }

    pub(crate) fn shrink_to_fit(&mut self) {
        self.up_sets.shrink_to_fit();
        self.invariant_sets.shrink_to_fit();
        self.down_sets.shrink_to_fit();
    }

    pub(crate) fn bounds(&self, var: InferVar) -> VarBoundsRef<'_> {
        let index = var.to_node().index();

        VarBoundsRef {
            upper: &self.up_sets[index],
            invariant: &self.invariant_sets[index],
            lower: &self.down_sets[index],
        }
    }

    pub(crate) fn bounds_mut(&mut self, var: InferVar) -> VarBoundsMut<'_> {
        let index = var.to_node().index();

        VarBoundsMut {
            upper: &mut self.up_sets[index],
            invariant: &mut self.invariant_sets[index],
            lower: &mut self.down_sets[index],
        }
    }

    pub(crate) fn bounds_disjoint_pair_mut(
        &mut self,
        vars: [InferVar; 2],
    ) -> [VarBoundsMut<'_>; 2] {
        let indexes = vars.map(|var| var.to_node().index());

        let [left_up, right_up] = self.up_sets.get_disjoint_mut(indexes).unwrap();
        let [left_inv, right_inv] = self.invariant_sets.get_disjoint_mut(indexes).unwrap();
        let [left_down, right_down] = self.down_sets.get_disjoint_mut(indexes).unwrap();

        [
            VarBoundsMut {
                upper: left_up,
                invariant: left_inv,
                lower: left_down,
            },
            VarBoundsMut {
                upper: right_up,
                invariant: right_inv,
                lower: right_down,
            },
        ]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct VarBoundsRef<'b> {
    pub(crate) upper: &'b PolarSet,
    pub(crate) invariant: &'b InvariantSet,
    pub(crate) lower: &'b PolarSet,
}

impl VarBoundsRef<'_> {
    pub(crate) fn is_empty(&self) -> bool {
        self.upper.is_empty() && self.lower.is_empty() && self.invariant.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct VarBoundsMut<'b> {
    pub(crate) upper: &'b mut PolarSet,
    pub(crate) invariant: &'b mut InvariantSet,
    pub(crate) lower: &'b mut PolarSet,
}

impl VarBoundsMut<'_> {
    pub(crate) fn take(self) -> VarBounds {
        VarBounds {
            upper: mem::take(self.upper),
            invariant: mem::take(self.invariant),
            lower: mem::take(self.lower),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct VarBounds {
    pub(crate) upper: PolarSet,
    pub(crate) invariant: InvariantSet,
    pub(crate) lower: PolarSet,
}

type PolarSet = OrderSet<InferValue>;
type InvariantSet = OrderMap<InferValue, InvariantPolarity>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InvariantPolarity {
    Upper,
    Lower,
}
