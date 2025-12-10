use std::num::NonZeroU8;

use either::Either;
use ordermap::{OrderMap, OrderSet};
use proptest_state_machine::{self, ReferenceStateMachine, StateMachineTest, prop_state_machine};

use proptest::prelude::*;

// rust-analyzer whoopsie: workspace renames are not (always?) applied after
// in-file renames, though that might just be an LSP woe.
use crate::unify::{BiunificationTable, LowerBound, UnifyKey, UpperBound};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Var(u32);

impl UnifyKey for Var {
    type Value = Term;

    fn index(self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "Var"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
struct Term(u32);

impl Term {
    fn from_sequential(value: u32) -> Self {
        const MASK: u32 = 0x5555_5555;
        let low = value & MASK;
        let hi = value & !MASK;

        let mix = low.rotate_left(1) | hi.rotate_right(1);

        Self(mix)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TermRange(u32, u32);

#[derive(Debug, Default, Clone)]
struct TermGen(u32);

impl TermGen {
    fn next_term(&mut self) -> Term {
        let term = Term::from_sequential(self.0);
        self.0 += 1;
        term
    }

    fn n_terms(&mut self, len: u8) -> TermRange {
        let start = self.0;
        self.0 += u32::from(len);

        TermRange(start, self.0)
    }
}

impl TermRange {
    fn n_terms(len: u8) -> Self {
        assert!(len > 0);
        Self(0, len.into())
    }

    fn into_iter(self) -> impl Iterator<Item = Term> {
        (self.0..self.1).map(|it| Term::from_sequential(it))
    }
}

type TestTable = BiunificationTable<Var>;

prop_state_machine! {
    #[test]
    fn test_unifier(sequential 1..20 => UnifyTest);

    #[test]
    fn test_bounds_unifier(sequential 1..50 => BoundTest);
}

proptest! {
    #[test]
    fn test_eq_shuffled_terms_shallow_unify(
        shuffled_terms in any::<NonZeroU8>().prop_flat_map(|len| {
            prop::array::uniform::<_, 2>(gen_shuffled_terms(len.get()))
        }),
        as_lub: bool,
    ) {
        let mut unify = TestTable::default();
        let vars @ [left, right] = std::array::from_fn(|_| unify.fresh_var());

        for (terms, var) in shuffled_terms.into_iter().zip(vars.into_iter()) {
            if as_lub {
                terms.into_iter().for_each(|term| unify.lub_var(var, term, |_| ()));
            } else {
                terms.into_iter().for_each(|term| unify.glb_var(term, var, |_| ()));
            }
        }

        prop_assert_eq!(unify.shallow_unify_vars(left, right), Ok(()));
    }

    #[test]
    fn test_eq_shuffled_terms_unify_after_full(
        shuffled_terms in any::<NonZeroU8>().prop_flat_map(|len| {
            prop::array::uniform::<_, 3>(gen_shuffled_terms(len.get()))
        }),
        as_lub: bool,
    ) {
        let mut unify = TestTable::default();
        let vars @ [left, right, post] = std::array::from_fn(|_| unify.fresh_var());

        for (terms, var) in shuffled_terms.into_iter().zip(vars.into_iter()) {
            if as_lub {
                terms.into_iter().for_each(|term| unify.lub_var(var, term, |_| ()));
            } else {
                terms.into_iter().for_each(|term| unify.glb_var(term, var, |_| ()));
            }
        }

        unify.full_unify_var_var(left, right, |_| ());

        prop_assert_eq!(unify.shallow_unify_vars(left, post), Ok(()));
    }
}

fn gen_shuffled_terms(len: u8) -> impl Strategy<Value = Vec<Term>> {
    Just(TermRange::n_terms(len).into_iter().collect()).prop_shuffle()
}

#[derive(Debug, Clone)]
struct RefUnifyState {}

#[derive(Debug, Clone, Copy)]
enum UnifyTransition {
    NewUnbounded(u8),
    NewLubd(u8, u8),
    NewGlbd(u8, u8),
    NewBivariant(u8, u8),
    NewInvariant(u8, u8),
}

impl UnifyTransition {
    fn repeat_len(self) -> u8 {
        match self {
            UnifyTransition::NewUnbounded(len)
            | UnifyTransition::NewLubd(len, _)
            | UnifyTransition::NewGlbd(len, _)
            | UnifyTransition::NewBivariant(len, _)
            | UnifyTransition::NewInvariant(len, _) => len,
        }
    }
}

impl prop::arbitrary::Arbitrary for UnifyTransition {
    type Parameters = ();

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_compose! {
            fn dim_pair()(repeat in 1u8..20u8, terms in 1u8..10u8) -> (u8, u8) {
                (repeat, terms)
            }
        }

        use UnifyTransition::*;

        prop_oneof![
            dim_pair().prop_map(|(repeat, _)| NewUnbounded(repeat)),
            dim_pair().prop_map(|(repeat, terms)| NewLubd(repeat, terms)),
            dim_pair().prop_map(|(repeat, terms)| NewGlbd(repeat, terms)),
            dim_pair().prop_map(|(repeat, terms)| NewBivariant(repeat, terms)),
            dim_pair().prop_map(|(repeat, terms)| NewInvariant(repeat, terms)),
        ]
        .boxed()
    }

    type Strategy = BoxedStrategy<Self>;
}

impl ReferenceStateMachine for RefUnifyState {
    type State = RefUnifyState;

    type Transition = UnifyTransition;

    fn init_state() -> proptest::prelude::BoxedStrategy<Self::State> {
        Just(RefUnifyState {}).boxed()
    }

    fn transitions(_state: &Self::State) -> proptest::prelude::BoxedStrategy<Self::Transition> {
        any::<Self::Transition>()
    }

    fn apply(state: Self::State, _: &Self::Transition) -> Self::State {
        state
    }
}

struct UnifyTest {
    table: TestTable,
    vars: Vec<Var>,
    term: TermGen,

    expect_unify: Vec<Result<(), ()>>,
    expect_roots: usize,
}

impl StateMachineTest for UnifyTest {
    type SystemUnderTest = Self;

    type Reference = RefUnifyState;

    fn init_test(_: &<Self::Reference as ReferenceStateMachine>::State) -> Self::SystemUnderTest {
        Self {
            table: TestTable::default(),
            vars: Default::default(),
            term: Default::default(),
            expect_unify: Default::default(),
            expect_roots: 0,
        }
    }

    fn apply(
        mut state: Self::SystemUnderTest,
        _: &<Self::Reference as ReferenceStateMachine>::State,
        transition: <Self::Reference as ReferenceStateMachine>::Transition,
    ) -> Self::SystemUnderTest {
        let fresh_start = state.vars.len();
        state
            .vars
            .extend((0..transition.repeat_len()).map(|_| state.table.fresh_var()));
        state
            .expect_unify
            .extend((0..transition.repeat_len()).map(|_| Ok(())));
        let freshes = &state.vars[fresh_start..];

        match transition {
            UnifyTransition::NewUnbounded(_) => {
                // No need to restrict vars
            }
            UnifyTransition::NewLubd(_, terms) => {
                for term in state.term.n_terms(terms).into_iter() {
                    for var in freshes {
                        state.table.lub_var(*var, term, |_| ());
                    }
                }
            }
            UnifyTransition::NewGlbd(_, terms) => {
                for term in state.term.n_terms(terms).into_iter() {
                    for var in freshes {
                        state.table.glb_var(term, *var, |_| ());
                    }
                }
            }
            UnifyTransition::NewBivariant(_, terms) => {
                let ts0 = state.term.n_terms(terms);
                let ts1 = state.term.n_terms(terms);

                for (term0, term1) in ts0.into_iter().zip(ts1.into_iter()) {
                    for var in freshes {
                        state.table.lub_var(*var, term0, |_| ());
                        state.table.glb_var(term1, *var, |_| ());
                    }
                }
            }
            UnifyTransition::NewInvariant(_, terms) => {
                for term in state.term.n_terms(terms).into_iter() {
                    for var in freshes {
                        state.table.lub_var(*var, term, |_| ());
                        state.table.glb_var(term, *var, |_| ());
                    }
                }
            }
        }

        if !matches!(transition, UnifyTransition::NewUnbounded(_)) {
            state.expect_roots += 1;

            if state.expect_roots > 1 {
                // Transitioning to a new unify root
                state.expect_unify[fresh_start] = Err(());
            }
        }

        state
    }

    fn teardown(
        mut state: Self::SystemUnderTest,
        _: <Self::Reference as ReferenceStateMachine>::State,
    ) {
        let outcomes = state.expect_unify.into_iter().skip(1);

        // unify everything
        for (pairs, outcome) in state.vars.windows(2).zip(outcomes) {
            let &[left, right] = pairs else {
                unreachable!()
            };

            // Unification should be transitive over empty or equal bounds.
            let result = state.table.shallow_unify_vars(left, right);
            assert_eq!(result, outcome);
        }

        // fold down the roots
        let mut roots: Vec<_> = state
            .vars
            .into_iter()
            .map(|var| state.table.root_var(var))
            .collect();
        roots.dedup();

        let expected_roots = state.expect_roots.max(1);
        assert_eq!(roots.len(), expected_roots);
    }
}

#[derive(Debug, Default, Clone)]
struct RefBoundState {
    var_stack: usize,
    unique_upper: usize,
    unique_lower: usize,
    unique_invariant: usize,
}

#[derive(Debug, Clone, Copy)]
enum BoundTransition {
    FreshVar,
    LubTerms(u8),
    GlbTerms(u8),
    DualTerms(u8),
    InvariantTerms(u8),
    UnifyVar,
}

impl ReferenceStateMachine for RefBoundState {
    type State = Self;

    type Transition = BoundTransition;

    fn init_state() -> BoxedStrategy<Self::State> {
        Just(Self::default()).boxed()
    }

    fn transitions(state: &Self::State) -> BoxedStrategy<Self::Transition> {
        prop_compose! {
            fn dim()(terms in 1u8..10u8) -> u8 {
                terms
            }
        }

        if state.var_stack == 0 {
            // Only valid state when the stack is empty is to create a fresh variable.
            return Just(BoundTransition::FreshVar).boxed();
        }

        use BoundTransition::*;

        if state.var_stack == 1 {
            prop_oneof![
                Just(FreshVar),
                dim().prop_map(|repeat| LubTerms(repeat)),
                dim().prop_map(|repeat| GlbTerms(repeat)),
                dim().prop_map(|repeat| DualTerms(repeat)),
                dim().prop_map(|repeat| InvariantTerms(repeat)),
            ]
            .boxed()
        } else {
            prop_oneof![
                Just(FreshVar),
                dim().prop_map(|repeat| LubTerms(repeat)),
                dim().prop_map(|repeat| GlbTerms(repeat)),
                dim().prop_map(|repeat| DualTerms(repeat)),
                dim().prop_map(|repeat| InvariantTerms(repeat)),
                Just(UnifyVar),
            ]
            .boxed()
        }
    }

    fn apply(mut state: Self::State, transition: &Self::Transition) -> Self::State {
        match transition {
            BoundTransition::FreshVar => {
                state.var_stack += 1;
            }
            BoundTransition::LubTerms(_) => {
                state.unique_upper += 1;
            }
            BoundTransition::GlbTerms(_) => {
                state.unique_lower += 1;
            }
            BoundTransition::DualTerms(_) => {
                state.unique_upper += 1;
                state.unique_lower += 1;
            }
            BoundTransition::InvariantTerms(_) => {
                state.unique_invariant += 1;
            }
            BoundTransition::UnifyVar => {
                state.var_stack -= 1;
            }
        }

        state
    }

    fn preconditions(state: &Self::State, transition: &Self::Transition) -> bool {
        match transition {
            BoundTransition::FreshVar => true,
            BoundTransition::LubTerms(_)
            | BoundTransition::GlbTerms(_)
            | BoundTransition::DualTerms(_)
            | BoundTransition::InvariantTerms(_) => state.var_stack > 0,
            BoundTransition::UnifyVar => state.var_stack > 1,
        }
    }
}

#[derive(Default)]
struct BoundTest {
    table: TestTable,
    vars: Vec<Var>,
    term: TermGen,
    term_origin: OrderMap<Term, BoundOrigin>,
    seen_relates: OrderSet<(Term, Term)>,
}

enum BoundOrigin {
    Lub,
    Glb,
    Dual,
    Invariant,
}

impl StateMachineTest for BoundTest {
    type SystemUnderTest = Self;

    type Reference = RefBoundState;

    fn init_test(_: &<Self::Reference as ReferenceStateMachine>::State) -> Self::SystemUnderTest {
        Self {
            table: TestTable::default(),
            vars: Default::default(),
            term: Default::default(),
            term_origin: Default::default(),
            seen_relates: Default::default(),
        }
    }

    fn apply(
        mut state: Self::SystemUnderTest,
        _: &<Self::Reference as ReferenceStateMachine>::State,
        transition: <Self::Reference as ReferenceStateMachine>::Transition,
    ) -> Self::SystemUnderTest {
        let mut track_seen = |terms: Either<LowerBound<'_, Var>, UpperBound<'_, Var>>| {
            let pair = match terms {
                Either::Left(lower) => (*lower.lower, *lower.term),
                Either::Right(upper) => (*upper.term, *upper.upper),
            };

            let (_, fresh) = state.seen_relates.insert_full(pair);
            assert!(fresh, "relate seen more than once");
        };

        match transition {
            BoundTransition::FreshVar => {
                state.vars.push(state.table.fresh_var());
            }
            BoundTransition::UnifyVar => {
                let Some(right_var) = state.vars.pop() else {
                    unreachable!()
                };
                let Some(left_var) = state.vars.last_mut().copied() else {
                    unreachable!()
                };

                if let Err(()) = state.table.shallow_unify_vars(left_var, right_var) {
                    state
                        .table
                        .full_unify_var_var(left_var, right_var, |relate| track_seen(relate));
                }
            }
            BoundTransition::LubTerms(repeat_terms) => {
                let Some(top_var) = state.vars.last_mut().copied() else {
                    unreachable!()
                };

                let term = state.term.next_term();
                state.term_origin.insert(term, BoundOrigin::Lub);

                for _ in 0..repeat_terms {
                    state
                        .table
                        .lub_var(top_var, term, |relate| track_seen(Either::Left(relate)));
                }
            }
            BoundTransition::GlbTerms(repeat_terms) => {
                let Some(top_var) = state.vars.last_mut().copied() else {
                    unreachable!()
                };

                let term = state.term.next_term();
                state.term_origin.insert(term, BoundOrigin::Glb);

                for _ in 0..repeat_terms {
                    state
                        .table
                        .glb_var(term, top_var, |relate| track_seen(Either::Right(relate)));
                }
            }
            BoundTransition::DualTerms(repeat_terms) => {
                let Some(top_var) = state.vars.last_mut().copied() else {
                    unreachable!()
                };

                let term_upper = state.term.next_term();
                state.term_origin.insert(term_upper, BoundOrigin::Dual);

                let term_lower = state.term.next_term();
                state.term_origin.insert(term_lower, BoundOrigin::Dual);

                for _ in 0..repeat_terms {
                    state.table.lub_var(top_var, term_upper, |relate| {
                        track_seen(Either::Left(relate))
                    });
                }

                for _ in 0..repeat_terms {
                    state.table.glb_var(term_lower, top_var, |relate| {
                        track_seen(Either::Right(relate))
                    });
                }
            }
            BoundTransition::InvariantTerms(repeat_terms) => {
                let Some(top_var) = state.vars.last_mut().copied() else {
                    unreachable!()
                };

                let term = state.term.next_term();
                state.term_origin.insert(term, BoundOrigin::Invariant);

                for _ in 0..repeat_terms {
                    state
                        .table
                        .lub_var(top_var, term, |relate| track_seen(Either::Left(relate)));
                }

                for _ in 0..repeat_terms {
                    state
                        .table
                        .glb_var(term, top_var, |relate| track_seen(Either::Right(relate)));
                }
            }
        }

        state
    }

    fn teardown(
        mut state: Self::SystemUnderTest,
        mut ref_state: <Self::Reference as ReferenceStateMachine>::State,
    ) {
        while state.vars.len() > 1 {
            let transition = BoundTransition::UnifyVar;
            state = Self::apply(state, &ref_state, transition);
            ref_state = <Self::Reference as ReferenceStateMachine>::apply(ref_state, &transition);
        }

        eprintln!("ref_state: {ref_state:?}");
        eprintln!("relates:\n{}", emit_dotviz(&state));

        if ref_state.unique_invariant == 0 {
            let expected_edges = ref_state.unique_upper * ref_state.unique_lower;
            assert_eq!(
                state.seen_relates.len(),
                expected_edges,
                "fully unified graph is not bipartite"
            );
        } else {
            // comes-from: (U+I)(L+I) => UL+IL+IU+II
            let real = state.seen_relates.len();

            let lower_limit = ref_state.unique_upper * ref_state.unique_lower;
            let upper_tie_break_term = ref_state.unique_lower * ref_state.unique_invariant;
            let lower_tie_break_term = ref_state.unique_upper * ref_state.unique_invariant;
            let invariant_graph_term = ref_state.unique_invariant * ref_state.unique_invariant;
            let upper_limit =
                lower_limit + upper_tie_break_term + lower_tie_break_term + invariant_graph_term;

            let with_upper_tie_break =
                lower_limit + upper_tie_break_term + invariant_graph_term / 2;
            let with_lower_tie_break =
                lower_limit + lower_tie_break_term + invariant_graph_term / 2;
            let new_edges = real - lower_limit;

            eprintln!(
                "edge difference: real {real} vs +{new_edges}\n - upper break: {with_upper_tie_break}\n - lower break: {with_lower_tie_break}\n - terms: base {lower_limit} + upper {upper_tie_break_term} + lower {lower_tie_break_term} + invariant {invariant_graph_term}\n - upper limit {upper_limit}",
            );

            assert!(real < upper_limit, "invariant ties must be broken");
            assert!(real >= lower_limit, "missing edges");
        }
    }
}

fn emit_dotviz(state: &BoundTest) -> String {
    let (mut lub, mut glb, mut inv, mut dual) = (vec![], vec![], vec![], vec![]);
    for (term, origin) in &state.term_origin {
        let which = match origin {
            BoundOrigin::Lub => &mut lub,
            BoundOrigin::Glb => &mut glb,
            BoundOrigin::Invariant => &mut inv,
            BoundOrigin::Dual => &mut dual,
        };

        which.push(*term);
    }

    let mut edges = vec![];
    for (left, right) in &state.seen_relates {
        edges.push((*left, *right));
    }

    let out = {
        let mut out = String::new();

        use std::fmt::Write;
        write!(&mut out, "const constraints: ConstraintGraph = {{\n").unwrap();
        write!(&mut out, "  lub: [").unwrap();
        for term in lub {
            write!(&mut out, "'Term{}',", term.0).unwrap();
        }
        write!(&mut out, "],\n").unwrap();

        write!(&mut out, "  glb: [").unwrap();
        for term in glb {
            write!(&mut out, "'Term{}',", term.0).unwrap();
        }
        write!(&mut out, "],\n").unwrap();

        write!(&mut out, "  dual: [").unwrap();
        for term in dual {
            write!(&mut out, "'Term{}',", term.0).unwrap();
        }
        write!(&mut out, "],\n").unwrap();

        write!(&mut out, "  inv: [").unwrap();
        for term in inv {
            write!(&mut out, "'Term{}',", term.0).unwrap();
        }
        write!(&mut out, "],\n").unwrap();

        write!(&mut out, "  edges: [").unwrap();
        for (left, right) in edges {
            let left = left.0;
            let right = right.0;
            write!(&mut out, "['Term{left}','Term{right}'],").unwrap();
        }
        write!(&mut out, "],\n").unwrap();
        write!(&mut out, "}};").unwrap();

        out
    };
    out
}
