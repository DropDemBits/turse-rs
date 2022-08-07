//! Scope testing
use toc_hir::symbol::LocalDefId;

use super::{DeclareKind, ForwardKind, PervasiveTracker, ScopeKind, ScopeTracker};

#[derive(Default)]
struct LocalDefAlloc {
    next_id: u32,
}

impl LocalDefAlloc {
    pub fn next(&mut self) -> LocalDefId {
        let id = self.next_id;
        self.next_id += 1;
        LocalDefId::new(id)
    }
}

#[test]
fn test_ident_declare_use() {
    // declare | usage
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    let def_id = defs.next();
    scopes.def_sym("a".into(), def_id, DeclareKind::Declared);
    let lookup_id = scopes.use_sym("a".into()).unwrap();

    assert_eq!(def_id, lookup_id);
}

#[test]
fn test_ident_redeclare() {
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    // First decl, pass
    let initial_id = defs.next();
    let old_def = scopes.def_sym("a".into(), initial_id, DeclareKind::Declared);
    assert_eq!(old_def, None);

    // Redecl, have different ids
    let redeclare_id = defs.next();
    let old_def = scopes.def_sym("a".into(), redeclare_id, DeclareKind::Declared);

    assert_ne!(initial_id, redeclare_id);
    // Should be the initial id
    assert_eq!(old_def, Some(initial_id));
}

#[test]
fn test_ident_declare_shadow() {
    // Identifier shadowing is not allow within inner scopes, but is detected later on
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    // Outer declare
    let outer_def = defs.next();
    let old_def = scopes.def_sym("a".into(), outer_def, DeclareKind::Declared);
    assert_eq!(old_def, None);

    // Inner declare
    scopes.with_scope(false, |scopes| {
        let shadow_def = defs.next();
        let old_def = scopes.def_sym("a".into(), shadow_def, DeclareKind::Declared);

        // Identifiers should be different
        assert_ne!(shadow_def, outer_def);
        // Old def should be outer_def
        assert_eq!(old_def, Some(outer_def));
        // Use here should fetch shadow_def
        assert_eq!(scopes.use_sym("a".into()).unwrap(), shadow_def);
    });

    // Use here should fetch outer_def
    assert_eq!(scopes.use_sym("a".into()).unwrap(), outer_def);
}

#[test]
fn test_ident_declare_no_shadow() {
    // Declaring outer after inner scopes should not cause issues
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    // Inner declare
    let (shadow_def, shadow_use) = scopes.with_scope(false, |scopes| {
        let shadow_def = defs.next();
        let old_def = scopes.def_sym("a".into(), shadow_def, DeclareKind::Declared);
        let shadow_use = scopes.use_sym("a".into()).unwrap();

        assert!(old_def.is_none());

        (shadow_def, shadow_use)
    });

    // Outer declare
    let outer_def = defs.next();
    let old_def = scopes.def_sym("a".into(), outer_def, DeclareKind::Declared);
    let outer_use = scopes.use_sym("a".into()).unwrap();

    // No shadowing should be done, outer_use should match outer_def,
    // and old_def shouldn't exist
    assert_eq!(outer_def, outer_use);
    assert!(old_def.is_none());
    // Identifiers should be different
    assert_ne!(shadow_def, outer_def);
    assert_ne!(shadow_use, outer_use);
}

#[test]
fn test_use_undefined() {
    let mut scopes = ScopeTracker::new(Default::default());

    let def_id = scopes.use_sym("a".into());

    // Should just be `None`
    assert_eq!(def_id, None);
}

#[test]
fn test_use_shared_undefined() {
    // Undeclared syms should be hoisted to the top-most import boundary
    let mut scopes = ScopeTracker::new(Default::default());

    // Don't contaminate root scope
    scopes.with_scope(true, |scopes| {
        scopes.with_scope(false, |scopes| {
            scopes.with_scope(false, |scopes| {
                scopes.with_scope(false, |scopes| {
                    assert_eq!(scopes.use_sym("undef".into()), None);

                    // Hoist through nested inner blocks, functions, and procedures
                    // This should be the first time it's being used
                    assert!(scopes.is_first_undecl_use("undef".into()));
                })
            })
        });

        assert_eq!(scopes.use_sym("undef".into()), None);

        // Undeclared tracking should have been hoisted to module level
        // This should not be the first undecl'd use
        assert!(!scopes.is_first_undecl_use("undef".into()));
    });

    assert_eq!(scopes.use_sym("undef".into()), None);
    // Tracking should not be hoisted across the import boundary,
    // so this should be the first use.
    assert!(scopes.is_first_undecl_use("undef".into()));
}

#[test]
fn test_use_import() {
    // External identifiers should be imported into the current scope (i.e share the same LocalDefId)
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    // Root declare
    let declare_id = defs.next();
    scopes.def_sym("a".into(), declare_id, DeclareKind::Declared);

    // Inner use
    scopes.with_scope(false, |scopes| {
        let import_id = scopes.use_sym("a".into()).unwrap();

        // Should use the same id
        assert_eq!(import_id, declare_id);
    });
}

#[test]
fn test_import_boundaries() {
    let mut pv_tracker = PervasiveTracker::default();
    let mut defs = LocalDefAlloc::default();

    let non_pervasive = defs.next();
    let pervasive = defs.next();

    pv_tracker.mark_pervasive(pervasive);

    // Declare some external identifiers
    let mut scopes = ScopeTracker::new(pv_tracker);
    scopes.def_sym("non_pervasive".into(), non_pervasive, DeclareKind::Declared);
    scopes.def_sym("pervasive".into(), pervasive, DeclareKind::Declared);

    // Make an inner block
    scopes.with_scope(false, |scopes| {
        // Should be able to access all 3 identifiers
        let inner_use_a = scopes.use_sym("non_pervasive".into());
        let inner_use_b = scopes.use_sym("pervasive".into());
        let inner_use_c = scopes.use_sym("undecl".into());
        assert_eq!(inner_use_a, Some(non_pervasive));
        assert_eq!(inner_use_b, Some(pervasive));
        assert_eq!(inner_use_c, None);
        assert!(scopes.is_first_undecl_use("undecl".into()));
    });

    // Make a block with a hard import boundary
    scopes.with_scope(true, |scopes| {
        // Can access even through an inner block
        scopes.with_scope(false, |scopes| {
            // Should only be able to access the pervasive identifier
            let undecl_use_a = scopes.use_sym("non_pervasive".into());
            let imported_use_b = scopes.use_sym("pervasive".into());
            let undecl_use_c = scopes.use_sym("undecl".into());

            assert_ne!(undecl_use_a, Some(non_pervasive));
            assert_eq!(imported_use_b, Some(pervasive));
            assert_eq!(undecl_use_c, None);
            // Different from the other one, hidden behind an import boundary
            assert!(scopes.is_first_undecl_use("undecl".into()));
        });
    });
}

#[test]
fn test_forward_declare() {
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    let forward_def = defs.next();
    scopes.def_sym(
        "fwd".into(),
        forward_def,
        DeclareKind::Forward(ForwardKind::Type, None),
    );
    let resolve_def = defs.next();
    scopes.def_sym(
        "fwd".into(),
        resolve_def,
        DeclareKind::Resolved(ForwardKind::Type),
    );

    assert_eq!(scopes.use_sym("fwd".into()).unwrap(), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
        Some(vec![forward_def])
    );
}

#[test]
fn test_forward_declare_double() {
    // Forward declares in the same scope should be captured
    // when resolving them
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    let forward_def0 = defs.next();
    scopes.def_sym(
        "fwd".into(),
        forward_def0,
        DeclareKind::Forward(ForwardKind::Type, None),
    );
    let forward_def1 = defs.next();
    scopes.def_sym(
        "fwd".into(),
        forward_def1,
        DeclareKind::Forward(ForwardKind::Type, None),
    );

    let resolve_def = defs.next();
    scopes.def_sym(
        "fwd".into(),
        resolve_def,
        DeclareKind::Resolved(ForwardKind::Type),
    );

    assert_eq!(scopes.use_sym("fwd".into()).unwrap(), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
        Some(vec![forward_def0, forward_def1])
    );
}

#[test]
fn test_forward_declare_overwritten() {
    // Later forward declares of different types should replace old forward lists
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    let forward_type = defs.next();
    let forward_proc = defs.next();
    let resolve_def = defs.next();

    scopes.def_sym(
        "fwd".into(),
        forward_type,
        DeclareKind::Forward(ForwardKind::Type, None),
    );
    scopes.def_sym(
        "fwd".into(),
        forward_proc,
        DeclareKind::Forward(ForwardKind::_Procedure, None),
    );

    scopes.def_sym(
        "fwd".into(),
        resolve_def,
        DeclareKind::Resolved(ForwardKind::Type),
    );

    assert_eq!(scopes.use_sym("fwd".into()).unwrap(), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
        None
    );
}

#[test]
fn test_forward_declare_overwritten_normal() {
    // Normal declarations should overwrite forward lists
    let mut scopes = ScopeTracker::new(Default::default());
    let mut defs = LocalDefAlloc::default();

    let forward_type = defs.next();
    let normal_def = defs.next();
    let resolve_def = defs.next();

    scopes.def_sym(
        "fwd".into(),
        forward_type,
        DeclareKind::Forward(ForwardKind::Type, None),
    );
    scopes.def_sym("fwd".into(), normal_def, DeclareKind::Declared);

    scopes.def_sym(
        "fwd".into(),
        resolve_def,
        DeclareKind::Resolved(ForwardKind::Type),
    );

    assert_eq!(scopes.use_sym("fwd".into()), Some(resolve_def));
    assert_eq!(
        scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
        None
    );
}

#[test]
fn test_forward_resolve_only_same_scope() {
    // Forward declares are only resolved in the same scope,
    // doesn't matter if it's an import boundary,
    // doesn't matter if the identifier is pervasive or not
    let mut pv_tracker = PervasiveTracker::default();
    let mut defs = LocalDefAlloc::default();

    let forward_def = defs.next();
    let inner_def = defs.next();
    let resolve_def = defs.next();

    pv_tracker.mark_pervasive(forward_def);

    let mut scopes = ScopeTracker::new(pv_tracker);

    // def scope def
    scopes.def_sym(
        "fwd".into(),
        forward_def,
        DeclareKind::Forward(ForwardKind::Type, None),
    );

    // Not import boundary
    scopes.with_scope(false, |scopes| {
        scopes.def_sym(
            "fwd".into(),
            inner_def,
            DeclareKind::Forward(ForwardKind::Type, None),
        );

        assert_eq!(
            scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
            Some(vec![inner_def])
        );
    });

    // Is import boundary
    scopes.with_scope(true, |scopes| {
        scopes.def_sym(
            "fwd".into(),
            inner_def,
            DeclareKind::Forward(ForwardKind::Type, None),
        );

        assert_eq!(
            scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
            Some(vec![inner_def])
        );
    });

    scopes.def_sym(
        "fwd".into(),
        resolve_def,
        DeclareKind::Resolved(ForwardKind::Type),
    );

    assert_eq!(scopes.use_sym("fwd".into()), Some(resolve_def));
    assert_eq!(
        scopes.take_resolved_forwards("fwd".into(), ForwardKind::Type),
        Some(vec![forward_def])
    );
}

#[test]
fn test_import_def_no_boundary() {
    // import ---
    let mut pv_tracker = PervasiveTracker::default();
    let mut defs = LocalDefAlloc::default();

    // shouldn't be peeked at from SurroundingScope
    // should be peeked at from CurrentScope
    let a_def = defs.next();

    pv_tracker.mark_pervasive(a_def);

    let mut scopes = ScopeTracker::new(pv_tracker);
    scopes.def_sym("a".into(), a_def, DeclareKind::Declared);

    assert_eq!(scopes.import_sym("a".into()), None);
}

#[test]
fn test_import_def_between_boundaries() {
    // import --- import -*- import
    let mut pv_tracker = PervasiveTracker::default();
    let mut defs = LocalDefAlloc::default();

    let outer_pv = defs.next();
    let outer = defs.next();
    let middle = defs.next();
    let inner = defs.next();

    pv_tracker.mark_pervasive(outer_pv);

    let mut scopes = ScopeTracker::new(pv_tracker);
    scopes.def_sym("outer_pv".into(), outer_pv, DeclareKind::Declared);
    scopes.def_sym("outer".into(), outer, DeclareKind::Declared);

    scopes.push_scope(ScopeKind::Module);
    {
        scopes.def_sym("middle".into(), middle, DeclareKind::Declared);

        scopes.push_scope(ScopeKind::Module);
        {
            scopes.def_sym("inner".into(), inner, DeclareKind::Declared);

            // outer_pv visible
            assert_eq!(scopes.import_sym("outer_pv".into()), Some(outer_pv));
            // outer invisible
            assert_eq!(scopes.import_sym("outer".into()), None);
            // middle visible
            assert_eq!(scopes.import_sym("middle".into()), Some(middle));
            // inner not importable
            assert_eq!(scopes.import_sym("inner".into()), None);
        }
        scopes.pop_scope();
    }
    scopes.pop_scope();
}

#[test]
fn test_import_def_implicit_boundary() {
    // import --- import -*- import

    // --- explicit -*- implicit
    // --- implicit -*- implicit (never encountered, would do scope hopping)
    // --- implicit -*- explicit
    let mut pv_tracker = PervasiveTracker::default();
    let mut defs = LocalDefAlloc::default();

    let outer_pv = defs.next();
    let outer = defs.next();
    let middle = defs.next();
    let inner = defs.next();

    pv_tracker.mark_pervasive(outer_pv);

    let mut scopes = ScopeTracker::new(pv_tracker);
    scopes.def_sym("outer_pv".into(), outer_pv, DeclareKind::Declared);
    scopes.def_sym("outer".into(), outer, DeclareKind::Declared);

    scopes.push_scope(ScopeKind::Module);
    {
        scopes.def_sym("middle".into(), middle, DeclareKind::Declared);

        scopes.push_scope(ScopeKind::Subprogram);
        {
            scopes.def_sym("inner".into(), inner, DeclareKind::Declared);

            // outer_pv visible
            assert_eq!(scopes.import_sym("outer_pv".into()), Some(outer_pv));
            // outer invisible
            assert_eq!(scopes.import_sym("outer".into()), None);
            // middle visible
            assert_eq!(scopes.import_sym("middle".into()), Some(middle));
            // inner not importable
            assert_eq!(scopes.import_sym("inner".into()), None);

            // middle should also be visible
            assert_eq!(scopes.use_sym("middle".into()).unwrap(), middle);
        }
        scopes.pop_scope();
    }
    scopes.pop_scope();
}
