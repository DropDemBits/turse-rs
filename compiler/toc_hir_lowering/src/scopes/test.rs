//! Scope testing
use toc_hir::symbol::{ForwardKind, LocalDefId, SymbolKind};

use super::ScopeTracker;

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

fn assert_declared() -> LocalDefId {
    panic!("should be declared")
}

fn make_undeclared(defs: &mut LocalDefAlloc) -> impl FnOnce() -> LocalDefId + '_ {
    move || defs.next()
}

#[test]
fn test_ident_declare_use() {
    // declare | usage
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let def_id = defs.next();
    scopes.def_sym("a", def_id, SymbolKind::Declared, false);
    let lookup_id = scopes.use_sym("a", assert_declared);

    assert_eq!(def_id, lookup_id);
}

#[test]
fn test_ident_redeclare() {
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    // First decl, pass
    let initial_id = defs.next();
    let old_def = scopes.def_sym("a", initial_id, SymbolKind::Declared, false);
    assert_eq!(old_def, None);

    // Redecl, have different ids
    let redeclare_id = defs.next();
    let old_def = scopes.def_sym("a", redeclare_id, SymbolKind::Declared, false);

    assert_ne!(initial_id, redeclare_id);
    // Should be the initial id
    assert_eq!(old_def, Some(initial_id));
}

#[test]
fn test_ident_declare_shadow() {
    // Identifier shadowing is not allow within inner scopes, but is detected later on
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    // Outer declare
    let outer_def = defs.next();
    let old_def = scopes.def_sym("a", outer_def, SymbolKind::Declared, false);
    assert_eq!(old_def, None);

    // Inner declare
    scopes.with_scope(false, |scopes| {
        let shadow_def = defs.next();
        let old_def = scopes.def_sym("a", shadow_def, SymbolKind::Declared, false);

        // Identifiers should be different
        assert_ne!(shadow_def, outer_def);
        // Old def should be outer_def
        assert_eq!(old_def, Some(outer_def));
        // Use here should fetch shadow_def
        assert_eq!(scopes.use_sym("a", assert_declared), shadow_def);
    });

    // Use here should fetch outer_def
    assert_eq!(scopes.use_sym("a", assert_declared), outer_def);
}

#[test]
fn test_ident_declare_no_shadow() {
    // Declaring outer after inner scopes should not cause issues
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    // Inner declare
    let (shadow_def, shadow_use) = scopes.with_scope(false, |scopes| {
        let shadow_def = defs.next();
        let old_def = scopes.def_sym("a", shadow_def, SymbolKind::Declared, false);
        let shadow_use = scopes.use_sym("a", assert_declared);

        assert!(old_def.is_none());

        (shadow_def, shadow_use)
    });

    // Outer declare
    let outer_def = defs.next();
    let old_def = scopes.def_sym("a", outer_def, SymbolKind::Declared, false);
    let outer_use = scopes.use_sym("a", assert_declared);

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
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let def_id = scopes.use_sym("a", || defs.next());

    // Should declare a new identifier
    assert_eq!(def_id, LocalDefId::new(0));
}

#[test]
fn test_use_shared_undefined() {
    // Undeclared identifiers should be hoisted to the top-most import boundary
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    // Don't contaminate root scope
    let inner_id = scopes.with_scope(true, |scopes| {
        let inner_id = {
            scopes.with_scope(false, |scopes| {
                scopes.with_scope(false, |scopes| {
                    scopes.with_scope(false, |scopes| {
                        // Hoist through nested inner blocks, functions, and procedures
                        scopes.use_sym("undef", make_undeclared(&mut defs))
                    })
                })
            })
        };

        // Declaration should have been hoisted to module level
        let top_level = scopes.use_sym("undef", make_undeclared(&mut defs));
        assert_eq!(inner_id, top_level);

        inner_id
    });

    // Identifier should not be hoisted across the import boundary
    let not_here = scopes.use_sym("undef", make_undeclared(&mut defs));
    assert_ne!(inner_id, not_here);
}

#[test]
fn test_use_import() {
    // External identifiers should be imported into the current scope (i.e share the same LocalDefId)
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    // Root declare
    let declare_id = defs.next();
    scopes.def_sym("a", declare_id, SymbolKind::Declared, false);

    // Inner use
    scopes.with_scope(false, |scopes| {
        let import_id = scopes.use_sym("a", assert_declared);

        // Should use the same id
        assert_eq!(import_id, declare_id);
    });
}

#[test]
fn test_import_boundaries() {
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    // Declare some external identifiers
    let non_pervasive = defs.next();
    scopes.def_sym("non_pervasive", non_pervasive, SymbolKind::Declared, false);
    let pervasive = defs.next();
    scopes.def_sym("pervasive", pervasive, SymbolKind::Declared, true);
    let undecl = scopes.use_sym("undecl", make_undeclared(&mut defs));

    // Make an inner block
    scopes.with_scope(false, |scopes| {
        // Should be able to access all 3 identifiers
        let inner_use_a = scopes.use_sym("non_pervasive", assert_declared);
        let inner_use_b = scopes.use_sym("pervasive", assert_declared);
        let inner_use_c = scopes.use_sym("undecl", assert_declared);
        assert_eq!(inner_use_a, non_pervasive);
        assert_eq!(inner_use_b, pervasive);
        assert_eq!(inner_use_c, undecl);
    });

    // Make a block with a hard import boundary
    scopes.with_scope(true, |scopes| {
        // Can access even through an inner block
        scopes.with_scope(false, |scopes| {
            // Should only be able to access the pervasive identifier
            let undecl_use_a = scopes.use_sym("non_pervasive", make_undeclared(&mut defs));
            let imported_use_b = scopes.use_sym("pervasive", make_undeclared(&mut defs));
            let undecl_use_c = scopes.use_sym("undecl", make_undeclared(&mut defs));

            assert_ne!(undecl_use_a, non_pervasive);
            assert_eq!(imported_use_b, pervasive);
            assert_ne!(undecl_use_c, undecl);
        });
    });
}

#[test]
fn test_forward_declare() {
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let forward_def = defs.next();
    scopes.def_sym(
        "fwd",
        forward_def,
        SymbolKind::Forward(ForwardKind::Type, None),
        false,
    );
    let resolve_def = defs.next();
    scopes.def_sym(
        "fwd",
        resolve_def,
        SymbolKind::Resolved(ForwardKind::Type),
        false,
    );

    assert_eq!(scopes.use_sym("fwd", assert_declared), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd", ForwardKind::Type),
        Some(vec![forward_def])
    );
}

#[test]
fn test_forward_declare_double() {
    // Forward declares in the same scope should be captured
    // when resolving them
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let forward_def0 = defs.next();
    scopes.def_sym(
        "fwd",
        forward_def0,
        SymbolKind::Forward(ForwardKind::Type, None),
        false,
    );
    let forward_def1 = defs.next();
    scopes.def_sym(
        "fwd",
        forward_def1,
        SymbolKind::Forward(ForwardKind::Type, None),
        false,
    );

    let resolve_def = defs.next();
    scopes.def_sym(
        "fwd",
        resolve_def,
        SymbolKind::Resolved(ForwardKind::Type),
        false,
    );

    assert_eq!(scopes.use_sym("fwd", assert_declared), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd", ForwardKind::Type),
        Some(vec![forward_def0, forward_def1])
    );
}

#[test]
fn test_forward_declare_overwritten() {
    // Later forward declares of different types should replace old forward lists
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let forward_type = defs.next();
    let forward_proc = defs.next();
    let resolve_def = defs.next();

    scopes.def_sym(
        "fwd",
        forward_type,
        SymbolKind::Forward(ForwardKind::Type, None),
        false,
    );
    scopes.def_sym(
        "fwd",
        forward_proc,
        SymbolKind::Forward(ForwardKind::_Procedure, None),
        false,
    );

    scopes.def_sym(
        "fwd",
        resolve_def,
        SymbolKind::Resolved(ForwardKind::Type),
        false,
    );

    assert_eq!(scopes.use_sym("fwd", assert_declared), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd", ForwardKind::Type),
        None
    );
}

#[test]
fn test_forward_declare_overwritten_normal() {
    // Normal declarations should overwrite forward lists
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let forward_type = defs.next();
    let normal_def = defs.next();
    let resolve_def = defs.next();

    scopes.def_sym(
        "fwd",
        forward_type,
        SymbolKind::Forward(ForwardKind::Type, None),
        false,
    );
    scopes.def_sym("fwd", normal_def, SymbolKind::Declared, false);

    scopes.def_sym(
        "fwd",
        resolve_def,
        SymbolKind::Resolved(ForwardKind::Type),
        false,
    );

    assert_eq!(scopes.use_sym("fwd", assert_declared), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd", ForwardKind::Type),
        None
    );
}

#[test]
fn test_forward_resolve_only_same_scope() {
    // Forward declares are only resolved in the same scope,
    // doesn't matter if it's an import boundary,
    // doesn't matter if the identifier is pervasive or not
    let mut scopes = ScopeTracker::new();
    let mut defs = LocalDefAlloc::default();

    let forward_def = defs.next();
    let inner_def = defs.next();
    let resolve_def = defs.next();

    // def scope def
    scopes.def_sym(
        "fwd",
        forward_def,
        SymbolKind::Forward(ForwardKind::Type, None),
        true,
    );

    // Not import boundary
    scopes.with_scope(false, |scopes| {
        scopes.def_sym(
            "fwd",
            inner_def,
            SymbolKind::Forward(ForwardKind::Type, None),
            false,
        );

        assert_eq!(
            scopes.take_resolved_forwards("fwd", ForwardKind::Type),
            Some(vec![inner_def])
        );
    });

    // Is import boundary
    scopes.with_scope(true, |scopes| {
        scopes.def_sym(
            "fwd",
            inner_def,
            SymbolKind::Forward(ForwardKind::Type, None),
            false,
        );

        assert_eq!(
            scopes.take_resolved_forwards("fwd", ForwardKind::Type),
            Some(vec![inner_def])
        );
    });

    scopes.def_sym(
        "fwd",
        resolve_def,
        SymbolKind::Resolved(ForwardKind::Type),
        false,
    );

    assert_eq!(scopes.use_sym("fwd", assert_declared), resolve_def);
    assert_eq!(
        scopes.take_resolved_forwards("fwd", ForwardKind::Type),
        Some(vec![forward_def])
    );
}
