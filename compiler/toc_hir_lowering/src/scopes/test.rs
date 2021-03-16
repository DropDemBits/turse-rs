//! Scope testing
use toc_hir::symbol::SymbolKind;
use toc_span::TextRange;

use super::ScopeBuilder;

fn dummy_range() -> TextRange {
    TextRange::new(0.into(), 1.into())
}

#[test]
fn test_ident_declare_use() {
    // declare | usage
    let mut scopes = ScopeBuilder::new();

    let def_id = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);
    let use_id = scopes.use_sym("a", dummy_range());

    assert_eq!(def_id, use_id.as_def());
}

#[test]
fn test_ident_redeclare() {
    let mut scopes = ScopeBuilder::new();

    // First decl, pass
    let initial_id = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);

    // Redecl, have different ids
    let redeclare_id = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);

    assert_ne!(initial_id, redeclare_id);
}

#[test]
fn test_ident_declare_shadow() {
    // Identifier shadowing is not allow within inner scopes, but is detected later on
    let mut scopes = ScopeBuilder::new();

    // Outer declare
    let outer_def = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);

    // Inner declare
    scopes.with_scope(false, |scopes| {
        let shadow_def = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);

        // Identifiers should be different
        assert_ne!(shadow_def, outer_def);
        // Use here should fetch shadow_def
        assert_eq!(scopes.use_sym("a", dummy_range()).as_def(), shadow_def);
    });

    // Use here should fetch outer_def
    assert_eq!(scopes.use_sym("a", dummy_range()).as_def(), outer_def);
}

#[test]
fn test_ident_declare_no_shadow() {
    // Declaring outer after inner scopes should not cause issues
    let mut scopes = ScopeBuilder::new();

    // Inner declare
    let (shadow_def, shadow_use) = scopes.with_scope(false, |scopes| {
        let shadow_def = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);
        let shadow_use = scopes.use_sym("a", dummy_range());

        (shadow_def, shadow_use)
    });

    // Outer declare
    let outer_def = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);
    let outer_use = scopes.use_sym("a", dummy_range());

    // No shadowing should be done, outer_use should match outer_def
    assert_eq!(outer_def, outer_use.as_def());
    // Identifiers should be different
    assert_ne!(shadow_def, outer_def);
    assert_ne!(shadow_use, outer_use);
}

#[test]
fn test_use_undefined() {
    let mut scopes = ScopeBuilder::new();

    let use_id = scopes.use_sym("a", dummy_range());
    let info = scopes.symbol_table.get_symbol(use_id.as_def());
    // Should be undeclared
    assert_eq!(info.kind, SymbolKind::Undeclared);
}

#[test]
fn test_use_shared_undefined() {
    // Undeclared identifiers should be hoisted to the top-most import boundary
    let mut scopes = ScopeBuilder::new();

    // Don't contaminate root scope
    let inner_id = scopes.with_scope(true, |scopes| {
        let inner_id = {
            let inner_id = scopes.with_scope(false, |scopes| {
                scopes.with_scope(false, |scopes| {
                    scopes.with_scope(false, |scopes| {
                        // Hoist through nested inner blocks, functions, and procedures
                        scopes.use_sym("undef", dummy_range())
                    })
                })
            });

            inner_id
        };

        // Declaration should have been hoisted to module level
        let top_level = scopes.use_sym("undef", dummy_range());
        assert_eq!(inner_id.as_def(), top_level.as_def());

        inner_id
    });

    // Identifier should not be hoisted across the import boundary
    let not_here = scopes.use_sym("undef", dummy_range());
    assert_ne!(inner_id, not_here);
}

#[test]
fn test_use_import() {
    // External identifiers should be imported into the current scope (i.e share the same IdentId)
    let mut scopes = ScopeBuilder::new();

    // Root declare
    let declare_id = scopes.def_sym("a", dummy_range(), SymbolKind::Declared, false);

    // Inner use
    scopes.with_scope(false, |scopes| {
        let import_id = scopes.use_sym("a", dummy_range());

        // Should use the same id
        assert_eq!(import_id.as_def(), declare_id);
    });
}

#[test]
fn test_import_boundaries() {
    let mut scopes = ScopeBuilder::new();

    // Declare some external identifiers
    let non_pervasive = scopes.def_sym("non_pervasive", dummy_range(), SymbolKind::Declared, false);
    let pervasive = scopes.def_sym("pervasive", dummy_range(), SymbolKind::Declared, true);
    let undecl = scopes.def_sym("undecl", dummy_range(), SymbolKind::Undeclared, false);

    // Make an inner block
    scopes.with_scope(false, |scopes| {
        // Should be able to access all 3 identifiers
        let inner_use_a = scopes.use_sym("non_pervasive", dummy_range());
        let inner_use_b = scopes.use_sym("pervasive", dummy_range());
        let inner_use_c = scopes.use_sym("undecl", dummy_range());
        assert_eq!(inner_use_a.as_def(), non_pervasive);
        assert_eq!(inner_use_b.as_def(), pervasive);
        assert_eq!(inner_use_c.as_def(), undecl);
    });

    // Make a block with a hard import boundary
    scopes.with_scope(true, |scopes| {
        // Can access even through an inner block
        scopes.with_scope(false, |scopes| {
            // Should only be able to access the pervasive identifier
            let undecl_use_a = scopes.use_sym("non_pervasive", dummy_range());
            let imported_use_b = scopes.use_sym("pervasive", dummy_range());
            let undecl_use_c = scopes.use_sym("undecl", dummy_range());

            assert_ne!(undecl_use_a.as_def(), non_pervasive);
            assert_eq!(imported_use_b.as_def(), pervasive);
            assert_ne!(undecl_use_c.as_def(), undecl);
        });
    });
}
