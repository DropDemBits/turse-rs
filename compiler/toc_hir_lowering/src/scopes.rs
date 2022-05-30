//! Scope building
//!
//! Only keeps track of [`LocalDefId`]s
//!
//! [`LocalDefId`]: symbol::LocalDefId
#[cfg(test)]
mod test;

use std::collections::{HashMap, HashSet};

use toc_hir::symbol::{self, ForwardKind, LocalDefId, Symbol, SymbolKind};

#[derive(Debug)]
pub(crate) struct Scope {
    /// What kind of this scope this is
    kind: ScopeKind,
    /// All symbols declared in a scope.
    symbols: HashMap<Symbol, symbol::LocalDefId>,
    /// Any symbols within this scope that relate to a forward declaration.
    forward_symbols: HashMap<Symbol, (ForwardKind, Vec<symbol::LocalDefId>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeKind {
    Root,
    #[allow(dead_code)] // Only constructed in tests for now
    Module,
    Block,
    Loop,
    Subprogram,
    SubprogramHeader,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LookupKind {
    /// Lookup started from using a definition.
    /// All normal rules apply.
    OnUse,
    /// Lookup started from adding a definition.
    /// Most rules apply, except that a definition
    /// can be shadowed if the scope allows it.
    OnDef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ImportBoundary {
    /// No import boundary for this scope, and all definitions are allowed
    /// through. This scope will not be a starting point for import lookup.
    None,
    /// Non-pervasive definitions are allowed to also be implicitly imported,
    /// and this scope will also serve as a starting point for import lookup.
    Implicit,
    /// Only pervasive definitions are allowed to be implicitly imported.
    /// and this scope will serve as a starting point for import lookup.
    Explicit,
}

impl ImportBoundary {
    /// If this boundary only allows pervasive definitions to be implicitly imported.
    fn only_pervasive(self) -> bool {
        matches!(self, Self::Explicit)
    }

    /// If this is the starting for import lookup
    fn starts_import_lookup(self) -> bool {
        matches!(self, Self::Explicit | Self::Implicit)
    }
}

impl ScopeKind {
    /// What kind of import boundary this scope forms
    fn import_boundary(&self) -> ImportBoundary {
        match self {
            ScopeKind::Root | ScopeKind::Module => ImportBoundary::Explicit,
            ScopeKind::Subprogram | ScopeKind::SubprogramHeader => ImportBoundary::Implicit,
            _ => ImportBoundary::None,
        }
    }

    /// If the scope allows shadowing of identifiers.
    fn allows_shadowing(&self) -> bool {
        self.import_boundary().only_pervasive() || matches!(self, ScopeKind::Subprogram)
    }
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            symbols: HashMap::new(),
            forward_symbols: HashMap::new(),
        }
    }

    fn def_in(&mut self, name: Symbol, def_id: symbol::LocalDefId) {
        self.symbols.insert(name, def_id);
    }
}

pub(crate) struct ScopeTracker {
    scopes: Vec<Scope>,
    /// Keeps track of what symbols had pervasive attributes
    pervasive_tracker: HashSet<symbol::LocalDefId>,
}

impl std::fmt::Debug for ScopeTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ScopeTracker").finish_non_exhaustive()
    }
}

impl ScopeTracker {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new(ScopeKind::Root)],
            pervasive_tracker: HashSet::default(),
        }
    }

    /// Wraps symbol definitions inside of a new scope
    ///
    /// Only used for tests
    #[cfg(test)]
    fn with_scope<F: FnOnce(&mut Self) -> R, R>(&mut self, is_import_boundary: bool, f: F) -> R {
        let kind = if is_import_boundary {
            ScopeKind::Module
        } else {
            ScopeKind::Block
        };

        self.scopes.push(Scope::new(kind));
        let ret = f(self);
        self.scopes.pop();

        ret
    }

    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    pub fn pop_scope(&mut self) {
        debug_assert!(self.scopes.len() > 1, "Cannot pop off root scope");
        self.scopes.pop();
    }

    /// Checks if the given `def_id` is pervasive (i.e. always implicitly imported)
    pub fn is_pervasive(&self, def_id: symbol::LocalDefId) -> bool {
        self.pervasive_tracker.contains(&def_id)
    }

    /// Bring the definition into scope with the name `name`
    ///
    /// # Returns
    /// The definition in scope that previously held this `name`, if present
    pub fn def_sym(
        &mut self,
        name: Symbol,
        def_id: symbol::LocalDefId,
        kind: SymbolKind,
        is_pervasive: bool,
    ) -> Option<symbol::LocalDefId> {
        use std::collections::hash_map::Entry;

        let last_def = self.lookup_def(name, LookupKind::OnDef);
        let def_scope = self.scopes.last_mut().unwrap();
        def_scope.def_in(name, def_id);

        // Update the forward decl list
        match kind {
            SymbolKind::Forward(forward_kind, _) => {
                // Add to this scope's forward declaration list
                let forward_group = def_scope.forward_symbols.entry(name);

                match forward_group {
                    Entry::Occupied(entry) => {
                        let forward_group = entry.into_mut();

                        // Only add to the same list if it's the same forward kind
                        // Any different forward declarations always drop the old resolved types
                        if forward_kind == forward_group.0 {
                            forward_group.1.push(def_id);
                        } else {
                            *forward_group = (forward_kind, vec![def_id])
                        }
                    }
                    Entry::Vacant(entry) => {
                        // New forward group, can insert without any issues
                        entry.insert((forward_kind, vec![def_id]));
                    }
                }
            }
            SymbolKind::Declared => {
                // Remove it completely, leaving any forward decls unresolved
                def_scope.forward_symbols.remove(&name);
            }
            _ => (),
        }

        if is_pervasive {
            self.pervasive_tracker.insert(def_id);
        }

        last_def
    }

    /// Looks up the given def named `name`, using `or_undeclared` if it doesn't exist
    pub fn use_sym(
        &mut self,
        name: Symbol,
        or_undeclared: impl FnOnce() -> symbol::LocalDefId,
    ) -> symbol::LocalDefId {
        self.lookup_def(name, LookupKind::OnUse).unwrap_or_else(|| {
            // ???: Do we still need to declare undecl's at the boundary scope?
            // Since we plan to run another pass to collect defs for the export tables,
            // would it make more sense to hoist up to root?
            //
            // The original motivation was that undeclared defs may or may not represent
            // unqualified imports, so it make sense to have the same undeclared names
            // in an import boundary to share a LocalDefId. Thus, if the undecl def is
            // really an unqualified import, we'd have done something approximating the
            // right thing.
            //
            // However, if ScopeTracker has access to the complete export tables, then we
            // can disambiguate between unqualified imports and undeclared definitions,
            // leaving us free to always have all of the undeclared defs share a LocalDefId.
            //
            // Addendum:
            // This conveniently deals with deduplicating undeclared identifier errors, so
            // not doing this would mean that we'd have to handle that undeclared tracking
            // elsewhere.

            // Declare at the import boundary
            let def_id = or_undeclared();
            Self::boundary_scope(&mut self.scopes).def_in(name, def_id);
            def_id
        })
    }

    /// Looks up the definition that would be imported by `name`
    pub fn import_sym(&mut self, name: Symbol) -> Option<LocalDefId> {
        // Lookup is performed between two boundaries
        //
        // Get the first scope after the one that starts the lookup, since we want everything declared outside of it
        let scopes = self
            .scopes
            .iter()
            .rev()
            .skip_while(|scope| !scope.kind.import_boundary().starts_import_lookup())
            .skip(1);

        let mut restrict_to_pervasive = false;

        for scope in scopes {
            if let Some(def_id) = scope.symbols.get(&name) {
                let def_id = *def_id;

                // Only allow an identifier to be fetched if we haven't
                // restricted our search to pervasive identifiers, or any
                // pervasive identifiers
                if !restrict_to_pervasive || self.is_pervasive(def_id) {
                    return Some(def_id);
                }
            }

            if scope.kind.import_boundary().only_pervasive() {
                // First case: Crossing an explicit import boundary
                // Restrict search to pervasive identifiers after import boundaries
                restrict_to_pervasive = true;
            }
        }

        None
    }

    /// Takes the list of definitions that are resolved by `name`.
    pub fn take_resolved_forwards(
        &mut self,
        name: Symbol,
        resolve_kind: ForwardKind,
    ) -> Option<Vec<symbol::LocalDefId>> {
        use std::collections::hash_map::Entry;

        let def_scope = self.scopes.last_mut().unwrap();
        let forward_group = def_scope.forward_symbols.entry(name);

        match forward_group {
            Entry::Occupied(entry) => {
                let (forward_kind, resolve_list) = entry.remove_entry().1;

                // Forward entries are only returned if it's being resolved to the same kind of forward decl
                // Otherwise, they should be left unresolved
                (resolve_kind == forward_kind).then(|| resolve_list)
            }
            Entry::Vacant(_) => {
                // No forward entries to return
                None
            }
        }
    }

    /// Looks up a DefId, with respect to scoping rules
    fn lookup_def(&self, name: Symbol, lookup_kind: LookupKind) -> Option<symbol::LocalDefId> {
        // Top-down search through all scopes for a DefId
        let mut restrict_to_pervasive = false;

        if matches!(lookup_kind, LookupKind::OnDef) {
            if let Some(scope) = self.scopes.last() {
                if scope.kind == ScopeKind::SubprogramHeader {
                    // In subprogram header for new definition, only look in this scope for duplicate parameter naming
                    return scope.symbols.get(&name).copied();
                }
            }
        }

        for scope in self.scopes.iter().rev() {
            if let Some(def_id) = scope.symbols.get(&name) {
                let def_id = *def_id;

                // Only allow an identifier to be fetched if we haven't
                // restricted our search to pervasive identifiers, or any
                // pervasive identifiers
                if !restrict_to_pervasive || self.is_pervasive(def_id) {
                    return Some(def_id);
                }
            }

            if scope.kind.import_boundary().only_pervasive()
                || (matches!(lookup_kind, LookupKind::OnDef) && scope.kind.allows_shadowing())
            {
                // First case: Crossing an explicit import boundary
                // Restrict search to pervasive identifiers after import boundaries
                //
                // Second case: Part of redeclaration checking, allows shadowing
                // Pervasive identifiers are the only things that can't be shadowed
                restrict_to_pervasive = true;
            }
        }

        // No identifier found
        None
    }

    fn boundary_scope(scopes: &mut [Scope]) -> &mut Scope {
        for scope in scopes.iter_mut().rev() {
            if scope.kind.import_boundary().only_pervasive() {
                return scope;
            }
        }

        // Root scope is always an import boundary
        unreachable!();
    }
}

impl Default for ScopeTracker {
    fn default() -> Self {
        Self::new()
    }
}
