//! Scope building
//!
//! Only keeps track of [`LocalDefId`]s
//!
//! [`LocalDefId`]: symbol::LocalDefId
#[cfg(test)]
mod test;

use std::collections::{HashMap, HashSet};

use toc_hir::symbol;

#[derive(Debug)]
pub(crate) struct Scope {
    /// All symbols declared in a scope.
    symbols: HashMap<String, symbol::LocalDefId>,
    /// If the scope is an import boundary.
    ///
    /// An import boundary only allows pervasive identifiers to be implicitly
    /// imported.
    is_import_boundary: bool,
}

impl Scope {
    fn new(is_import_boundary: bool) -> Self {
        Self {
            symbols: HashMap::new(),
            is_import_boundary,
        }
    }

    fn def_in(&mut self, name: &str, def_id: symbol::LocalDefId) {
        self.symbols.insert(name.to_string(), def_id);
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
            scopes: vec![Scope::new(true)],
            pervasive_tracker: HashSet::default(),
        }
    }

    /// Wraps symbol definitions inside of a new scope
    ///
    /// Only used for tests
    #[cfg(test)]
    fn with_scope<F: FnOnce(&mut Self) -> R, R>(&mut self, is_import_boundary: bool, f: F) -> R {
        self.scopes.push(Scope::new(is_import_boundary));
        let ret = f(self);
        self.scopes.pop();

        ret
    }

    pub fn push_scope(&mut self, is_import_boundary: bool) {
        self.scopes.push(Scope::new(is_import_boundary));
    }

    pub fn pop_scope(&mut self) {
        debug_assert!(self.scopes.len() > 1, "Cannot pop off root scope");
        self.scopes.pop();
    }

    pub fn def_sym(
        &mut self,
        name: &str,
        def_id: symbol::LocalDefId,
        is_pervasive: bool,
    ) -> symbol::LocalDefId {
        self.scopes.last_mut().unwrap().def_in(name, def_id);

        if is_pervasive {
            self.pervasive_tracker.insert(def_id);
        }

        def_id
    }

    pub fn use_sym(
        &mut self,
        name: &str,
        or_undeclared: impl FnOnce() -> symbol::LocalDefId,
    ) -> symbol::LocalDefId {
        self.lookup_def(name).unwrap_or_else(|| {
            // Declare at the import boundary
            let def_id = or_undeclared();
            Self::boundary_scope(&mut self.scopes).def_in(name, def_id);
            def_id
        })
    }

    /// Looks up a DefId, with respect to scoping rules
    fn lookup_def(&self, name: &str) -> Option<symbol::LocalDefId> {
        // Top-down search through all scopes for a DefId
        let mut restrict_to_pervasive = false;

        for scope in self.scopes.iter().rev() {
            if let Some(def_id) = scope.symbols.get(name) {
                let def_id = *def_id;

                // Only allow an identifier to be fetched if we haven't
                // restricted our search to pervasive identifiers, or any
                // pervasive identifiers
                if !restrict_to_pervasive || self.pervasive_tracker.contains(&def_id) {
                    return Some(def_id);
                }
            }

            if scope.is_import_boundary {
                // Crossing an import boundary
                // Restrict search to pervasive identifiers after import boundaries
                restrict_to_pervasive = true;
            }
        }

        // No identifier found
        None
    }

    fn boundary_scope(scopes: &mut Vec<Scope>) -> &mut Scope {
        for scope in scopes.iter_mut().rev() {
            if scope.is_import_boundary {
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
