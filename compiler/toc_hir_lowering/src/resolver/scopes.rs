//! Scope building
//!
//! Only keeps track of [`LocalDefId`]s
//!
//! [`LocalDefId`]: symbol::LocalDefId
#[cfg(test)]
mod test;

use std::collections::{HashMap, HashSet};

use toc_hir::symbol::{self, DefMap, LocalDefId, Symbol};

/// How a symbol is brought into scope
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeclareKind {
    /// The symbol is undeclared at the point of definition.
    Undeclared,
    /// The symbol is a normal declaration at the point of definition.
    Declared,
    /// This symbol always occludes previous declarations, including
    /// pervasive declarations.
    AlwaysShadow,
    /// The symbol is declared, but is only usable in certain contexts
    LimitedDeclared(LimitedKind),
    /// The symbol is a forward reference to a later declaration,
    /// with a [`LocalDefId`] pointing to the resolving definition.
    Forward(ForwardKind, Option<LocalDefId>),
    /// The symbol is a resolution of a forward declaration.
    Resolved(ForwardKind),

    // TODO: We only care about where it's exported from, attrs can come later (library local export table?)
    /// The symbol is from an export of an item, with a [`LocalDefId`]
    /// pointing to the original item.
    ItemExport(LocalDefId),

    // TODO: Shunt this info into a libray local import table/resolution map?
    /// The symbol is of an imported item, optionally with a [`LocalDefId`]
    /// pointing to the original item, or `None` if there isn't one.
    ItemImport(Option<LocalDefId>),

    /// The symbol an unqualified export from an import.
    /// [`LocalDefId`]s point to `(original_import, original_export)`
    UnqualifiedImport(LocalDefId, LocalDefId),
}

/// Disambiguates between different forward declaration kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ForwardKind {
    /// `type` forward declaration
    Type,
    /// `procedure` forward declaration
    // Only constructed in tests right now
    _Procedure,
}

/// Specificity on why a symbol is limited in visibility
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LimitedKind {
    /// Only usable in post-condition statements
    PostCondition,
}

#[derive(Debug)]
pub(crate) struct Scope {
    /// What kind of this scope this is
    kind: ScopeKind,
    /// All symbols declared in a scope.
    symbols: HashMap<Symbol, symbol::LocalDefId>,
    /// Any symbols within this scope that relate to a forward declaration.
    forward_symbols: HashMap<Symbol, (ForwardKind, Vec<symbol::LocalDefId>)>,
    /// All undeclared symbols
    undecl_symbols: HashSet<Symbol>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeKind {
    Root,
    #[allow(dead_code)] // Only constructed in tests for now
    Module,
    Block,
    Loop,
    Subprogram,
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
            ScopeKind::Subprogram => ImportBoundary::Implicit,
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
            undecl_symbols: HashSet::new(),
        }
    }

    fn def_in(&mut self, name: Symbol, def_id: symbol::LocalDefId) {
        self.symbols.insert(name, def_id);
    }

    /// Adds a symbol to this scope's undeclared list.
    ///
    /// Returns `true` if this is the first time it's being inserted.
    fn undecl_def_in(&mut self, name: Symbol) -> bool {
        self.undecl_symbols.insert(name)
    }
}

#[derive(Debug)]
pub(crate) struct ScopeTracker {
    scopes: Vec<Scope>,
    declare_kinds: DefMap<DeclareKind>,
    pervasive_defs: DefMap<()>,
}

impl ScopeTracker {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new(ScopeKind::Root)],
            declare_kinds: Default::default(),
            pervasive_defs: Default::default(),
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
        self.pervasive_defs.get(def_id).is_some()
    }

    pub fn declare_kind(&self, def_id: symbol::LocalDefId) -> DeclareKind {
        self.declare_kinds
            .get(def_id)
            .copied()
            .unwrap_or(DeclareKind::Undeclared)
    }

    pub fn declare_kind_mut(&mut self, def_id: symbol::LocalDefId) -> Option<&mut DeclareKind> {
        self.declare_kinds.get_mut(def_id)
    }

    /// Bring the definition into scope with the name `name`
    ///
    /// # Returns
    /// The definition in scope that previously held this `name`, if present
    pub fn def_sym(
        &mut self,
        name: Symbol,
        def_id: symbol::LocalDefId,
        kind: DeclareKind,
        is_pervasive: bool,
    ) -> Option<symbol::LocalDefId> {
        use std::collections::hash_map::Entry;

        let last_def = {
            let last_def = self.lookup_def(name, LookupKind::OnDef);

            if matches!(kind, DeclareKind::AlwaysShadow) {
                // Last def is only applicable if there's already a def in
                // the current scope
                self.scopes
                    .last()
                    .and_then(|scope| last_def.filter(|_| scope.symbols.contains_key(&name)))
            } else {
                last_def
            }
        };
        let def_scope = self.scopes.last_mut().unwrap();
        def_scope.def_in(name, def_id);

        self.declare_kinds.insert(def_id, kind);

        if is_pervasive {
            self.pervasive_defs.insert(def_id, ());
        }

        // Update the forward decl list
        match kind {
            DeclareKind::Forward(forward_kind, _) => {
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
            DeclareKind::Declared => {
                // Remove it completely, leaving any forward decls unresolved
                def_scope.forward_symbols.remove(&name);
            }
            _ => (),
        }

        last_def
    }

    /// Looks up the given def named `name`, or returning [`None`] if it doesn't exist
    pub fn use_sym(&mut self, name: Symbol) -> Option<symbol::LocalDefId> {
        self.lookup_def(name, LookupKind::OnUse)
    }

    /// Tests if this is the first use of an undeclared identifier in this scope.
    ///
    /// This is not an idempotent operation, so subsequent calls
    /// will return false, even if the first one returned true.
    pub fn is_first_undecl_use(&mut self, name: Symbol) -> bool {
        // ???: Do we still need to declare undecl's at the boundary scope?
        // We still hoist to an import boundary scope because we still want
        // to deduplicate undeclared identifier errors. It's still a question
        // of whether or not we want to extend it to any import boundary
        // (including soft boundaries), but the way we do it right now is
        // alright enough.
        Self::boundary_scope(&mut self.scopes).undecl_def_in(name)
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
