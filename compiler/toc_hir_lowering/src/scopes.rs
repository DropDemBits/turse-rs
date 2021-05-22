//! Scope building
#[cfg(test)]
mod test;

use std::collections::HashMap;

use toc_hir::symbol::{self, SymbolTable};
use toc_span::Span;

#[derive(Debug)]
pub(crate) struct Scope {
    /// All symbols declared in a scope.
    symbols: HashMap<String, symbol::DefId>,
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
}

#[derive(Debug)]
pub(crate) struct ScopeBuilder {
    symbol_table: SymbolTable,
    scopes: Vec<Scope>,
}

impl ScopeBuilder {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            scopes: vec![Scope::new(true)],
        }
    }

    pub fn finish(self) -> SymbolTable {
        self.symbol_table
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
        span: Span,
        kind: symbol::SymbolKind,
        is_pervasive: bool,
    ) -> symbol::DefId {
        Self::def_in_scope(
            &mut self.symbol_table,
            self.scopes.last_mut().unwrap(),
            name,
            span,
            kind,
            is_pervasive,
        )
    }

    pub fn use_sym(&mut self, name: &str, span: Span) -> symbol::UseId {
        let def_id = self.lookup_def(name).unwrap_or_else(|| {
            // Declare at the import boundary
            let def_id = Self::def_in_scope(
                &mut self.symbol_table,
                Self::boundary_scope(&mut self.scopes),
                name,
                span,
                symbol::SymbolKind::Undeclared,
                false,
            );
            def_id
        });

        self.symbol_table.use_sym(def_id, span)
    }

    fn def_in_scope(
        symbol_table: &mut SymbolTable,
        scope: &mut Scope,
        name: &str,
        span: Span,
        kind: symbol::SymbolKind,
        is_pervasive: bool,
    ) -> symbol::DefId {
        let def_id = symbol_table.def_sym(name, span, kind, is_pervasive);
        scope.symbols.insert(name.to_string(), def_id);
        def_id
    }

    /// Looks up a DefId, with respect to scoping rules
    fn lookup_def(&self, name: &str) -> Option<symbol::DefId> {
        // Top-down search through all scopes for a DefId
        let mut restrict_to_pervasive = false;

        for scope in self.scopes.iter().rev() {
            if let Some(def_id) = scope.symbols.get(name) {
                let def_id = *def_id;

                // Only allow an identifier to be fetched if we haven't
                // restricted our search to pervasive identifiers, or any
                // pervasive identifiers
                if !restrict_to_pervasive || self.symbol_table.get_symbol(def_id).is_pervasive {
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
