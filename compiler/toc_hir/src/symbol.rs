//! Everything related to symbols.
//! `SymbolTable` construction with respect to scoping rules occurs in `toc_hir_lowering`.

use std::fmt;

use indexmap::IndexMap;
use toc_span::Span;

use crate::unit::UnitId;

/// Definition of an identifier within a unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId(usize);

impl DefId {
    pub fn into_global(self, unit: UnitId) -> GlobalDefId {
        GlobalDefId(unit, self)
    }

    /// Creates a new `DefId`
    ///
    /// Only to be used in testing
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

/// Use of an identifier within a unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UseId(DefId, usize);

impl UseId {
    pub fn as_def(self) -> DefId {
        self.0
    }

    pub fn into_global(self, unit: UnitId) -> GlobalUseId {
        GlobalUseId(unit, self)
    }

    /// Creates a new `UseId`
    ///
    /// Only to be used in testing
    pub fn new(def: DefId, id: usize) -> Self {
        Self(def, id)
    }
}

/// Definition of an identifier in a specific unit.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalDefId(UnitId, DefId);

impl GlobalDefId {
    pub fn unit_id(self) -> UnitId {
        self.0
    }

    pub fn as_local(self) -> DefId {
        self.1
    }
}

impl fmt::Debug for GlobalDefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("GlobalDefId({:?}, {:?})", self.0, self.1))
    }
}

/// Use of an identifier in a specific unit
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalUseId(UnitId, UseId);

impl GlobalUseId {
    pub fn unit_id(self) -> UnitId {
        self.0
    }

    pub fn as_local(self) -> UseId {
        self.1
    }
}

impl fmt::Debug for GlobalUseId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("GlobalUseId({:?}, {:?})", self.0, self.1))
    }
}

#[derive(Debug)]
pub struct Symbol {
    /// Name of the symbol.
    pub name: String,
    /// The kind of symbol.
    pub kind: SymbolKind,
    /// If the symbol is pervasive, and can implicitly cross import boundaries.
    pub is_pervasive: bool,

    def_id: DefId,
    next_use: usize,
}

impl Symbol {
    /// Gets the uses of a symbol.
    pub fn uses(&self) -> impl Iterator<Item = UseId> + '_ {
        (0..self.next_use).map(move |id| UseId(self.def_id, id))
    }

    fn new_use(&mut self) -> UseId {
        let use_id = UseId(self.def_id, self.next_use);
        self.next_use += 1;
        use_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    /// The symbol is undeclared at the point of definition.
    Undeclared,
    /// The symbol is a normal declaration at the point of definition.
    Declared,
    /// The symbol is a forward reference to a later declaration.
    Forward,
    /// The symbol is a resolution of a forward declaration, with a `DefId`
    /// pointing back to the original forward declaration symbol.
    Resolved(DefId),
}

/// Symbol table for a given `Unit`.
///
/// Does not take care of symbol scoping rules.
#[derive(Debug)]
pub struct SymbolTable {
    defs: IndexMap<DefId, Symbol>,
    def_spans: IndexMap<DefId, Span>,
    use_spans: IndexMap<UseId, Span>,
    next_def: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            defs: IndexMap::new(),
            def_spans: IndexMap::new(),
            use_spans: IndexMap::new(),
            next_def: 0,
        }
    }

    /// Declares a new symbol.
    ///
    /// ## Parameters
    /// - `name`: The name of the symbol to define
    /// - `span`: The text span of the definition
    /// - `kind`: The kind of symbol to define
    ///
    /// ## Returns
    /// The `DefId` associated with the definition
    pub fn def_sym(
        &mut self,
        name: &str,
        span: Span,
        kind: SymbolKind,
        is_pervasive: bool,
    ) -> DefId {
        let def_id = self.new_def();

        self.defs.insert(
            def_id,
            Symbol {
                name: name.to_string(),
                kind,
                is_pervasive,
                def_id,
                next_use: 0,
            },
        );
        self.def_spans.insert(def_id, span);

        def_id
    }

    /// Uses a given symbol.
    ///
    /// ## Returns
    /// The UseId associated with the given DefId
    pub fn use_sym(&mut self, def: DefId, span: Span) -> UseId {
        let symbol = self.defs.get_mut(&def).expect("Missing symbol info");
        let use_id = symbol.new_use();

        self.use_spans.insert(use_id, span);

        use_id
    }

    pub fn get_symbol(&self, def: DefId) -> &Symbol {
        self.defs.get(&def).unwrap()
    }

    pub fn get_def_span(&self, def_id: DefId) -> Span {
        *self.def_spans.get(&def_id).unwrap()
    }

    pub fn get_use_span(&self, use_id: UseId) -> Span {
        *self.use_spans.get(&use_id).unwrap()
    }

    fn new_def(&mut self) -> DefId {
        let def_id = DefId(self.next_def);
        self.next_def += 1;
        def_id
    }

    pub fn iter_defs(&self) -> impl Iterator<Item = (DefId, Span, &Symbol)> {
        self.defs.iter().map(move |(k, v)| {
            let span = self.def_spans.get(k).unwrap();
            (*k, *span, v)
        })
    }

    pub fn iter_uses(&self) -> impl Iterator<Item = (UseId, Span)> + '_ {
        self.use_spans.iter().map(|(k, v)| (*k, *v))
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
