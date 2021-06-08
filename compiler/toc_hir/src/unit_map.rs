//! Unit map & related structures

use std::ops::Index;

use crate::symbol::SymbolTable;
use crate::{stmt, Database, Unit};

/// Id of a unit
///
/// This is only constructed inside of a `UnitMapBuilder`
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct UnitId(usize);

/// Helper for constructing a UnitMap
///
/// The first unit added is defined to be the root unit.
#[derive(Debug)]
pub struct UnitMapBuilder {
    units: Vec<Unit>,
}

impl UnitMapBuilder {
    pub fn new() -> Self {
        Self { units: vec![] }
    }

    /// Adds a new unit to the unit map
    ///
    /// Also provides a unit with it's associated unit id
    pub fn add_unit(
        &mut self,
        database: Database,
        stmts: Vec<stmt::StmtId>,
        symbol_table: SymbolTable,
    ) -> UnitId {
        let id = UnitId(self.units.len());

        let unit = Unit {
            id,
            database,
            stmts,
            symbol_table,
        };

        self.units.push(unit);

        id
    }

    /// Constructs the final `UnitMap`
    pub fn finish(self) -> UnitMap {
        UnitMap { units: self.units }
    }
}

impl Default for UnitMapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Map of all of the units in the program.
/// Units are accessed either through the indexing operator, or
/// by the `get_unit` method.
///
/// This is an immutable structure, generated by `UnitMapBuilder`.
#[derive(Debug)]
pub struct UnitMap {
    units: Vec<Unit>,
}

impl UnitMap {
    /// Gets an iterator over all of the units
    ///
    /// The first unit to be given is defined to be the root unit
    pub fn units(&self) -> UnitIterator<'_> {
        UnitIterator::new(self.units.iter())
    }

    /// Gets the unit associated with the given id
    pub fn get_unit(&self, id: UnitId) -> &Unit {
        // The unit id's index is guaranteed to be in bounds
        // Using `assert` for delegating bounds checking to here
        assert!(id.0 < self.units.len());
        &self.units[id.0]
    }
}

impl Index<UnitId> for UnitMap {
    type Output = Unit;

    fn index(&self, index: UnitId) -> &Self::Output {
        &self.units[index.0]
    }
}

type UnitIter<'u> = std::slice::Iter<'u, Unit>;

#[derive(Debug, Clone)]
pub struct UnitIterator<'u> {
    iter: UnitIter<'u>,
}

impl<'u> UnitIterator<'u> {
    fn new(iter: UnitIter<'u>) -> Self {
        Self { iter }
    }
}

impl<'u> Iterator for UnitIterator<'u> {
    type Item = &'u Unit;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}
