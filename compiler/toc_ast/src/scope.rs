use crate::ast::{IdentId, Identifier};
use crate::block::BlockKind;
use crate::types::TypeRef;
use toc_core::Location;

use std::collections::{HashMap, HashSet};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(dead_code)] // No variants constructed right now, deal with it later
enum ImportBoundary {
    None,
    Implicit,
    Hard,
}

/// A scoping block for identifiers
#[derive(Debug, Clone)]
pub struct ScopeBlock {
    /// Variant of block
    kind: BlockKind,
    /// Import boundary conditions
    import_boundary: ImportBoundary,
    /// Mapping of Strings to IdentId's, grouped by block.
    /// Each later mapping in the list builds upon mappings from the previous one.
    ///
    /// If a declaration here shadows another identifier declared before, the `bool`
    /// is marked as true.
    id_mappings: HashMap<String, (IdentId, bool)>,
    /// List of all identifiers shadowed in this scope.
    /// Also includes local definitions shadowed by other local definitions
    shadowed_by: Vec<(IdentId, IdentId)>,
    /// All identifiers used that were undeclared in this scope.
    /// Used both for reporting undeclared identifiers, and also resolving imports.
    undeclared_ids: HashSet<IdentId>,
    /// All identifiers used in the scope.
    /// Used for keeping track of imports
    used_ids: HashSet<IdentId>,
}

impl ScopeBlock {
    /// Creates a new scope block
    pub fn new(kind: BlockKind) -> Self {
        Self {
            kind,
            import_boundary: ImportBoundary::None,
            id_mappings: HashMap::new(),
            shadowed_by: vec![],
            undeclared_ids: HashSet::new(),
            used_ids: HashSet::new(),
        }
    }

    /// Gets an identifier id for an identifier declared in this scope block.
    ///
    /// If `None`, then the identifier is either imported, or undeclared beforehand.
    pub fn get_ident_id(&self, name: &str) -> Option<IdentId> {
        self.id_mappings.get(name).map(|(id, _)| *id)
    }

    /// Gets if a given identifier is shadowed in this scope block.
    pub fn is_ident_shadowed(&self, name: &str) -> bool {
        self.id_mappings
            .get(name)
            .map(|(_, is_shadowed)| *is_shadowed)
            .unwrap_or(false)
    }

    /// Gets an iterator over all of the identifiers shadowed in this scope block.
    /// The ordering is (old_id, new_id)
    pub fn shadowed_idents(&self) -> impl std::iter::Iterator<Item = &(IdentId, IdentId)> {
        self.shadowed_by.iter()
    }

    /// Gets an iterator over all of the identifiers declared in this scope block.
    pub fn declared_idents(&self) -> impl std::iter::Iterator<Item = &IdentId> {
        self.id_mappings.values().map(|(id, _)| id)
    }

    /// Gets an iterator over all of the undeclared identifiers used in this scope block.
    pub fn undeclared_idents(&self) -> impl std::iter::Iterator<Item = &IdentId> {
        self.undeclared_ids.iter()
    }

    /// Gets an iterator over all of the used identifiers used in this scope block.
    pub fn used_idents(&self) -> impl std::iter::Iterator<Item = &IdentId> {
        self.used_ids.iter()
    }

    /// Removes an identifier from the undeclared set of identifiers.
    /// Used when an undeclared identifier is really an import, and is therefore
    /// declared elsewhere.
    pub fn remove_undeclared_id(&mut self, id: &IdentId) {
        self.undeclared_ids.remove(id);
    }

    /// Declares an identifier in the scope, indicating whether it shadows
    /// another identifier.
    fn declare_ident(&mut self, name: String, new_id: IdentId, is_shadowing: bool) {
        self.id_mappings.insert(name, (new_id, is_shadowing));
    }

    /// Adds an identifier usage to this block.
    fn use_ident(&mut self, used_id: IdentId) {
        self.used_ids.insert(used_id);
    }

    /// Adds a shadowed by entry to the ScopeBlock
    fn add_shadowed_ident(&mut self, shadowed: IdentId, shadowed_by: IdentId) {
        self.shadowed_by.push((shadowed, shadowed_by));
    }
}

/// The root scope for a unit.
/// Handles lower scope groups and stuff, but only gives out IdentId's.
///
/// A block is a collection of identifiers.
#[derive(Debug)]
pub struct UnitScope {
    /// Next IdentId spot
    next_ident_id: u32,
    /// Mapping of IdentId's to Identifiers
    ident_ids: HashMap<IdentId, Identifier>,
    /// Scoping blocks
    blocks: Vec<ScopeBlock>,
    /// Current scope depth
    scope_depth: usize,
}

impl UnitScope {
    /// Creates a new scope group
    pub fn new() -> Self {
        // BlockKind::Unit and BlockKind::Main share the same properties, so it doesn't matter too much
        let kind = BlockKind::Main;

        Self {
            next_ident_id: 0,
            ident_ids: HashMap::new(),
            blocks: vec![ScopeBlock::new(kind)],
            scope_depth: 0,
        }
    }

    /// Pushes an identifier scope block.
    pub fn push_block(&mut self, block_kind: BlockKind) {
        // Increase scope depth
        self.scope_depth = self
            .scope_depth
            .checked_add(1)
            .expect("Too many scopes pushed");

        // Push new block
        let block = ScopeBlock::new(block_kind);
        self.blocks.push(block);
    }

    /// Pops the last identifier scope block, restoring state.
    ///
    /// # Panics
    /// If the only block present is the root block, a panic from an assertion is produced,
    /// as it is an error to pop off the root block
    ///
    /// # Returns
    /// Returns the scope block associated with the last scope, as it is no longer needed
    pub fn pop_block(&mut self) -> ScopeBlock {
        // Decrease scope depth
        self.scope_depth = self
            .scope_depth
            .checked_sub(1)
            .expect("Cannot pop off root block");

        // Pop it!
        self.blocks.pop().expect("Cannot pop off root block")
    }

    /// Declares an identifier in the current block under a given name.
    ///
    /// If an identifier is already declared under this given name, or is accessable in a higher block,
    /// the previous definition replaces the old definition within the boundaries of the block.
    ///
    /// # Parameters
    /// - `name`: The name of the new identifier
    /// - `decl_location`: The location where the identifier is declared
    /// - `type_spec`: The type specification for the identifier
    /// - `is_const`: If the new identifier isn't mutable at runtime
    /// - `is_typedef`: If the new identifier is for a type definition
    /// - `is_pervasive`: If the identifier definition is allowed to be implicitly imported through hard
    ///                   import boundaries.
    ///
    /// # Returns
    /// Returns an `IdentId` corresponding to the new definition of the identifier
    pub fn declare_ident(
        &mut self,
        name: String,
        decl_location: Location,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
        is_pervasive: bool,
    ) -> IdentId {
        self._declare_ident(
            name,
            decl_location,
            type_spec,
            is_const,
            is_typedef,
            is_pervasive,
            true,
        )
    }

    /// Uses an identifier.
    ///
    /// Should an identifier not be declared at this point, a new identifier is declared within the current block.
    ///
    /// `use_ident` relies on `get_ident_id` for IdentId lookup. See `get_ident_id` for more info on lookup rules.
    ///
    /// # Parameters
    /// - `name`: The name of the identifier to use
    /// - `use_location`: The location where the identifier is used
    ///
    /// # Returns
    /// Returns the IdentId referencing this identifier.
    pub fn use_ident(&mut self, name: &str, use_location: Location) -> IdentId {
        let use_id = if let Some(use_id) = self.get_ident_id(name) {
            use_id
        } else {
            // Declare a new undeclared identifier
            let name = name.to_string();

            // ???: Which block should an unused identifier be located in?
            // Should be at the frontier of the import scope, passing through soft boundaries
            // and stopping at hard boundaries (even if pervasive)
            self._declare_ident(
                name,
                use_location,
                TypeRef::TypeError,
                false,
                false,
                false,
                false,
            )
        };

        // Increment the usage count
        self.ident_ids
            .entry(use_id)
            .and_modify(|ident| ident.usages = ident.usages.saturating_add(1));

        // Keep track of used identifiers
        self.current_block_mut().use_ident(use_id);

        use_id
    }

    /// Gets the IdentId for a given identifier name.
    /// Always gives the most recent IdentId for a given name.
    ///
    /// Currently, not all import rules are followed.
    /// (todo: explain import boundaries & pervasive identifiers here or in design doc)
    ///
    /// # Parameters
    /// - `name`: The name of the identifier to lookup
    ///
    /// # Returns
    /// Returns `Some(IdentId)` if an identifier has been declared, or `None` otherwise.
    pub fn get_ident_id(&self, name: &str) -> Option<IdentId> {
        // Top-down search through all blocks for an id
        for block in self.blocks.iter().rev() {
            if let Some(id) = block.id_mappings.get(name) {
                return Some(id.0);
            }
        }

        // TODO(resolver): Deal with pervasive import rules

        // No identifier found
        None
    }

    /// Gets the associated `Identifier` info for a given IdentId
    ///
    /// # Panics
    /// Will panic with "No Identifier for given IdentId" if the given IdentId has no associated `Identifier`.
    ///
    /// # Parameters
    /// - `id`: The IdentId to use for looking up the identifier info
    ///
    /// # Returns
    /// Returns the associated identifier info
    pub fn get_ident_info(&self, id: &IdentId) -> &Identifier {
        self.ident_ids
            .get(id)
            .expect("No Identifier for given IdentId")
    }

    /// Gets a mutable reference to the associated `Identifier` info for a given IdentId
    ///
    /// # Panics
    /// Will panic with "No Identifier for given IdentId" if the given IdentId has no associated `Identifier`.
    ///
    /// # Parameters
    /// - `id`: The IdentId to use for looking up the identifier info
    ///
    /// # Returns
    /// Returns the mutable reference to the associated identifier info
    pub fn get_ident_info_mut(&mut self, id: &IdentId) -> &mut Identifier {
        self.ident_ids
            .get_mut(id)
            .expect("No Identifier for given IdentId")
    }

    /// Fetches the highest level ScopeBlock
    pub fn current_block(&self) -> &ScopeBlock {
        self.blocks.last().expect("No blocks to fetch")
    }

    fn current_block_mut(&mut self) -> &mut ScopeBlock {
        self.blocks.last_mut().expect("No blocks to fetch")
    }

    /// Makes a new identifier id
    fn make_ident_id(&mut self) -> IdentId {
        let new_id = self.next_ident_id;

        // If there are too many identifier ids, then die.
        // can't have more than 4 billion declarations
        self.next_ident_id = self
            .next_ident_id
            .checked_add(1)
            .expect("Too many declared identifiers");

        IdentId(new_id)
    }

    /// Same as `UnitScope::declare_ident`, but allows for declaring "undeclared" identifiers
    #[allow(clippy::too_many_arguments)] // Is an internal function, don't care about this
    fn _declare_ident(
        &mut self,
        name: String,
        decl_location: Location,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
        is_pervasive: bool,
        as_declared: bool,
    ) -> IdentId {
        let new_id = self.make_ident_id();

        let info = Identifier::new(
            decl_location,
            type_spec,
            name.clone(),
            is_const,
            is_typedef,
            as_declared,
            is_pervasive,
        );

        // TODO(resolver): Handle pervasive identifiers

        // Check if an identifer is being shadowed by this declaration
        let old_id = self.get_ident_id(&name);
        let is_shadowing = old_id.is_some();

        // Replace the old mapping
        self.current_block_mut()
            .declare_ident(name, new_id, is_shadowing);

        if let Some(old_id) = old_id {
            // Add shadowed entry
            self.current_block_mut().add_shadowed_ident(old_id, new_id);
        }

        assert!(self.ident_ids.insert(new_id, info).is_none()); // Must be a new entry

        new_id
    }
}

impl Default for UnitScope {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::block::BlockKind;
    use crate::types::PrimitiveType;
    use toc_core::Location;

    #[test]
    fn test_ident_declare_use() {
        // declare | usage
        let mut unit_scope = UnitScope::new();

        let id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
            false,
        );
        let info = unit_scope.get_ident_info(&id);
        assert_eq!(info.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        let id = unit_scope.use_ident("a", Location::new());
        let info = unit_scope.get_ident_info(&id);
        assert_eq!(info.type_spec, TypeRef::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn test_ident_redeclare() {
        let mut unit_scope = UnitScope::new();

        // First decl, pass
        let initial_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
            false,
        );

        // Redecl, have different ids
        let redeclare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::String_),
            false,
            false,
            false,
        );

        // Ensure that we're keeping track of the identifiers
        assert_eq!(
            unit_scope.get_ident_info(&redeclare_id).type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_eq!(
            unit_scope.get_ident_info(&initial_id).type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        assert_ne!(initial_id, redeclare_id);
    }

    #[test]
    fn test_ident_declare_shadow() {
        // Identifier shadowing is not allow within inner scopes, but is detected later on
        let mut unit_scope = UnitScope::new();

        // Outer declare
        let declare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
            false,
        );
        assert_eq!(
            unit_scope.get_ident_info(&declare_id).type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // Inner declare
        unit_scope.push_block(BlockKind::InnerBlock);
        {
            let shadow_id = unit_scope.declare_ident(
                String::from("a"),
                Location::new(),
                TypeRef::Primitive(PrimitiveType::Real),
                false,
                false,
                false,
            );

            assert_eq!(
                unit_scope.get_ident_info(&shadow_id).type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );

            // Identifiers should be different
            assert_ne!(shadow_id, declare_id);

            // Should be marked as shadowing things...
            assert!(unit_scope.current_block().is_ident_shadowed("a"));
            // And have a shadowing entry
            assert!(unit_scope.current_block().shadowed_idents().count() > 0);
        }
        unit_scope.pop_block();

        // But not in the root scope
        assert!(!unit_scope.current_block().is_ident_shadowed("a"));
        assert!(unit_scope.current_block().shadowed_idents().count() == 0);
    }

    #[test]
    fn test_ident_declare_no_shadow() {
        // Declaring outer after inner scopes should not cause issues
        let mut unit_scope = UnitScope::new();

        // Inner declare
        unit_scope.push_block(BlockKind::InnerBlock);
        let shadow_id = {
            let shadow_id = unit_scope.declare_ident(
                String::from("a"),
                Location::new(),
                TypeRef::Primitive(PrimitiveType::Real),
                false,
                false,
                false,
            );
            assert_eq!(
                unit_scope.get_ident_info(&shadow_id).type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );

            // No shadowing should be done
            assert!(!unit_scope.current_block().is_ident_shadowed("a"));
            shadow_id
        };
        unit_scope.pop_block();

        // Outer declare
        let declare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
            false,
        );

        assert_eq!(
            unit_scope.get_ident_info(&declare_id).type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // No shadowing should be done
        assert!(!unit_scope.current_block().is_ident_shadowed("a"));
        // Identifiers should be different
        assert_ne!(shadow_id, declare_id);
    }

    #[test]
    fn test_resolve_defined() {
        let mut unit_scope = UnitScope::new();

        let ident = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Unknown,
            false,
            false,
            false,
        );
        // Should stay as TypeRef::Unknown
        assert_eq!(
            unit_scope.get_ident_info(&ident).type_spec,
            TypeRef::Unknown
        );

        // Change up the info
        {
            let info = unit_scope.get_ident_info_mut(&ident);
            info.type_spec = TypeRef::Primitive(PrimitiveType::String_);
        }

        // Same reference should use a new identifier
        assert_eq!(
            unit_scope.get_ident_info(&ident).type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
    }

    #[test]
    #[should_panic(expected = "No Identifier for given IdentId")]
    fn test_resolve_undefined() {
        let mut unit_scope = UnitScope::new();
        // Panics!
        let _info = unit_scope.get_ident_info_mut(&IdentId(0));
    }

    #[test]
    fn test_use_undefined() {
        let mut unit_scope = UnitScope::new();

        let ident = unit_scope.use_ident("a", Location::new());
        // Should be the first identifier
        assert_eq!(ident, IdentId(0));

        let info = unit_scope.get_ident_info(&ident);
        // Should be undeclared & TypeRef::TypeError
        assert_eq!(info.type_spec, TypeRef::TypeError);
        assert_eq!(info.is_declared, false);
    }

    #[test]
    fn test_use_import() {
        // External identifiers should be imported into the current scope (i.e share the same IdentId)
        let mut unit_scope = UnitScope::new();

        // Root declare
        let declare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
            false,
        );
        assert_eq!(
            unit_scope.get_ident_info(&declare_id).type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // Inner use
        unit_scope.push_block(BlockKind::InnerBlock);
        {
            let import_id = unit_scope.use_ident("a", Location::new());

            assert_eq!(
                unit_scope.get_ident_info(&import_id).type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
            // Should use the same id
            assert_eq!(import_id, declare_id);
        }
        unit_scope.pop_block();
    }
}
