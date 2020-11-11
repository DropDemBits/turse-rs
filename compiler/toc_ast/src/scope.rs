use crate::ast::ident::{IdentId, Identifier, RefKind};
use crate::block::BlockKind;
use crate::types::TypeRef;
use toc_core::Location;

use std::collections::{HashMap, HashSet};

// TODO(was_doing): import boundaries & pervasiveness

/// All import boundary variants
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[allow(dead_code)] // No variants constructed right now, deal with it later
enum ImportBoundary {
    /// All identifiers are allowed to be imported from parent blocks
    None,
    /// Only explicitly imported identifiers and pervasive identifiers
    /// are allowed to be imported from parent blocks
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
    ///
    /// # Parameters
    /// - `kind`: The kind of block to create. Determines the import boundary.
    pub fn new(kind: BlockKind) -> Self {
        let import_boundary = match kind {
            BlockKind::Unit
            | BlockKind::Main
            | BlockKind::Module
            | BlockKind::Monitor
            | BlockKind::MonitorClass
            | BlockKind::Class => ImportBoundary::Hard,
            _ => ImportBoundary::None,
        };

        Self {
            kind,
            import_boundary,
            id_mappings: HashMap::new(),
            shadowed_by: vec![],
            undeclared_ids: HashSet::new(),
            used_ids: HashSet::new(),
        }
    }

    /// Gets the block kind of the current ScopeBlock
    pub fn kind(&self) -> &BlockKind {
        &self.kind
    }

    /// Gets an identifier id for an identifier declared in this scope block.
    ///
    /// If `None`, then the identifier is either imported, or undeclared beforehand.
    pub fn get_ident_id(&self, name: &str) -> Option<IdentId> {
        self.id_mappings.get(name).map(|(id, _)| *id)
    }

    /// Gets if a given identifier name is shadowed in this scope block.
    pub fn is_ident_shadowed(&self, name: &str) -> bool {
        self.id_mappings
            .get(name)
            .map_or(false, |(_, is_shadowed)| *is_shadowed)
    }

    /// Gets an iterator over all of the identifiers shadowed in this scope block.
    /// The ordering is (`old_id`, `new_id`)
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
    pub fn remove_undeclared_id(&mut self, id: IdentId) {
        self.undeclared_ids.remove(&id);
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

    /// Adds a shadowed by entry to the `ScopeBlock`
    fn add_shadowed_ident(&mut self, shadowed: IdentId, shadowed_by: IdentId) {
        self.shadowed_by.push((shadowed, shadowed_by));
    }
}

/// The root scope for a unit.
/// Handles lower scope groups and stuff, but only gives out `IdentId`'s.
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
        Self {
            next_ident_id: 0,
            ident_ids: HashMap::new(),
            blocks: vec![],
            scope_depth: 0,
        }
    }

    /// Pushes an identifier scope block.
    ///
    /// # Parameters
    /// - `block_kind`: The kind of scope block to push.
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
    /// If there are no blocks present, a panic will occur with the message "No scopes to pop off"
    ///
    /// # Returns
    /// Returns the scope block associated with the last scope, as it is no longer needed
    pub fn pop_block(&mut self) -> ScopeBlock {
        // Decrease scope depth
        self.scope_depth = self
            .scope_depth
            .checked_sub(1)
            .expect("No scopes to pop off");

        // Pop it!
        self.blocks.pop().expect("No scopes to pop off")
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
        ref_kind: RefKind,
        is_pervasive: bool,
    ) -> IdentId {
        self._declare_ident(name, decl_location, type_spec, ref_kind, is_pervasive, true)
    }

    /// Uses an identifier.
    ///
    /// Should an identifier not be declared at this point, a new identifier is declared within the current block.
    ///
    /// `use_ident` relies on `get_ident_id` for `IdentId` lookup. See `get_ident_id` for more info on lookup rules.
    ///
    /// # Parameters
    /// - `name`: The name of the identifier to use
    /// - `use_location`: The location where the identifier is used
    ///
    /// # Returns
    /// Returns the `IdentId` referencing this identifier.
    pub fn use_ident(&mut self, name: &str, use_location: Location) -> IdentId {
        let use_id = self.get_ident_id(name).unwrap_or_else(|| {
            // Declare a new undeclared identifier
            let name = name.to_string();

            // ???: Which block should an unused identifier be located in?
            // Should be at the frontier of the import scope, passing through soft boundaries
            // and stopping at hard boundaries (even if pervasive)
            self._declare_ident(
                name,
                use_location,
                TypeRef::TypeError,
                RefKind::Var,
                false,
                false,
            )
        });

        // Increment the usage count
        self.ident_ids
            .entry(use_id)
            .and_modify(|ident| ident.usages = ident.usages.saturating_add(1));

        // Keep track of used identifiers
        self.current_block_mut().use_ident(use_id);

        use_id
    }

    /// Gets the `IdentId` for a given identifier name.
    /// Always gives the most recent `IdentId` for a given name.
    /// If the root block has been popped off, use the root block's
    /// `get_ident_id` associated method instead
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
        let mut restrict_to_pervasive = false;

        for block in self.blocks.iter().rev() {
            if let Some(id) = block.id_mappings.get(name) {
                let id = &id.0;

                // Only allow an identifier to be fetched if we haven't
                // restricted our search to pervasive identifiers, or any
                // pervasive identifiers
                if !restrict_to_pervasive || self.get_ident_info(id).is_pervasive {
                    return Some(*id);
                }
            }

            if block.import_boundary > ImportBoundary::None {
                // Crossing an import boundary
                // Restrict search to pervasive identifiers after import boundaries
                restrict_to_pervasive = true;
            }
        }

        // TODO(resolver): Deal with pervasive import rules

        // No identifier found
        None
    }

    /// Gets the associated `Identifier` info for a given `IdentId`
    ///
    /// # Panics
    /// Will panic with "No Identifier for given IdentId" if the given `IdentId` has no associated `Identifier`.
    ///
    /// # Parameters
    /// - `id`: The `IdentId` to use for looking up the identifier info
    ///
    /// # Returns
    /// Returns the associated identifier info
    pub fn get_ident_info(&self, id: &IdentId) -> &Identifier {
        self.ident_ids
            .get(id)
            .expect("No Identifier for given IdentId")
    }

    /// Gets a mutable reference to the associated `Identifier` info for a given `IdentId`
    ///
    /// # Panics
    /// Will panic with "No Identifier for given IdentId" if the given `IdentId` has no associated `Identifier`.
    ///
    /// # Parameters
    /// - `id`: The `IdentId` to use for looking up the identifier info
    ///
    /// # Returns
    /// Returns the mutable reference to the associated identifier info
    pub fn get_ident_info_mut(&mut self, id: &IdentId) -> &mut Identifier {
        self.ident_ids
            .get_mut(id)
            .expect("No Identifier for given IdentId")
    }

    /// Fetches the highest level `ScopeBlock`
    ///
    /// # Panics
    /// Will panic with "No blocks to fetch" if the root block has been popped
    /// off (e.g. after parsing)
    ///
    /// # Returns
    /// Returns a reference to the current / highest level `ScopeBlock`
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
    fn _declare_ident(
        &mut self,
        name: String,
        decl_location: Location,
        type_spec: TypeRef,
        ref_kind: RefKind,
        is_pervasive: bool,
        as_declared: bool,
    ) -> IdentId {
        let new_id = self.make_ident_id();

        let info = Identifier::new(
            decl_location,
            type_spec,
            name.clone(),
            ref_kind,
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

mod pretty_print {
    use super::{IdentId, UnitScope};
    use std::fmt;

    impl fmt::Display for UnitScope {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            // Just dump the identifier table
            f.write_str("[\n")?;
            for (id, _) in self.ident_ids.keys().enumerate() {
                let info = self
                    .ident_ids
                    .get(&IdentId(id as u32))
                    .expect("infalliable");

                f.write_fmt(format_args!("{:8} -> {}", id, info))?;
            }
            f.write_str("]")
        }
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
        unit_scope.push_block(BlockKind::Main);

        let id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            RefKind::Var,
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
        unit_scope.push_block(BlockKind::Main);

        // First decl, pass
        let initial_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            RefKind::Var,
            false,
        );

        // Redecl, have different ids
        let redeclare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::String_),
            RefKind::Var,
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
        unit_scope.push_block(BlockKind::Main);

        // Outer declare
        let declare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            RefKind::Var,
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
                RefKind::Var,
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
        unit_scope.push_block(BlockKind::Main);

        // Inner declare
        unit_scope.push_block(BlockKind::InnerBlock);
        let shadow_id = {
            let shadow_id = unit_scope.declare_ident(
                String::from("a"),
                Location::new(),
                TypeRef::Primitive(PrimitiveType::Real),
                RefKind::Var,
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
            RefKind::Var,
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
        unit_scope.push_block(BlockKind::Main);

        let ident = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Unknown,
            RefKind::Var,
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
        unit_scope.push_block(BlockKind::Main);

        // Panics!
        let _info = unit_scope.get_ident_info_mut(&IdentId(0));
    }

    #[test]
    fn test_use_undefined() {
        let mut unit_scope = UnitScope::new();
        unit_scope.push_block(BlockKind::Main);

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
        unit_scope.push_block(BlockKind::Main);

        // Root declare
        let declare_id = unit_scope.declare_ident(
            String::from("a"),
            Location::new(),
            TypeRef::Primitive(PrimitiveType::Int),
            RefKind::Var,
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

    #[test]
    #[should_panic(expected = "No scopes to pop off")]
    fn test_pop_all() {
        let mut unit_scope = UnitScope::new();
        unit_scope.pop_block();
    }

    #[test]
    fn test_import_boundaries() {
        let mut unit_scope = UnitScope::new();
        unit_scope.push_block(BlockKind::Main);

        // Declare some external identifiers
        let non_pervasive = unit_scope.declare_ident(
            "non_pervasive".to_string(),
            Default::default(),
            TypeRef::Unknown,
            RefKind::Const,
            false,
        );
        let pervasive = unit_scope.declare_ident(
            "pervasive".to_string(),
            Default::default(),
            TypeRef::Unknown,
            RefKind::Const,
            true,
        );

        // Make an inner block
        {
            unit_scope.push_block(BlockKind::InnerBlock);

            // Should be able to access both identifiers
            let inner_use_a = unit_scope.get_ident_id("non_pervasive");
            let inner_use_b = unit_scope.get_ident_id("pervasive");
            assert_eq!(inner_use_a, Some(non_pervasive));
            assert_eq!(inner_use_b, Some(pervasive));

            unit_scope.pop_block();
        }

        // Make a block with a hard import boundary
        for block_kind in &[
            BlockKind::Unit,
            BlockKind::Main,
            BlockKind::Module,
            BlockKind::Class,
            BlockKind::Monitor,
            BlockKind::MonitorClass,
        ] {
            unit_scope.push_block(*block_kind);
            // Can access even through an inner block
            unit_scope.push_block(BlockKind::InnerBlock);

            // Should only be able to access the pervasive identifiers
            let undecl_use_a = unit_scope.get_ident_id("non_pervasive");
            let imported_use_b = unit_scope.get_ident_id("pervasive");

            assert_ne!(undecl_use_a, Some(non_pervasive));
            assert_eq!(undecl_use_a, None);
            assert_eq!(imported_use_b, Some(pervasive));

            unit_scope.pop_block(); // inner block
            unit_scope.pop_block();
        }

        unit_scope.pop_block();
    }
}
