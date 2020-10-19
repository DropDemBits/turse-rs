use crate::ast::{self, IdentInstance, Identifier};
use crate::block::CodeBlock;
use crate::types::TypeRef;
use toc_core::Location;

use std::cell::RefCell;
use std::collections::HashMap;
use std::num::NonZeroU32;
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct ImportInfo {
    /// The name of the imported identifier
    pub name: String,
    /// The instance of the imported identifier, from the imported scope
    pub instance: IdentInstance,
    /// How many scopes down, relative to the global scope, the identifier was imported from
    /// Starts from 0
    pub downscopes: usize,
}

/// Common storage unit for identifiers
#[derive(Debug)]
enum IdentEntry {
    /// Single definition of an identifier.
    Single(Identifier),
    /// Multiple definitions of an identifier, with the latest definition as
    /// the last element.
    Multiple(Vec<Identifier>),
}

/// Error in the declaration or usage of an identifier.
/// Will always include an identifier as part of the error
#[derive(Debug)]
pub enum IdentError {
    /// An identifier was not declared in any scope.
    Undeclared(Identifier),
    /// An identifier was already declared in the current scope, or a parent scope.
    Redeclared(Identifier),
}

impl std::fmt::Display for IdentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentError::Undeclared(ident) => {
                f.write_fmt(format_args!("'{}' has not been declared yet", ident.name))
            }
            IdentError::Redeclared(ident) => {
                f.write_fmt(format_args!("'{}' has already been declared", ident.name))
            }
        }
    }
}

impl From<IdentError> for Identifier {
    fn from(ident_error: IdentError) -> Self {
        match ident_error {
            IdentError::Undeclared(ident) => ident,
            IdentError::Redeclared(ident) => ident,
        }
    }
}

/// Scope of identifiers
#[derive(Debug)]
pub struct Scope {
    /// Mappings for all active identifiers
    /// Identifiers are unique within a given scope.
    ///
    /// Any name conflicts are resolved by creating a new vector of identifiers
    /// and storing all definitions inside of that vector. The latest version
    /// (i.e. the last element) is used in `use_ident`, but the older
    /// declarations will be available through `get_ident_instance`.
    ///
    /// All name conflicts are stored as the validator needs the correct
    /// instance of the identifier at the point where it is visting the AST
    /// for correct type checking.
    idents: HashMap<String, IdentEntry>,
    /// Parent scopes of the current scope.
    /// First element is the global scope for the current code unit.
    /// If the vector is empty, the current scope is the global scope.
    parent_blocks: Vec<Weak<RefCell<CodeBlock>>>,
    /// Next import index into the import table
    next_import_index: u32,
    /// Import table
    import_table: Vec<ImportInfo>,
    /// If the containing block forms an import boundary
    /// Import boundaries are used to see if an identifier can be redeclared
    /// within the current scope
    is_import_boundary: bool,
}

impl Scope {
    pub fn new(parent_blocks: &[Rc<RefCell<CodeBlock>>]) -> Self {
        // Take a weak reference to the parent blocks
        // We do not need a strong reference to the parent blocks, as we do not
        // want to take ownership of them.
        // The CodeBlock / Parser is what will take ownership of
        // the blocks
        Self {
            idents: HashMap::new(),
            parent_blocks: parent_blocks
                .iter()
                .map(|scope| Rc::downgrade(scope))
                .collect(),
            next_import_index: 0,
            import_table: vec![],
            is_import_boundary: false,
        }
    }

    // We return an (Identifier, Option<String>) instead of a Result<Identifier, String>
    // because we may need to notify the user of an error, as well as creating a new identifier

    /// Declares an identifier in the current scope.
    /// If the identifier with the same name has already been declared, the new
    /// identifier definition will overwrite the old one and an IdentError::Redeclared
    /// will be produced.
    pub fn declare_ident(
        &mut self,
        at: Location,
        name: String,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
    ) -> Result<Identifier, IdentError> {
        // Check to see if the identifier exists in the current scope or within the import boundary
        let mut ident_exists = self.get_ident(&name).is_some();
        let mut external_declaration = false;

        for block_ref in self.parent_blocks.iter().rev() {
            if ident_exists {
                // Found something, either in current or parent scope
                break;
            }

            let block_ref = block_ref
                .upgrade()
                .expect("Block dropped while references still exists to it");

            ident_exists = block_ref.borrow().scope.get_ident(&name).is_some();
            // Declaration is from external to the current scope
            external_declaration = true;
        }

        let mut new_ident = Identifier::new(
            at,
            type_spec,
            name.clone(),
            is_const,
            is_typedef,
            true,
            0, // Not imported
        );

        // Make a declared identifier start from instance 1
        // Instance 0 is reserved for usage by unqualified imports and usage-before-declare
        new_ident.instance = 1;

        if ident_exists {
            // Old identifier has already been declared, either within the current
            // scope, or within the import boundary

            // import boundary:
            // - always no entry
            // redecl
            // - has entry

            if external_declaration {
                // Declaration is from an above scope and has not been imported,
                // import the entry for consistency
                let imported = self.import_ident(at, &name).unwrap();

                // This identifier is now the second instance (first after import)
                new_ident.instance = 1;

                let all_idents = vec![imported, new_ident.clone()];

                let old_value = self.idents.insert(name, IdentEntry::Multiple(all_idents));
                // Make sure that the ident was imported
                assert!(old_value.is_some());
            } else {
                // Declaration already exists, update the 'new_def's instance id
                self.idents
                    .entry(name.clone())
                    .and_modify(|old_entry| match old_entry {
                        IdentEntry::Single(old_ident) => {
                            // 2 situations:
                            // - old_ident is an import or is not declared
                            //   - new_def is still instance 1, can do existing
                            // - old_ident is a local declare, and not a use-before declare
                            //   - new_def is instance 2
                            //   - need to create a dummy entry

                            if old_ident.import_index.is_some() || !old_ident.is_declared {
                                // Old identifier is either an import, or an undeclared identifier
                                // Can create a simple list of the two
                                new_ident.instance = 1;

                                // Replace the single with a multiple
                                *old_entry = IdentEntry::Multiple(vec![
                                    old_ident.clone(),
                                    new_ident.clone(),
                                ]);
                            } else {
                                // Old identifier is a local declare, need to create a dummy entry
                                // New ident is the second instance (after dummy)
                                new_ident.instance = old_ident.instance.checked_add(1).unwrap();
                                assert_eq!(new_ident.instance, 2, "With old ident {:?}", old_ident);

                                // Create a dummy entry
                                let mut dummy_ident = old_ident.clone();
                                dummy_ident.name = String::from("<not a real entry>");
                                dummy_ident.instance = 0;
                                dummy_ident.is_declared = false;
                                dummy_ident.type_spec = TypeRef::TypeError;

                                // Replace the single with a multiple containing dummy, old, and new
                                *old_entry = IdentEntry::Multiple(vec![
                                    dummy_ident,
                                    old_ident.clone(),
                                    new_ident.clone(),
                                ]);
                            }
                        }
                        IdentEntry::Multiple(all_idents) => {
                            // Update the instance id
                            new_ident.instance = all_idents
                                .last()
                                .unwrap()
                                .instance
                                .checked_add(1)
                                .unwrap_or_else(|| panic!("Too many redeclarations of '{}'", name));

                            // Add to the existing ident list
                            all_idents.push(new_ident.clone());
                        }
                    });
            }

            // TODO: Check if the identifier is across an import boundary, to see if it can be overwritten
            Err(IdentError::Redeclared(new_ident))
        } else {
            // Declare the identifier
            let old_value = self
                .idents
                .insert(name, IdentEntry::Single(new_ident.clone()));

            assert!(old_value.is_none());

            // Defining a new identifier
            Ok(new_ident)
        }
    }

    /// Resolves the given identifier in the given scope, by modifying the
    /// identifier with the same instance.
    ///
    /// If the given identifier has not been declared in the current scope
    /// (either by importing from an external scope or by local declaration),
    /// an IdentError::Undeclared error is returned.
    ///
    /// Otherwise, an identifier should already be declared by the time this is executed
    pub fn resolve_ident(
        &mut self,
        name: &str,
        new_info: &Identifier,
    ) -> Result<Identifier, IdentError> {
        let ident_store = self.idents.get_mut(name);

        if ident_store.is_none() {
            // Given identifier is undeclared
            return Err(IdentError::Undeclared(new_info.clone()));
        }

        let ident_store = ident_store.unwrap();

        // Grab the identifier reference
        let ident = match ident_store {
            IdentEntry::Single(ident) => ident,
            IdentEntry::Multiple(all_defs) => &mut all_defs[new_info.instance as usize],
        };

        ident.type_spec = new_info.type_spec;
        ident.is_compile_eval = new_info.is_compile_eval;

        // Remove modifying these if not needed
        ident.is_const = new_info.is_const;
        ident.is_typedef = new_info.is_typedef;

        // Give back the resolved copy
        Ok(ident.clone())
    }

    /// Imports the identifier from the one of the parent scopes
    fn import_ident(&mut self, at: Location, name: &str) -> Option<Identifier> {
        // Take the identifier from the parent scopes, in reverse order
        for (downscopes, block_ref) in self.parent_blocks.iter().enumerate().rev() {
            let block_ref = block_ref
                .upgrade()
                .expect("Block dropped while references still exists to it");
            let block = block_ref.borrow();
            let parent_ident = block.scope.get_ident(name);

            if let Some(declared) = parent_ident {
                let mut reference = declared.clone();

                // Add import info entry
                let index = self.add_import_entry(ImportInfo {
                    name: name.to_string(),
                    instance: reference.instance,
                    downscopes,
                });

                // Change the location to point to the reference location
                reference.location = at;
                // Update the import index
                reference.import_index.replace(index);
                // Instance is 0 in the local scope
                reference.instance = 0;

                // Add to the local definition table
                let old_value = self
                    .idents
                    .insert(name.to_string(), IdentEntry::Single(reference.clone()));
                assert!(old_value.is_none());

                return Some(reference);
            }
        }

        // No entry found
        None
    }

    /// Uses an identifer.
    ///
    /// # Process
    /// If an identifier is not found in the current scope, it is returned from one of the parent scopes.
    /// If an identifer is found from one of the parent scopes, a new import entry is also created.
    ///
    /// Should an identifier not be found, an IdentError::Undeclared with a placeholder identer is
    /// created.
    pub fn use_ident(&mut self, at: Location, name: &str) -> Result<Identifier, IdentError> {
        if let Some(declared) = self.get_ident(name) {
            let reference = Identifier {
                // Change the location to be that of the reference location
                location: at,
                ..declared.clone()
            };

            Ok(reference)
        } else {
            // Import the identifier from the parent scopes
            let imported = self.import_ident(at, name);

            if let Some(imported_ident) = imported {
                // Return the imported identifier
                return Ok(imported_ident);
            }

            // None found, make a new one!
            let mut err_ident = Identifier::new(
                at,
                TypeRef::TypeError, // Produce a type error to propagate the error
                name.to_string(),
                false,
                false,
                false,
                0, // Not imported, just creating a new definition
            );

            // Usages before define always use instance 0
            err_ident.instance = 0;

            // Define the error entry
            let old_value = self
                .idents
                .insert(name.to_string(), IdentEntry::Single(err_ident.clone()));

            // While this should never happen on a single thread, if the
            // parser is somehow made multithreaded, then there may already be
            // a definition
            assert!(old_value.is_none());

            Err(IdentError::Undeclared(err_ident))
        }
    }

    /// Gets the latest declaration of the identifier with the given name from
    /// the current scope's identifier list
    pub fn get_ident(&self, name: &str) -> Option<&Identifier> {
        if let Some(ident_store) = self.idents.get(name) {
            match ident_store {
                IdentEntry::Single(ident) => Some(ident),
                IdentEntry::Multiple(all) => all.last(),
            }
        } else {
            None
        }
    }

    /// Get the given instance of the identifier with the given name from
    /// the current scope's identifier list
    pub fn get_ident_instance(
        &self,
        name: &str,
        instance: ast::IdentInstance,
    ) -> Option<&Identifier> {
        if let Some(ident_store) = self.idents.get(name) {
            match ident_store {
                IdentEntry::Single(ident) => {
                    // Undeclared/imported identifiers start from instance 0
                    // Declared identifiers start from instance 1
                    assert!(
                        ((!ident.is_declared || ident.import_index.is_some()) && instance == 0)
                            || (ident.is_declared && instance == 1),
                        "No additional declarations for the given identifier '{}' (instance #{})",
                        name,
                        instance
                    );
                    Some(ident)
                }
                IdentEntry::Multiple(all) => Some(&all[instance as usize]),
            }
        } else {
            None
        }
    }

    /// Adds the given entry to the import table
    /// Returns the corresponding import index (plus 1)
    fn add_import_entry(&mut self, info: ImportInfo) -> NonZeroU32 {
        self.import_table.push(info);

        self.next_import_index += 1;
        // Import index is +1 of regular index
        NonZeroU32::new(self.next_import_index).unwrap()
    }

    /// Gets the scope's import table
    pub fn import_table(&self) -> &Vec<ImportInfo> {
        &self.import_table
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::block::BlockKind;
    use crate::types::PrimitiveType;
    use toc_core::Location;

    fn make_test_block(
        block_kind: BlockKind,
        enclosing_blocks: &[Rc<RefCell<CodeBlock>>],
    ) -> CodeBlock {
        CodeBlock::new(block_kind, enclosing_blocks)
    }

    /// Makes a nested block list with the specified number of blocks
    /// `depth` How many nested blocks to construct
    fn make_test_block_list(depth: usize) -> Vec<Rc<RefCell<CodeBlock>>> {
        let mut blocks = vec![];

        for _ in 0..depth {
            blocks.push(Rc::new(RefCell::new(make_test_block(
                BlockKind::InnerBlock,
                &blocks,
            ))));
        }

        blocks
    }

    #[test]
    fn test_ident_declare_use() -> Result<(), IdentError> {
        let root_block = make_test_block(BlockKind::Main, &[]);
        let mut scope = root_block.scope;

        let ident = scope.declare_ident(
            Location::new(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
        )?;
        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        let use_ident = scope.use_ident(Location::new(), "a")?;
        assert_eq!(use_ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        Ok(())
    }

    #[test]
    fn test_ident_redeclare() -> Result<(), IdentError> {
        let root_block = make_test_block(BlockKind::Main, &[]);
        let mut scope = root_block.scope;

        // First decl, pass
        let ident = scope.declare_ident(
            Location::new(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
        )?;
        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        // Redecl, fail
        let redeclare_ident: Identifier = scope
            .declare_ident(
                Location::new(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::String_),
                false,
                false,
            )
            .expect_err("Previous declaration failed")
            .into();

        assert_eq!(
            redeclare_ident.type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        // Ensure that we're keeping track of the identifiers
        assert_eq!(
            scope.get_ident_instance("a", 2).unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_eq!(
            scope.get_ident_instance("a", 1).unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        Ok(())
    }

    #[test]
    fn test_ident_declare_shadow() -> Result<(), IdentError> {
        // Identifier shadowing is not allow within inner scopes
        let mut blocks = vec![];
        let root_block = Rc::new(RefCell::new(make_test_block(BlockKind::Main, &blocks)));
        blocks.push(root_block);
        let inner_block = Rc::new(RefCell::new(make_test_block(
            BlockKind::InnerBlock,
            &blocks,
        )));
        blocks.push(inner_block);

        // Outer declare
        {
            let root_scope = &mut blocks[0].borrow_mut().scope;

            let declare_ident = root_scope.declare_ident(
                Location::new(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Int),
                false,
                false,
            )?;
            assert_eq!(
                declare_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        }

        // Inner declare
        {
            let inner_scope = &mut blocks[1].borrow_mut().scope;
            let shadow_ident: Identifier = inner_scope
                .declare_ident(
                    Location::new(),
                    String::from("a"),
                    TypeRef::Primitive(PrimitiveType::Real),
                    false,
                    false,
                )
                .expect_err("Identifier not imported from parent")
                .into();

            assert_eq!(
                shadow_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );
            // Shadows imply importing things
            // Shadowed
            assert_eq!(
                inner_scope.get_ident_instance("a", 1).unwrap().type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );

            // Imported
            assert_eq!(
                inner_scope.get_ident_instance("a", 0).unwrap().type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
            assert_eq!(
                inner_scope
                    .get_ident_instance("a", 0)
                    .unwrap()
                    .import_index
                    .is_some(),
                true
            );
        }

        Ok(())
    }

    #[test]
    fn test_ident_declare_no_shadow() -> Result<(), IdentError> {
        // Declaring outer after inner scopes should not cause issues
        let mut blocks = vec![];
        let root_block = Rc::new(RefCell::new(make_test_block(BlockKind::Main, &blocks)));
        blocks.push(root_block);
        let inner_block = Rc::new(RefCell::new(make_test_block(
            BlockKind::InnerBlock,
            &blocks,
        )));
        blocks.push(inner_block);

        // Inner declare
        {
            let inner_scope = &mut blocks[1].borrow_mut().scope;
            let shadow_ident = inner_scope.declare_ident(
                Location::new(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Real),
                false,
                false,
            )?;
            assert_eq!(
                shadow_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );

            // No importing should be done
            let fetched_ident = inner_scope.get_ident("a").unwrap();
            assert_eq!(fetched_ident.instance, 1);
            assert_eq!(fetched_ident.import_index.is_none(), true);
        }

        // Outer declare
        {
            let root_scope = &mut blocks[0].borrow_mut().scope;

            let declare_ident = root_scope.declare_ident(
                Location::new(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Int),
                false,
                false,
            )?;
            assert_eq!(
                declare_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        }

        Ok(())
    }

    #[test]
    fn test_resolve_defined() -> Result<(), IdentError> {
        let root_block = make_test_block(BlockKind::Main, &[]);
        let mut scope = root_block.scope;

        let ident = scope.declare_ident(
            Location::new(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
        )?;
        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        let new_info = Identifier::new(
            Location::new(),
            TypeRef::Primitive(PrimitiveType::String_),
            String::from(""),
            true,
            true,
            true,
            0,
        );
        let ident = scope.resolve_ident("a", &new_info)?;

        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::String_));
        assert_eq!(
            scope.get_ident("a").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        Ok(())
    }

    #[test]
    fn test_resolve_undefined() {
        let root_block = make_test_block(BlockKind::Main, &[]);
        let mut scope = root_block.scope;

        let new_info = Identifier::new(
            Location::new(),
            TypeRef::Primitive(PrimitiveType::String_),
            String::from(""),
            true,
            true,
            true,
            0,
        );

        // Panics!
        let _err = scope
            .resolve_ident("a", &new_info)
            .expect_err("The given identifier has been declared");
    }

    #[test]
    fn test_use_undefined() {
        let root_block = make_test_block(BlockKind::Main, &[]);
        let mut scope = root_block.scope;

        let ident: Identifier = scope
            .use_ident(Location::new(), "a")
            .expect_err("Unused identifier was defined")
            .into();
        assert_eq!(ident.type_spec, TypeRef::TypeError);
        // Should be the first instance
        assert_eq!(ident.instance, 0);
    }

    #[test]
    fn test_use_import() -> Result<(), IdentError> {
        // External identifiers should be imported into the current scope
        let blocks = make_test_block_list(3);

        // Outer declare
        {
            let root_scope = &mut blocks[1].borrow_mut().scope;

            let declare_ident = root_scope.declare_ident(
                Location::new(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Int),
                false,
                false,
            )?;
            assert_eq!(
                declare_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
            assert_eq!(declare_ident.instance, 1);
        }

        // Inner use
        {
            let inner_scope = &mut blocks[2].borrow_mut().scope;
            let import_ident = inner_scope.use_ident(Location::new(), "a")?;

            assert_eq!(
                import_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
            // Going down 1 scope from root/import boundary
            assert_eq!(inner_scope.import_table[0].downscopes, 1);
            // First declaration in the local scope, is an import
            assert_eq!(import_ident.instance, 0);
            assert_eq!(import_ident.import_index.is_some(), true);
        }

        Ok(())
    }
}