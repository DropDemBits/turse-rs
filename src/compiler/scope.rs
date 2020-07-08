use crate::compiler::ast::Identifier;
use crate::compiler::block::CodeBlock;
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;
use std::cell::RefCell;
use std::collections::HashMap;
use std::num::NonZeroU32;
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct ImportInfo {
    /// How many scopes down, relative to the global scope, the identifier was imported from
    /// Starts from 0
    downscopes: usize,
}

/// Scope of identifiers
#[derive(Debug)]
pub struct Scope {
    /// Mappings for all active identifiers
    /// Identifiers are unique within a given scope, and any name conflicts
    /// are resolved by storing the most recent definition (as it is assumed
    /// that the old identifier definition is not going to be used anymore)
    idents: HashMap<String, Identifier>,
    /// Parent scopes of the current scope
    /// First element is the global scope for the current code unit
    /// If the vector is empty, the current scope is the global scope
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
    pub fn new(parent_blocks: &Vec<Rc<RefCell<CodeBlock>>>) -> Self {
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
    /// identifier definition will overwrite the old one and an error message
    /// will be produced.
    pub fn declare_ident(
        &mut self,
        ident: &Token,
        name: String,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
    ) -> (Identifier, Option<String>) {
        let new_def = Identifier::new(
            ident.clone(),
            type_spec,
            name.clone(),
            is_const,
            is_typedef,
            true,
            0, // Not imported
        );

        // Check to see if the identifier exists in the current scope or within the import boundary
        let mut ident_exists = self.get_ident(&name).is_some();

        for block_ref in self.parent_blocks.iter().rev() {
            if ident_exists {
                // Found something, either in current or parent scope
                break;
            }

            let block_ref = block_ref
                .upgrade()
                .expect("Memory Error: Scope freed while references still existed towards it");

            ident_exists = block_ref.borrow().scope.get_ident(&name).is_some();
        }

        // Always declare the identifier
        // For error recovery purposes
        let old_value = self.idents.insert(name.clone(), new_def.clone());

        if ident_exists {
            // Old identifier has already been declared, either within the current
            // scope, or within the import boundary

            // TODO: Check if the identifier is across an import boundary, to
            // see if it can be overwritten
            (
                new_def,
                Some(format!("'{}' has already been declared", name)),
            )
        } else {
            assert!(old_value.is_none());

            // Defining a new identifier
            (new_def, None)
        }
    }

    /// Resolves the given identifier in the given scope, by modifying the current identifier
    /// If the given identifier has not been declared in the current scope
    /// (either by importing from an external scope or by local declaration), a panic is done
    /// An identifier should already be declared by the time this is executed
    #[allow(dead_code)]
    pub fn resolve_ident(
        &mut self,
        name: &str,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
    ) -> Identifier {
        let ident = self
            .idents
            .get_mut(name)
            .expect("The given identifier has not been declared yet");
        ident.type_spec = type_spec;
        // Remove modifying these if not needed
        ident.is_const = is_const;
        ident.is_typedef = is_typedef;

        // Give back the resovled copy
        ident.clone()
    }

    /// Uses an identifer.
    /// If an identifier is not found in the current scope, it is returned from one of the parent scopes
    /// If an identifer is found from one of the parent scopes, a new import entry is also created
    /// Should an identifier not be found, an error message is produced, and
    /// an identifier with the same name is declared in the current scope
    pub fn use_ident(&mut self, ident: &Token, name: &str) -> (Identifier, Option<String>) {
        if let Some(declared) = self.get_ident(name) {
            let mut reference = declared.clone();
            // Change the location to be that of the reference location
            reference.token = ident.clone();

            (reference, None)
        } else {
            // Peek into each of the parent scopes, in reverse order
            for (block_ref, downscopes) in self
                .parent_blocks
                .iter()
                .zip(0..self.parent_blocks.len())
                .rev()
            {
                let block_ref = block_ref
                    .upgrade()
                    .expect("Memory Error: Scope freed while references still existed towards it");
                let block = block_ref.borrow();
                let parent_ident = block.scope.get_ident(name);

                if let Some(declared) = parent_ident {
                    // Add import info entry
                    let index = self.add_import_entry(ImportInfo { downscopes });

                    let mut reference = declared.clone();

                    // Change the location to point to the reference location
                    reference.token = ident.clone();
                    // Update the import index
                    reference.import_index.replace(index);

                    // Add to the local definition table
                    let old_value = self.idents.insert(name.to_string(), reference.clone());
                    assert!(old_value.is_none());

                    return (reference, None);
                }
            }

            // None found, make a new one!
            let err_ident = Identifier::new(
                ident.clone(),
                TypeRef::TypeError, // Produce a type error to propagate the error
                name.to_string(),
                false,
                false,
                false,
                0, // Not imported, just creating a new definition
            );

            // Define the error entry
            let old_value = self.idents.insert(name.to_string(), err_ident.clone());

            // While this should never happen on a single thread, if the
            // parser is somehow made multithreaded, then there may already be
            // a definition
            assert!(old_value.is_none());

            (
                err_ident,
                Some(format!("'{}' has not been declared yet", name)),
            )
        }
    }

    /// Gets the identifier with the given name from the current scope's identifier list
    pub fn get_ident(&self, name: &str) -> Option<&Identifier> {
        self.idents.get(name)
    }

    /// Adds the given entry to the import table
    /// Returns the corresponding import index (plus 1)
    fn add_import_entry(&mut self, info: ImportInfo) -> NonZeroU32 {
        self.import_table.push(info);

        self.next_import_index += 1;
        // Import index is +1 of regular index
        NonZeroU32::new(self.next_import_index).unwrap()
    }
}
