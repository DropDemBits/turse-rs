#![allow(dead_code)]
pub(crate) mod ast;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod token;
pub(crate) mod type_validator;
pub(crate) mod types;

extern crate unicode_segmentation;

use crate::compiler::ast::Identifier;
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;
use std::cell::RefCell;
use std::collections::HashMap;
use std::num::NonZeroU32;
use std::rc::{Rc, Weak};

/// Location of a token in a file/text stream
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    /// Starting byte of a lexeme
    start: usize,
    /// Ending byte of a lexeme
    end: usize,
    /// Line number of the lexeme
    pub line: usize,
    /// Starting column of the lexeme
    pub column: usize,
    /// The span of the lexeme, in columns
    pub width: usize,
}

impl Location {
    pub fn new() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
            width: 0,
        }
    }

    /// Advances the location to the next lexeme, beginning a new lexeme
    pub fn step(&mut self) {
        self.start = self.end;
        self.column += self.width;
        self.width = 0;
    }

    /// Advances the column location by the give amount of steps
    pub fn columns(&mut self, steps: usize) {
        self.width += steps;
    }

    /// Advances the line location by the give amount of steps, as well as resetting the column
    pub fn lines(&mut self, steps: usize) {
        self.column = 1;
        self.width = 0;
        self.line += steps;
    }

    /// Moves the end of the lexeme to the given byte index
    pub fn current_to(&mut self, next_end: usize) {
        self.end = next_end;
    }

    /// Moves the end of the lexeme to the end of the given location
    pub fn current_to_other(&mut self, other: &Location) {
        self.end = other.end;
    }

    /// Gets the lexeme corresponding to this location
    pub fn get_lexeme<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}

// To migrate to scope.rs

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
    idents: HashMap<String, ast::Identifier>,
    /// Parent scopes of the current scope
    /// First element is the global scope for the current code unit
    /// If the vector is empty, the current scope is the global scope
    parent_scopes: Vec<Weak<RefCell<Self>>>,
    /// Next import index into the import table
    next_import_index: u32,
    /// Import table
    import_table: Vec<ImportInfo>,
}

impl Scope {
    pub fn new(parent_scopes: &Vec<Rc<RefCell<Self>>>) -> Self {
        // Take a weak reference to the parent scopes
        // We do not need a strong reference to the parent scopes, as we do not
        // want to take ownership of the parent scopes. The CodeUnit / Parser
        // is what will take ownership of the scopes
        Self {
            idents: HashMap::new(),
            parent_scopes: parent_scopes
                .iter()
                .map(|scope| Rc::downgrade(scope))
                .collect(),
            next_import_index: 0,
            import_table: vec![],
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
    ) -> (ast::Identifier, Option<String>) {
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

        for scope_ref in self.parent_scopes.iter().rev() {
            if ident_exists {
                // Found something, either in current or parent scope
                break;
            }

            let scope_ref = scope_ref
                .upgrade()
                .expect("Memory Error: Scope freed while references still existed towards it");

            ident_exists = scope_ref.borrow().get_ident(&name).is_some();
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
    pub fn use_ident(&mut self, ident: &Token, name: &str) -> (ast::Identifier, Option<String>) {
        if let Some(declared) = self.get_ident(name) {
            let mut reference = declared.clone();
            // Change the location to be that of the reference location
            reference.token = ident.clone();

            (reference, None)
        } else {
            // Peek into each of the parent scopes, in reverse order
            for (scope_ref, downscopes) in self
                .parent_scopes
                .iter()
                .zip(0..self.parent_scopes.len())
                .rev()
            {
                let scope_ref = scope_ref
                    .upgrade()
                    .expect("Memory Error: Scope freed while references still existed towards it");
                let scope = scope_ref.borrow();
                let parent_ident = scope.get_ident(name);

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
    fn get_ident(&self, name: &str) -> Option<&ast::Identifier> {
        self.idents.get(name)
    }

    /// Checks if the identifier has been declared in the current scope
    /// True if it is declared in the current scope, false otherwise
    fn is_declared(&self, name: &str) -> bool {
        self.get_ident(name).is_some()
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
