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
    /// Identifiers are unique within a given scope.
    /// Any name conflicts are resolved by storing the most recent definition
    /// as it is assumed that the old identifier definition is not going to
    /// be used anymore.
    idents: HashMap<String, Identifier>,
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
        ident: Token,
        name: String,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
    ) -> (Identifier, Option<String>) {
        // Check to see if the identifier exists in the current scope or within the import boundary
        let mut ident_exists = self.get_ident(&name).is_some();

        for block_ref in self.parent_blocks.iter().rev() {
            if ident_exists {
                // Found something, either in current or parent scope
                break;
            }

            let block_ref = block_ref
                .upgrade()
                .expect("Block dropped while references still exists to it");

            ident_exists = block_ref.borrow().scope.get_ident(&name).is_some();
        }

        let new_def = Identifier::new(
            ident,
            type_spec,
            name.clone(),
            is_const,
            is_typedef,
            true,
            0, // Not imported
        );

        // Always declare the identifier for error recovery purposes
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
    pub fn resolve_ident(&mut self, name: &str, new_info: &Identifier) -> Identifier {
        let ident = self
            .idents
            .get_mut(name)
            .expect("The given identifier has not been declared yet");
        ident.type_spec = new_info.type_spec;
        ident.is_compile_eval = new_info.is_compile_eval;
        // Remove modifying these if not needed
        ident.is_const = new_info.is_const;
        ident.is_typedef = new_info.is_typedef;

        // Give back the resovled copy
        ident.clone()
    }

    /// Uses an identifer.
    /// If an identifier is not found in the current scope, it is returned from one of the parent scopes
    /// If an identifer is found from one of the parent scopes, a new import entry is also created
    /// Should an identifier not be found, an error message is produced, and
    /// an identifier with the same name is declared in the current scope
    pub fn use_ident(&mut self, ident: Token, name: &str) -> (Identifier, Option<String>) {
        if let Some(declared) = self.get_ident(name) {
            let mut reference = declared.clone();
            // Change the location to be that of the reference location
            reference.token = ident;

            (reference, None)
        } else {
            // Peek into each of the parent scopes, in reverse order
            for (downscopes, block_ref) in self.parent_blocks.iter().enumerate().rev() {
                let block_ref = block_ref
                    .upgrade()
                    .expect("Block dropped while references still exists to it");
                let block = block_ref.borrow();
                let parent_ident = block.scope.get_ident(name);

                if let Some(declared) = parent_ident {
                    // Add import info entry
                    let index = self.add_import_entry(ImportInfo { downscopes });

                    let mut reference = declared.clone();

                    // Change the location to point to the reference location
                    reference.token = ident;
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
                ident,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::block::BlockKind;
    use crate::compiler::token::TokenType;
    use crate::compiler::types::PrimitiveType;
    use crate::compiler::Location;

    fn make_test_block(
        block_kind: BlockKind,
        enclosing_blocks: &Vec<Rc<RefCell<CodeBlock>>>,
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

    fn make_ident_token() -> Token {
        Token {
            location: Location::new(),
            token_type: TokenType::Identifier,
        }
    }

    #[test]
    fn test_ident_declare_use() {
        let root_block = make_test_block(BlockKind::Main, &vec![]);
        let mut scope = root_block.scope;

        let (ident, declare_msg) = scope.declare_ident(
            make_ident_token(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
        );
        assert!(declare_msg.is_none());
        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        let (use_ident, use_msg) = scope.use_ident(make_ident_token(), "a");
        assert!(use_msg.is_none());
        assert_eq!(use_ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn test_ident_redeclare() {
        let root_block = make_test_block(BlockKind::Main, &vec![]);
        let mut scope = root_block.scope;

        // First decl, pass
        let (ident, declare_msg) = scope.declare_ident(
            make_ident_token(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
        );
        assert!(declare_msg.is_none());
        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        // Redecl, fail
        let (redeclare_ident, redeclare_msg) = scope.declare_ident(
            make_ident_token(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::String_),
            false,
            false,
        );
        assert!(redeclare_msg.is_some());
        assert_eq!(
            redeclare_ident.type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
    }

    #[test]
    fn test_ident_declare_shadow() {
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

            let (declare_ident, declare_msg) = root_scope.declare_ident(
                make_ident_token(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Int),
                false,
                false,
            );
            assert!(declare_msg.is_none());
            assert_eq!(
                declare_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        }

        // Inner declare
        {
            let inner_scope = &mut blocks[1].borrow_mut().scope;
            let (shadow_ident, shadow_msg) = inner_scope.declare_ident(
                make_ident_token(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Real),
                false,
                false,
            );
            assert!(shadow_msg.is_some());
            assert_eq!(
                shadow_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );
        }
    }

    #[test]
    fn test_ident_declare_no_shadow() {
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
            let (shadow_ident, shadow_msg) = inner_scope.declare_ident(
                make_ident_token(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Real),
                false,
                false,
            );
            assert!(shadow_msg.is_none());
            assert_eq!(
                shadow_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Real)
            );
        }

        // Outer declare
        {
            let root_scope = &mut blocks[0].borrow_mut().scope;

            let (declare_ident, declare_msg) = root_scope.declare_ident(
                make_ident_token(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Int),
                false,
                false,
            );
            assert!(declare_msg.is_none());
            assert_eq!(
                declare_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        }
    }

    #[test]
    fn test_resolve_defined() {
        let root_block = make_test_block(BlockKind::Main, &vec![]);
        let mut scope = root_block.scope;

        let (ident, msg) = scope.declare_ident(
            make_ident_token(),
            String::from("a"),
            TypeRef::Primitive(PrimitiveType::Int),
            false,
            false,
        );
        assert!(msg.is_none());
        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::Int));

        let new_info = Identifier::new(
            make_ident_token(),
            TypeRef::Primitive(PrimitiveType::String_),
            String::from(""),
            true,
            true,
            true,
            0,
        );
        let ident = scope.resolve_ident("a", &new_info);

        assert_eq!(ident.type_spec, TypeRef::Primitive(PrimitiveType::String_));
        assert_eq!(
            scope.get_ident("a").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
    }

    #[test]
    #[should_panic(expected = "The given identifier has not been declared yet")]
    fn test_resolve_undefined() {
        let root_block = make_test_block(BlockKind::Main, &vec![]);
        let mut scope = root_block.scope;

        // Panics!
        let new_info = Identifier::new(
            make_ident_token(),
            TypeRef::Primitive(PrimitiveType::String_),
            String::from(""),
            true,
            true,
            true,
            0,
        );
        let _ = scope.resolve_ident("a", &new_info);
    }

    #[test]
    fn test_use_undefined() {
        let root_block = make_test_block(BlockKind::Main, &vec![]);
        let mut scope = root_block.scope;

        let (ident, msg) = scope.use_ident(make_ident_token(), "a");
        assert!(msg.is_some());
        assert_eq!(ident.type_spec, TypeRef::TypeError);
    }

    #[test]
    fn test_use_import() {
        // External identifiers should be imported into the current scope
        let mut blocks = make_test_block_list(3);

        // Outer declare
        {
            let root_scope = &mut blocks[1].borrow_mut().scope;

            let (declare_ident, declare_msg) = root_scope.declare_ident(
                make_ident_token(),
                String::from("a"),
                TypeRef::Primitive(PrimitiveType::Int),
                false,
                false,
            );
            assert!(declare_msg.is_none());
            assert_eq!(
                declare_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        }

        // Inner use
        {
            let inner_scope = &mut blocks[2].borrow_mut().scope;
            let (shadow_ident, shadow_msg) = inner_scope.use_ident(make_ident_token(), "a");
            assert!(shadow_msg.is_none());
            assert_eq!(
                shadow_ident.type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
            // Going down 1 scope from root/import boundary
            assert_eq!(inner_scope.import_table[0].downscopes, 1);
        }
    }
}
