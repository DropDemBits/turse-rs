//! IR Generation and manipulation

// Mute IR warnings as we aren't touching this for now
#![allow(dead_code)]
mod builder;
mod graph;

pub use builder::IrBuilder;

use crate::compiler::types::TypeRef;
use std::collections::HashMap;

/// Variable reference spaces
#[derive(Debug, Copy, Clone)]
pub enum AddressSpace {
    /// Global, per-unit space
    Global,
    /// Local stack space
    Local,
}

/// Reference node in a block scope (a group of references)
#[derive(Debug)]
pub struct ReferenceNode {
    /// References used in the current reference scope
    references: HashMap<String, graph::Reference>,
}

impl ReferenceNode {
    /// Creates a new reference scope
    pub fn new() -> Self {
        Self {
            references: HashMap::new(),
        }
    }

    /// Assigns a value to a given reference.
    /// Creates a new one if one doesn't exist.
    pub fn assign_ref(
        &mut self,
        name: &str,
        type_ref: &TypeRef,
        address_space: AddressSpace,
    ) -> graph::Reference {
        self.references
            .entry(name.to_string())
            .and_modify(|existing| {
                // Advance the generation number
                existing.generation += 1;
            })
            .or_insert(graph::Reference {
                name: name.to_string(),
                type_ref: type_ref.clone(),
                generation: 0,
                address_space,
            })
            .clone()
    }

    /// Uses a reference.
    pub fn use_ref(&self, name: &str) -> graph::Reference {
        self.references
            .get(name)
            .map(|r| r.clone())
            .expect(&format!("Reference '{}' is missing an 'assign_ref'", name))
    }
}
