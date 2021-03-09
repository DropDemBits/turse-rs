//! Type related HIR nodes

use la_arena::Idx;

pub type TypeIdx = Idx<Type>;

#[derive(Debug)]
pub enum Type {
    /// Error Type, only used to represent invalid code
    Missing,
}
