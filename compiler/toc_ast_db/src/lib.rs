//! Database queries & structures for the AST level of compilation

pub mod db;
pub mod span;

mod source;

pub use toc_source_graph::{DependGraph, SourceGraph};
