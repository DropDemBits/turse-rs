//! AST representation
#![allow(clippy::if_same_then_else)]
pub mod ast;
pub mod block;
pub mod scope;
pub mod types;
pub mod value;

// To move to frontend
pub mod token;

extern crate toc_core;
