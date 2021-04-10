//! AST representation
#![allow(clippy::if_same_then_else)]
pub mod ast;
pub(crate) mod pretty_print;
pub mod scope;
pub mod types;
pub mod unit;
pub mod value;

extern crate toc_core;
