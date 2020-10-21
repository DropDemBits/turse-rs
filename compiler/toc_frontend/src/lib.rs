//! Front end for the compiler
#![allow(clippy::if_same_then_else)]
pub mod context;
pub mod parser;
pub mod scanner;
pub mod token;
//pub mod validator; // TODO(resolver): Re-enable validator once resolver mess is done

extern crate toc_ast;
extern crate toc_core;

#[cfg(test)]
extern crate rand;
extern crate unicode_segmentation;
