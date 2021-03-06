//! Front end for the compiler
#![allow(clippy::if_same_then_else)]
pub mod context;
pub mod parser;
pub mod scanner;
pub mod validator;

extern crate toc_ast;
extern crate toc_core;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
extern crate rand;
extern crate unicode_segmentation;
