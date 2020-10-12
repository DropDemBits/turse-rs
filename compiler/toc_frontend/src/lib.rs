//! Front end for the compiler
#![allow(clippy::if_same_then_else)]
pub mod parser;
pub mod scanner;
pub mod token;
pub mod validator;

extern crate toc_ast;
extern crate toc_core;

#[cfg(test)]
extern crate rand;
extern crate unicode_segmentation;
