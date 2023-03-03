//! General code analysis, including type checking and dead code reporting

mod const_eval;
mod lints;
mod query;
mod typeck;

pub mod db;
pub mod ty;

#[cfg(test)]
mod test_db;

pub use db::{AnalysisJar, ConstEvalJar, TypeJar};
