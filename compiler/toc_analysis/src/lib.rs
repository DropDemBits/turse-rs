//! General code analysis, including type checking and dead code reporting

mod const_eval;
mod query;
mod typeck;

pub mod db;
pub mod ty;

#[cfg(test)]
mod test_db;
