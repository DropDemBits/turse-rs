//! Definition of HIR related queries (including identifier resolution)

// How to generate the Library graph:
// We're passed a graph of `FileId`s and asked to lower them
// `SourceRoots` -> `LibraryGraph`
// It's just a simple `map` operation

pub mod db;
mod query;
