//! Defines the type system for the Turing programming language.
//!
//! This builds off of the hir-def crate, and implements the set of rules that
//! define a well-formed program, that is:
//!
//! - All of the item types are well-formed.
//! - Every body of an item is well-formed.
//!
//! With each of the rules of well-formedness being dependent on the kind of item.
//!
//! ## Notion of Types/`Ty`
//!
//! A type ([`Ty`]) in the type system is not the same as the notion of types
//! in [`hir-def`]. That is, if something participates in the type system,
//! then that something can have a type derived from it. For example, while
//! a module is an item, in the type system it is lowered to a module type.
//!
//! This simplifies the type system by not having arbitrary separations
//! between participating entities and proper types like `int`. In fact, type
//! definitions like `int` are referred to as type constructors or [`TyCons`], as
//! they directly construct types that live in the type system.
//!
//! [`Ty`]: ty::Ty
//! [`hir-def`]: toc_hir_def
//! [`TyCons`]: toc_hir_def::ty_cons::TyCons
//!
//! ## High-level Type Checking
//!
//! Conceptually, the type checking (typeck) phase is broken up into a few steps:
//!
//! 1. Item-level well-formedness
//!
//!    This checks if an item is considered to be well-formed. For a function
//!    declaration, it is considered well-formed if the type constructors in its
//!    definition are also well-formed.
//!
//! 2. Constraint generation
//!
//!    The bodies of items are traversed to create constraints that are related
//!    to the statement and expression level well-formedness rules. This also
//!    serves as an incremental boundary, as a body will generate the same
//!    constraints regardless of the spans in its body, the definitions it uses
//!    that are defined outside of the body, or the values of the preprocessor
//!    variables.
//!
//! 3. Constraint solving
//!
//!    This is the heart of the type checking process, in which body non-local
//!    definitions are substituted in and constraints are solved. From this, we
//!    get a type environment where every expression has a corresponding type.
//!    We also record what coercions needed to be performed in order to go from
//!    one type to another compatible one.
//!
//! 4. Error reporting
//!
//!    If a body is not well-formed, the remaining constraints are inspected
//!    to form better structured error messages. This builds off of the base
//!    [`InferError`]s that get emitted during constraint generation, and are
//!    intentionally meant to be simple errors, but also have enough information
//!    to reconstruct the context.
//!
//! [`InferError`]: infer::InferError

pub mod ty;
pub mod infer;

#[salsa_macros::db]
pub trait Db: toc_hir_def::Db {}

#[salsa_macros::db]
impl<DB> Db for DB where DB: toc_hir_def::Db {}
