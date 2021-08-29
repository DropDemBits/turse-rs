//! Definition of HIR related queries (including identifier resolution)

use std::sync::Arc;

use toc_hir::{library_graph::LibraryGraph, ty};
use toc_reporting::CompileResult;
use toc_salsa::salsa;

// ???: How do we generate the Library graph?
// We're probably passed a graph of `FileId`s and asked to lower them
// `SourceRoots` -> `LibraryGraph`

mod query;

/// HIR tree related queries
#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: toc_hir_lowering::LoweringDb {
    /// Graph of all libraries
    #[salsa::invoke(query::hir_library_graph)]
    #[salsa::dependencies]
    fn library_graph(&self) -> CompileResult<LibraryGraph>;
}

/// Salsa-backed type interner
#[salsa::query_group(InternedTypeStorage)]
pub trait InternedType {
    #[salsa::interned]
    fn intern_type(&self, ty: Arc<ty::Type>) -> salsa::InternId;
}

/// Implements the plumbing to connect the salsa interner into the HIR tree interface.
///
/// The given database must have `InternedTypeStorage` as part of its storage list.
#[macro_export]
macro_rules! impl_hir_type_interner {
    ($db:path) => {
        impl ::toc_hir::ty::TypeInterner for $db {
            fn intern_type(&self, ty: ::toc_hir::ty::Type) -> ::toc_hir::ty::TypeId {
                use std::num::NonZeroU32;
                let ty = Arc::new(ty);
                let raw = <Self as $crate::InternedType>::intern_type(self, ty).as_u32();
                let raw = NonZeroU32::new(raw.wrapping_add(1)).expect("too many ids");
                ::toc_hir::ty::TypeId(raw)
            }

            fn lookup_type(
                &self,
                type_id: ::toc_hir::ty::TypeId,
            ) -> ::std::sync::Arc<::toc_hir::ty::Type> {
                let raw = type_id.0.get().wrapping_sub(1);
                let interned = salsa::InternId::from(raw);

                <Self as $crate::InternedType>::lookup_intern_type(self, interned)
            }
        }
    };
}
