//! Re-exports of salsa
pub use salsa;

/// Creates a key for interning in a salsa database
///
/// # Example
///
/// ```rust
/// # use toc_salsa::create_intern_key;
/// create_intern_key!(
///     /// Doc comments are propogated!
///     pub Interned;   
/// );
/// ```
#[macro_export]
macro_rules! create_intern_key {
    ($(#[$attr:meta])* $visi:vis $key:ident;) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        $visi struct $key(u32);

        impl $crate::salsa::InternKey for $key {
            fn from_intern_id(v: $crate::salsa::InternId) -> Self {
                Self(v.as_u32())
            }

            fn as_intern_id(&self) -> $crate::salsa::InternId {
                self.0.into()
            }
        }

        impl ::std::fmt::Debug for $key {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let raw: u32 = self.0;
                f.debug_tuple(stringify!($key))
                    .field(&raw)
                    .finish()
            }
        }
    };
}
