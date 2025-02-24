//! Common interned symbols
#![allow(non_upper_case_globals)] // Keeping common names

use std::sync::LazyLock;

use crate::symbol::Symbol;

/// For unnamed definitions that can be generated by the user
pub static Anonymous: LazyLock<Symbol> = LazyLock::new(|| "<anonymous>".into());
/// Name of the root of a translation unit
pub static Root: LazyLock<Symbol> = LazyLock::new(|| "<root>".into());
/// For unnamed definitions that *can't* be generated by the user (e.g. unnamed parameters)
pub static Unnamed: LazyLock<Symbol> = LazyLock::new(|| "<unnamed>".into());
