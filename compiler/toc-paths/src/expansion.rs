//! Expansion of paths into their absolute forms

use std::{collections::HashMap, fmt};

use camino::{Utf8Component, Utf8Path, Utf8PathBuf};

use crate::Db;

/// What each path prefix should expand to
#[salsa::input(singleton)]
pub struct PrefixExpansions {
    /// Builtin set of prefix expansions
    #[return_ref]
    pub builtin_expansions: HashMap<BuiltinPrefix, Utf8PathBuf>,
    // ???: Do we want to support user-provided expansions?
    // Would be easy to though
}

#[salsa::tracked]
impl PrefixExpansions {
    #[salsa::tracked(return_ref)]
    pub(crate) fn builtin_expansion(
        self,
        db: &dyn Db,
        prefix: BuiltinPrefix,
    ) -> Option<Utf8PathBuf> {
        self.builtin_expansions(db).get(&prefix).cloned()
    }
}

/// Built-in prefixes for paths.
///
/// The `FileSystem::set_prefix_expansion` query
/// should be used to set the corresponding path the path expands into.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinPrefix {
    /// %oot, should point to the Turing home directory
    /// (the directory containing "support" from an (Open)Turing installation).
    Oot,
    /// `%help`, should point to the Turing help directory
    /// (normally set as "%oot/support/help").
    Help,
    /// `%user`, should point to the current user's home directory
    /// (normally "$HOME" on Linux, or "C:/Users/%USER%" on Windows).
    UserHome,
    /// `%job`, can be specified by the user
    /// (set to a user provided path, or the temp directory root otherwise).
    Job,
    /// `%tmp`, should point to the temp directory
    /// (normally should be the path returned from [`env::temp_dir`][temp_dir])
    ///
    /// [temp_dir]: std::env::temp_dir
    Temp,
}

impl BuiltinPrefix {
    pub(crate) fn percent_prefix(self) -> &'static str {
        match self {
            BuiltinPrefix::Oot => "%oot",
            BuiltinPrefix::Help => "%help",
            BuiltinPrefix::UserHome => "%home",
            BuiltinPrefix::Job => "%job",
            BuiltinPrefix::Temp => "%tmp",
        }
    }
}

impl TryFrom<&str> for BuiltinPrefix {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "oot" => Self::Oot,
            "help" => Self::Help,
            "home" => Self::UserHome,
            "job" => Self::Job,
            "tmp" => Self::Temp,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for BuiltinPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.percent_prefix())
    }
}

/// Expands any prefixes in the path
pub fn expand_path(db: &dyn Db, path: Utf8PathBuf) -> Utf8PathBuf {
    fn try_it(db: &dyn Db, path: &Utf8Path) -> Option<Utf8PathBuf> {
        let path = path;

        let prefix_name = {
            // Try parsing out a percent prefix
            let Some(Utf8Component::Normal(comp)) = path.components().next() else {
                return None;
            };
            comp.strip_prefix('%')?
        };
        let prefix_path = BuiltinPrefix::try_from(prefix_name).ok()?;

        // If there's no expansion, then just default to returning the path as-is
        let base_path = PrefixExpansions::get(db)
            .builtin_expansion(db, prefix_path)
            .as_ref()?;

        // Can unwrap since we've checked that the prefix is in the string
        Some(base_path.join(path.strip_prefix(prefix_path.percent_prefix()).unwrap()))
    }

    try_it(db, &path).unwrap_or(path)
}
