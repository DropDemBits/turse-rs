use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    path::{Path, PathBuf},
};

use camino::Utf8PathBuf;
use lsp_types::{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position};
use ropey::Rope;
use toc_analysis::db::HirAnalysis;
use toc_hir::library_graph::SourceLibrary;
use toc_paths::RawPath;
use toc_source_graph::{DependencyList, RootLibraries};
use toc_vfs::{LoadError, LoadStatus};
use toc_vfs_db::{SourceTable, VfsBridge, VfsDbExt};
use tracing::{error, trace};

pub type Cancellable<T = ()> = Result<T, salsa::Cancelled>;

#[derive(Default)]
pub struct ServerState {
    db: LspDatabase,
    files: FileStore,
    existing_libraries: BTreeMap<RawPath, SourceLibrary>,
}

impl ServerState {
    pub fn open_file(&mut self, uri: &lsp_types::Url, version: i32, text: String) {
        // This is where we update the source graph, as well as the file sources, and the file store
        let Ok(path) = uri.to_file_path() else {
            error!("BUG: Encountered bad path during file open: {uri:?}");
            return;
        };

        // Track the file in the file store
        self.files.add_file(&path, text, version);

        // Push updated sources & source graph into db
        self.update_file(&path, false);
    }

    pub fn close_file(&mut self, uri: &lsp_types::Url) {
        // This is where we remove files from the source graph (if applicable / non-root) and from the file store
        let Ok(path) = uri.to_file_path() else {
            error!("BUG: Encountered bad path during file close: {uri:?}");
            return;
        };

        // Notify that the editor isn't using the file anymore
        self.files.remove_file(&path);

        // Push removed sources & updated source graph
        self.update_file(&path, true);
    }

    pub fn apply_changes(
        &mut self,
        uri: &lsp_types::Url,
        version: i32,
        changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    ) {
        let Ok(path) = uri.to_file_path() else {
            error!("BUG: Encountered bad path during file change: {uri:?}");
            return;
        };

        self.files.apply_changes(&path, version, changes);

        // Push updated sources & source graph into db
        self.update_file(&path, false);
    }

    fn update_file(&mut self, path: &Path, removed: bool) {
        let db = &mut self.db;

        // Add the root path to the file db
        // FIXME: Remove unwrap by using Utf8Path{Buf}
        let root_file = RawPath::new(db, Utf8PathBuf::from_path_buf(path.to_owned()).unwrap());
        let load_status = if !removed {
            let contents = self.files.source(path).into_owned();
            Ok(LoadStatus::Modified(contents.into()))
        } else {
            Err(LoadError::new(path, toc_vfs::ErrorKind::NotFound))
        };
        db.update_file(root_file, load_status);

        // Setup source graph
        let roots_modified = if !removed {
            match self.existing_libraries.entry(root_file) {
                std::collections::btree_map::Entry::Vacant(ent) => {
                    let Some(file_name) = path.file_name() else {
                        error!("trying to update folder (from path {})", path.display());
                        return;
                    };

                    let library_id = SourceLibrary::new(
                        db,
                        file_name.to_string_lossy().to_string(),
                        root_file,
                        toc_hir::library_graph::ArtifactKind::Binary,
                        DependencyList::empty(db),
                    );

                    ent.insert(library_id);
                    true
                }
                std::collections::btree_map::Entry::Occupied(ent) => {
                    trace!("reusing {ent:?} for {root_file:?}");
                    false
                }
            }
        } else {
            match self.existing_libraries.entry(root_file) {
                std::collections::btree_map::Entry::Vacant(_) => {
                    // already removed, nothing needs to be updated
                    error!("{root_file:?} already was removed");
                    false
                }
                std::collections::btree_map::Entry::Occupied(ent) => {
                    trace!("closing library {ent:?}");
                    ent.remove();
                    true
                }
            }
        };

        if roots_modified {
            let root_libraries =
                RootLibraries::try_get(db).unwrap_or_else(|| RootLibraries::new(db, vec![]));

            // This is than having to clone the whole source graph (since that's reconstructed anyways)
            let all_roots = self
                .existing_libraries
                .values()
                .copied()
                .collect::<Vec<_>>();

            root_libraries.set_roots(db).to(all_roots);
        }

        // FIXME: Recursively load in files, respecting already loaded files
        // FIXME: Deal with adding source roots that depend on files that are already source roots
    }

    /// Collect diagnostics for all libraries
    pub fn collect_diagnostics(&self) -> Cancellable<Vec<(PathBuf, Vec<Diagnostic>)>> {
        let analyze_res = self.db.analyze_libraries();
        let msgs = analyze_res.messages();

        // Note: this does noisily fail, but we don't gracefully handle panics yet
        msgs.assert_no_delayed_reports();

        fn to_diag_level(kind: toc_reporting::AnnotateKind) -> DiagnosticSeverity {
            use toc_reporting::AnnotateKind;

            match kind {
                AnnotateKind::Note => DiagnosticSeverity::HINT,
                AnnotateKind::Info => DiagnosticSeverity::INFORMATION,
                AnnotateKind::Warning => DiagnosticSeverity::WARNING,
                AnnotateKind::Error => DiagnosticSeverity::ERROR,
            }
        }

        // Breakup messages into per-file bundles
        let mut bundles = HashMap::new();

        for msg in msgs.iter() {
            // Convert each message into a `Diagnostic`
            let Some(range) = self.map_span_to_location(msg.span()).map(|loc| loc.range) else {
                error!("BUG: Encountered bad message span (Original message: {msg:#?})");
                continue;
            };

            let severity = to_diag_level(msg.kind());
            let annotations = msg
                .annotations()
                .iter()
                .filter_map(|annotate| {
                    let Some(location) = self.map_span_to_location(annotate.span()) else {
                        error!(
                            "BUG: Encountered bad annotation span (Original annotation (from {range:?}): {annotate:#?})"
                        );
                        return None;
                    };

                    Some(DiagnosticRelatedInformation {
                        location,
                        message: annotate.message().to_string(),
                    })
                })
                .collect();
            let message = if !msg.footer().is_empty() {
                // Push all footer infos into the main message
                let mut message = msg.message().to_string();

                for annotate in msg.footer() {
                    message.push('\n');
                    message.push_str(annotate.message());
                }

                message
            } else {
                msg.message().to_string()
            };

            let diagnostic = Diagnostic::new(
                range,
                Some(severity),
                None,
                None,
                message,
                Some(annotations),
                None,
            );

            // Only accept diagnostics with files attached
            let Some((file, _)) = msg.span().into_parts() else {
                // FIXME: Log a warning in this situation
                continue;
            };
            let file_diagnostics = bundles.entry(file.into_raw()).or_insert_with(Vec::new);
            file_diagnostics.push(diagnostic);
        }

        // Add empty bundles for files that are tracked, but don't have any diagnostics
        // This is done to remove any diagnostics from files which previously had some
        for (path, _) in self.files.file_map.iter() {
            // FIXME: Remove unwrap by using Utf8Path{Buf}
            let file_id = RawPath::new(
                &self.db,
                Utf8PathBuf::from_path_buf(path.to_owned()).unwrap(),
            );
            bundles.entry(file_id).or_insert_with(Vec::new);
        }

        // Convert RawPaths into the underlying PathBufs
        Ok(bundles
            .into_iter()
            .map(|(path, bundle)| (path.raw_path(&self.db).as_std_path().to_owned(), bundle))
            .collect())
    }

    fn map_span_to_location(&self, span: toc_span::Span) -> Option<Location> {
        let db = &self.db;

        let (file, range) = span.into_parts()?;
        let (start, end) = (u32::from(range.start()), u32::from(range.end()));
        let path = file.into_raw();

        let start = toc_ast_db::map_byte_index_to_position(db, path, start as usize)?;
        let end = toc_ast_db::map_byte_index_to_position(db, path, end as usize)?;

        let path = path.raw_path(db);

        Some(Location::new(
            lsp_types::Url::from_file_path(path.as_str()).unwrap(),
            lsp_types::Range::new(start.into_position(), end.into_position()),
        ))
    }
}

trait IntoPosition {
    fn into_position(self) -> Position;
}

impl IntoPosition for toc_ast_db::span::LspPosition {
    fn into_position(self) -> Position {
        Position::new(self.line, self.column)
    }
}

#[salsa::db(
    toc_paths::Jar,
    toc_vfs_db::Jar,
    toc_source_graph::Jar,
    toc_ast_db::Jar,
    toc_hir_lowering::Jar,
    toc_hir_db::Jar,
    toc_analysis::TypeJar,
    toc_analysis::ConstEvalJar,
    toc_analysis::AnalysisJar
)]
#[derive(Default)]
struct LspDatabase {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl salsa::Database for LspDatabase {}

impl VfsBridge for LspDatabase {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }

    fn load_new_file(&self, _path: toc_paths::RawPath) -> (String, Option<LoadError>) {
        // Don't actually load in file sources
        // TODO: Send a message to load in new file sources, and indicate that we're tracking changes to them
        (
            String::new(),
            Some(toc_vfs::LoadError::new("", toc_vfs::ErrorKind::NotLoaded)),
        )
    }
}

// FIXME: Impl ParallelDb now  that path interning is in the db

/// File store tracks the state of all files currently used by the LSP.
/// Files sources can come from two primary locations:
///
/// - From the editor itself (indicated by open/close events)
/// - Encountered during source graph invalidation (via `FileLoader` load file)
///
/// Files from the first source automatically have the sources watched for changes
/// through `didChange` events. Files from the second source need to be manually
/// added to a watcher list provided by the client.
///
/// Once tracked from one of these sources, a file can be in one of the following states:
///
/// - Tracked from the filesystem: Changes come from filesystem events
/// - Tracked from the editor: Changes come from the editor, frequent
/// - Untracked: File does not correspond either to a file on-disk, nor from an editor buffer
///
/// `FileStore` is what will hold the source of truth for the actual file sources.
///
/// Practically, the `FileStore` only cares if a file is tracked or not, since that's the only thing that's important
/// when loading in new files (we don't want to reload in files that we're already tracking changes for).
///
// ???: Can this changed behaviour be pushed into the vfs?
// ???: Can we have fixed-point file loading? (allows for progress bar, may depend on behaviour being pushed into vfs)
//      - Files will be in an incomplete state, db queries dependent on these files must not be exec'd
//      - Will need to pub what files need sources
//      - Could set status of frontier files to indicate that results from using them are invalid...
#[derive(Default)]
struct FileStore {
    // FIXME: Use Utf8PathBuf
    file_map: HashMap<PathBuf, FileInfo>,
}

impl FileStore {
    /// Adds a file to be tracked inside of the file store
    ///
    /// Returns `true` if this replaced an already tracked file
    fn add_file(&mut self, path: &Path, text: String, version: i32) -> bool {
        let old = self.file_map.insert(
            path.into(),
            FileInfo {
                version,
                source: Rope::from(text),
            },
        );

        old.is_some()
    }

    /// Removes a file from being tracked inside of the file store
    fn remove_file(&mut self, path: &Path) {
        self.file_map
            .remove(path)
            .expect("non-tracked file removed from file store");
    }

    fn apply_changes(
        &mut self,
        path: &Path,
        version: i32,
        changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    ) {
        let file_info = self.file_map.get_mut(path).expect("file not tracked yet");

        // Apply the changes
        for change in changes {
            match change.range {
                Some(part) => {
                    // Incremental change, replace slice of text
                    fn position_to_char(rope: &Rope, pos: Position) -> Option<usize> {
                        let line = usize::try_from(pos.line).ok()?;
                        let chr = usize::try_from(pos.character).ok()?;
                        let base = rope.line_to_char(line);
                        let col = rope.line(line).utf16_cu_to_char(chr);
                        Some(base + col)
                    }

                    let Some(start) = position_to_char(&file_info.source, part.start) else {
                        error!("bad start position {:#?}", part.start);
                        return;
                    };
                    let Some(end) = position_to_char(&file_info.source, part.end) else {
                        error!("bad start position {:#?}", part.end);
                        return;
                    };

                    file_info.source.remove(start..end);
                    file_info.source.insert(start, &change.text);
                }
                None => {
                    // Full replacement, build a new slice
                    file_info.source = Rope::from(change.text);
                }
            }
        }

        // Update version
        file_info.version = version;
    }

    /// Gets the source of the given file
    fn source(&self, path: &Path) -> Cow<'_, str> {
        let source = &self
            .file_map
            .get(path)
            .expect("file not tracked yet")
            .source;

        source.into()
    }
}

struct FileInfo {
    version: i32,
    source: Rope,
}

#[cfg(test)]
mod tests {
    use lsp_types::Url;

    use super::*;

    #[test]
    fn state_open_close_file() {
        let mut state = ServerState::default();

        // Make an absolute path for making a url from
        let path = Path::new("/").canonicalize().unwrap().join("test.yee");
        let uri = Url::from_file_path(path).unwrap();

        // Survive opening & closing a file
        state.open_file(&uri, 0, "% blah".into());
        state.close_file(&uri);
    }
    #[test]
    fn state_open_close_repeated_file() {
        let mut state = ServerState::default();

        // Make an absolute path for making a url from
        let path = Path::new("/").canonicalize().unwrap().join("test.yee");
        let uri = Url::from_file_path(path).unwrap();

        // Survive repeated opening & closing a file
        state.open_file(&uri, 0, "% blah".into());
        state.close_file(&uri);
        state.open_file(&uri, 0, "% blah".into());
        state.close_file(&uri);
        state.open_file(&uri, 0, "% blah".into());
        state.close_file(&uri);
    }
}
