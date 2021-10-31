use std::borrow::Cow;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lsp_types::{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position};
use toc_analysis::db::HirAnalysis;
use toc_ast_db::db::{AstDatabaseExt, SourceParser, SpanMapping};
use toc_ast_db::SourceGraph;
use toc_salsa::salsa;
use toc_vfs::{LoadError, LoadStatus};
use toc_vfs_db::db::VfsDatabaseExt;

#[derive(Default)]
pub(crate) struct ServerState {
    db: LspDatabase,
    files: FileStore,
}

impl ServerState {
    pub(crate) fn open_file(&mut self, uri: &lsp_types::Url, version: i32, text: String) {
        // This is where we update the source graph, as well as the file sources, and the file store
        let path = Path::new(uri.path());
        // Track the file in the file store
        self.files.add_file(path, text, version);

        // Push updated sources & source graph into db
        self.update_file(path, false);
    }

    pub(crate) fn close_file(&mut self, uri: &lsp_types::Url) {
        // This is where we remove files from the source graph (if applicable / non-root) and from the file store
        let path = Path::new(uri.path());

        // Notify that the editor isn't using the file anymore
        self.files.remove_file(path);

        // Push removed sources & updated source graph
        self.update_file(path, true);
    }

    pub(crate) fn apply_changes(
        &mut self,
        uri: &lsp_types::Url,
        version: i32,
        changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    ) {
        let path = Path::new(uri.path());

        self.files.apply_changes(path, version, changes);

        // Push updated sources & source graph into db
        self.update_file(path, false);
    }

    fn update_file(&mut self, path: &Path, removed: bool) {
        let db = &mut self.db;
        let contents = self.files.source(path).into_owned();

        // Add the root path to the file db
        let root_file = db.vfs.intern_path(path.into());
        let load_status = if !removed {
            Ok(LoadStatus::Modified(contents.into()))
        } else {
            Err(LoadError::NotFound)
        };
        db.update_file(root_file, load_status);

        // Setup source graph
        let mut source_graph = SourceGraph::default();
        source_graph.add_root(root_file);
        db.set_source_graph(Arc::new(source_graph));

        // TODO: Recursively load in files, respecting already loaded files
        let file_loader = LspFileLoader::new(&self.files);
        db.invalidate_source_graph(&file_loader);
    }

    /// Collect diagnostics for all libraries
    pub(crate) fn collect_diagnostics(&self) -> Vec<(PathBuf, Vec<Diagnostic>)> {
        let analyze_res = self.db.analyze_libraries();
        let msgs = analyze_res.messages();

        fn to_diag_level(kind: toc_reporting::AnnotateKind) -> DiagnosticSeverity {
            use toc_reporting::AnnotateKind;

            match kind {
                AnnotateKind::Note => DiagnosticSeverity::Hint,
                AnnotateKind::Info => DiagnosticSeverity::Information,
                AnnotateKind::Warning => DiagnosticSeverity::Warning,
                AnnotateKind::Error => DiagnosticSeverity::Error,
            }
        }

        // Breakup messages into per-file bundles
        let mut bundles = HashMap::new();

        for msg in msgs.iter() {
            // Convert each message into a `Diagnostic`
            let range = self.map_span_to_location(msg.span()).range;
            let severity = to_diag_level(msg.kind());
            let annotations = msg
                .annotations()
                .iter()
                .map(|annotate| DiagnosticRelatedInformation {
                    location: self.map_span_to_location(annotate.span()),
                    message: annotate.message().to_string(),
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
            let file = if let Some(file) = msg.span().file {
                file
            } else {
                // FIXME: Log a warning in this situation
                continue;
            };
            let file_diagnostics = bundles.entry(file).or_insert(vec![]);
            file_diagnostics.push(diagnostic);
        }

        // Add empty bundles for files that are tracked, but don't have any diagnostics
        // This is done to remove any diagnostics from files which previously had some
        for (path, _) in self.files.file_map.iter() {
            let file_id = self
                .db
                .vfs
                .lookup_id(path)
                .expect("all paths should be interned already");
            bundles.entry(file_id).or_insert(vec![]);
        }

        // Convert FileIds into paths
        let bundles = bundles
            .into_iter()
            .map(|(file, bundle)| (self.db.vfs.lookup_path(file).to_path_buf(), bundle))
            .collect();

        bundles
    }

    fn map_span_to_location(&self, span: toc_span::Span) -> Location {
        let db = &self.db;
        let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

        let file = span.file.unwrap();

        let start = db.map_byte_index_to_position(file, start as usize).unwrap();
        let end = db.map_byte_index_to_position(file, end as usize).unwrap();

        let path = &db.file_path(file);

        Location::new(
            lsp_types::Url::from_file_path(path.as_str()).unwrap(),
            lsp_types::Range::new(start.into_position(), end.into_position()),
        )
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

#[salsa::database(
    toc_vfs_db::db::FileSystemStorage,
    toc_ast_db::db::SpanMappingStorage,
    toc_ast_db::db::SourceParserStorage,
    toc_hir_db::db::HirDatabaseStorage,
    toc_hir_db::db::InternedTypeStorage,
    toc_analysis::db::TypeInternStorage,
    toc_analysis::db::TypeDatabaseStorage,
    toc_analysis::db::ConstEvalStorage,
    toc_analysis::db::HirAnalysisStorage
)]
#[derive(Default)]
struct LspDatabase {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for LspDatabase {}

toc_vfs::impl_has_vfs!(LspDatabase, vfs);

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
                source: text,
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
            // We're assuming that we're only getting full text changes only
            assert!(
                change.range.is_none(),
                "found incremental change, which is not handled yet"
            );

            file_info.source = change.text;
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

    fn is_tracked(&self, path: &Path) -> bool {
        self.file_map.contains_key(path)
    }
}

struct FileInfo {
    version: i32,
    source: String,
}

struct LspFileLoader<'a> {
    files: &'a FileStore,
}

impl<'a> LspFileLoader<'a> {
    fn new(files: &'a FileStore) -> Self {
        Self { files }
    }
}

impl<'a> toc_vfs::FileLoader for LspFileLoader<'a> {
    fn load_file(&self, path: &Path) -> toc_vfs::LoadResult {
        if self.files.is_tracked(path) {
            // Tracked, source is unchanged from before
            Ok(toc_vfs::LoadStatus::Unchanged)
        } else {
            // Load in a new source
            use std::fs;

            match fs::read(path) {
                Ok(contents) => Ok(toc_vfs::LoadStatus::Modified(contents)),
                Err(err) => match err.kind() {
                    std::io::ErrorKind::NotFound => Err(toc_vfs::LoadError::NotFound),
                    _ => Err(toc_vfs::LoadError::Other(Arc::new(err.to_string()))),
                },
            }
        }
    }

    fn normalize_path(&self, _path: &Path) -> Option<PathBuf> {
        // No canonicalization to be performed right now
        None
    }
}
