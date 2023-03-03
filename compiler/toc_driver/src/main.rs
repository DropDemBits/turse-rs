//! Dummy bin for running the new scanner and parser

use std::{collections::HashMap, fs};

use camino::{Utf8Path, Utf8PathBuf};
use toc_analysis::db::HirAnalysis;
use toc_hir::library_graph::{DependencyList, SourceLibrary};
use toc_paths::RawPath;
use toc_source_graph::RootLibraries;
use toc_span::{FileId, Span};
use toc_vfs::FileLoader;
use toc_vfs_db::{SourceTable, VfsBridge};

mod config;

// Unrelated FIXMEs:
// FIXME(toc_hir_lowering): Deal with include globs
// FIXME: resolve imports between units

fn main() {
    use clap::Parser;

    let args = config::Args::parse();

    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(args.log_level.unwrap_or_default())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting global subscriber failed");

    let loader = MainFileLoader::default();
    let path = std::path::Path::new(&args.source_file);
    let maybe_name = path
        .file_name()
        .map(|name| name.to_string_lossy().to_string())
        .unwrap_or_else(|| "main".to_string());
    let path = loader.normalize_path(path).unwrap_or_else(|| path.into());
    let output_path = path.with_extension("tbc");
    let db = MainDatabase::default();

    // Add the root path to the db
    let root_file = RawPath::new(&db, path.try_into().unwrap());

    // Set the source root
    let _lib_id = SourceLibrary::new(
        &db,
        maybe_name,
        root_file,
        toc_hir::library_graph::ArtifactKind::Binary,
        DependencyList::empty(&db),
    );
    RootLibraries::new(&db, vec![_lib_id]);

    // Dump requested information
    if let Some(dump_mode) = args.dump {
        match dump_mode {
            config::DumpMode::Ast => {
                // Show CST + dependencies for the current file
                let source = toc_vfs_db::source_of(&db, root_file);
                let parsed = toc_ast_db::parse_file(&db, source);
                let tree = parsed.result();
                let dependencies = toc_ast_db::parse_depends(&db, source);

                println!("Parsed output: {}", tree.dump_tree());
                println!("Dependencies: {:#?}", dependencies.result());
            }
            config::DumpMode::Hir => {
                use toc_hir_db::Db;

                // Dump library graph
                println!("Libraries:");
                let source_graph = toc_source_graph::source_graph(&db).as_ref().ok().unwrap();

                for &library in source_graph.all_libraries(&db) {
                    let file = library.root(&db);
                    println!(
                        "{file:?}: {tree}",
                        tree = toc_hir_pretty::tree::pretty_print_tree(&db.library(library.into()))
                    );
                }
            }
            config::DumpMode::HirGraph => {
                use toc_hir_db::Db;

                let source_graph = toc_source_graph::source_graph(&db).as_ref().ok().unwrap();
                let out =
                    toc_hir_pretty::graph::pretty_print_graph(&db, *source_graph, |library| {
                        db.library(library)
                    });
                println!("{out}");
            }
        }
    }

    let codegen_res = if args.lint {
        // Lint-only mode
        db.analyze_libraries().map(|_| None)
    } else {
        // Do codegen
        toc_hir_codegen::generate_code(&db)
    };

    // We only need to get the messages for the queries at the end of the chain
    let msgs = codegen_res.messages();
    let mut cache = VfsCache::new(&db);

    for msg in msgs.iter() {
        emit_message(&db, &mut cache, msg);
    }
    msgs.assert_no_delayed_reports();

    if let Some(blob) = codegen_res.result() {
        let mut encoded = vec![];
        blob.encode_to(&db, &mut encoded)
            .expect("failed to encode bytecode");
        std::fs::write(output_path, encoded).expect("failed to write bytecode");
    }

    std::process::exit(if msgs.has_errors() { -1 } else { 0 });
}

fn emit_message(db: &MainDatabase, cache: &mut VfsCache, msg: &toc_reporting::ReportMessage) {
    use ariadne::{Color, Config, Label, LabelAttach, ReportKind};
    use std::ops::Range;

    fn mk_range(db: &MainDatabase, span: Span) -> Option<(FileId, Range<usize>)> {
        let (file, range) = span.into_parts()?;
        let start: usize = range.start().into();
        let end: usize = range.end().into();

        let start = toc_ast_db::map_byte_index_to_character(db, file.into_raw(), start).unwrap();
        let end = toc_ast_db::map_byte_index_to_character(db, file.into_raw(), end).unwrap();

        Some((file, start..end))
    }

    fn kind_to_colour(kind: toc_reporting::AnnotateKind) -> Color {
        match kind {
            toc_reporting::AnnotateKind::Note => Color::Cyan,
            toc_reporting::AnnotateKind::Info => Color::Unset,
            toc_reporting::AnnotateKind::Warning => Color::Yellow,
            toc_reporting::AnnotateKind::Error => Color::Red,
        }
    }

    let kind = match msg.kind() {
        toc_reporting::AnnotateKind::Note => ReportKind::Advice,
        toc_reporting::AnnotateKind::Info => ReportKind::Advice,
        toc_reporting::AnnotateKind::Warning => ReportKind::Warning,
        toc_reporting::AnnotateKind::Error => ReportKind::Error,
    };

    let top_span = msg.span();
    let Some((file, range)) = mk_range(db, top_span) else {
        // Notify that we've encountered a bad span
        // Missing files don't fall under here, as they use the file they're missing from
        tracing::error!("BUG: Encountered bad message span (Original message: {msg:#?})");
        return;
    };

    let config = Config::default().with_label_attach(LabelAttach::End);
    let mut builder = ariadne::Report::build(kind, file, range.start)
        .with_message(msg.message())
        .with_config(config);

    for (order, annotate) in msg.annotations().iter().enumerate() {
        let span = if let Some(span) = mk_range(db, annotate.span()) {
            span
        } else {
            // Notify that we've encountered a bad span
            tracing::error!(
                "BUG: Encountered bad annotation span (Original annotation: {annotate:#?})",
            );
            continue;
        };

        builder = builder.with_label(
            Label::new(span)
                .with_message(annotate.message())
                .with_order(order as i32)
                .with_color(kind_to_colour(annotate.kind())),
        );
    }

    if let Some(footer) = msg.footer().first() {
        builder = builder.with_note(footer.message());
    }

    builder.finish().eprint(cache).unwrap();
}

struct VfsCache<'db> {
    db: &'db MainDatabase,
    sources: HashMap<FileId, ariadne::Source>,
}

impl<'db> VfsCache<'db> {
    fn new(db: &'db MainDatabase) -> Self {
        Self {
            db,
            sources: Default::default(),
        }
    }
}

impl ariadne::Cache<FileId> for VfsCache<'_> {
    fn fetch(&mut self, id: &FileId) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        use std::collections::hash_map::Entry;

        Ok(match self.sources.entry(*id) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let source = toc_vfs_db::source_of(self.db, id.into_raw());
                let value = ariadne::Source::from(source.contents(self.db));
                entry.insert(value)
            }
        })
    }

    fn display<'a>(&self, id: &'a FileId) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id.into_raw().raw_path(self.db).clone()))
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
struct MainDatabase {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl salsa::Database for MainDatabase {}

impl VfsBridge for MainDatabase {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }

    fn load_new_file(&self, path: toc_paths::RawPath) -> (String, Option<toc_vfs::LoadError>) {
        match fs::read(path.raw_path(self).as_path()) {
            Ok(contents) => (String::from_utf8_lossy(&contents).into_owned(), None),
            Err(err) => match err.kind() {
                std::io::ErrorKind::NotFound => (
                    String::new(),
                    Some(toc_vfs::LoadError::new("", toc_vfs::ErrorKind::NotFound)),
                ),
                _ => (
                    String::new(),
                    Some(toc_vfs::LoadError::new(
                        "",
                        toc_vfs::ErrorKind::Other(std::sync::Arc::new(err.to_string())),
                    )),
                ),
            },
        }
    }

    fn normalize_path(&self, path: &Utf8Path) -> Utf8PathBuf {
        fs::canonicalize(path.as_std_path())
            .ok()
            .and_then(|path| Utf8PathBuf::try_from(path).ok())
            .unwrap_or_else(|| path.to_owned())
    }
}

#[derive(Default)]
struct MainFileLoader {}

impl toc_vfs::FileLoader for MainFileLoader {
    fn load_file(&self, path: &std::path::Path) -> toc_vfs::LoadResult {
        match fs::read(path) {
            Ok(contents) => Ok(toc_vfs::LoadStatus::Modified(contents)),
            Err(err) => match err.kind() {
                std::io::ErrorKind::NotFound => {
                    Err(toc_vfs::LoadError::new(path, toc_vfs::ErrorKind::NotFound))
                }
                _ => Err(toc_vfs::LoadError::new(
                    path,
                    toc_vfs::ErrorKind::Other(std::sync::Arc::new(err.to_string())),
                )),
            },
        }
    }

    fn normalize_path(&self, path: &std::path::Path) -> Option<std::path::PathBuf> {
        fs::canonicalize(path).ok()
    }
}
