//! Dummy bin for running the new scanner and parser

use std::{collections::HashMap, fs, sync::Arc};

use toc_analysis::db::HirAnalysis;
use toc_ast_db::{
    db::{AstDatabaseExt, SourceParser, SpanMapping},
    SourceGraph,
};
use toc_hir::library_graph::Library;
use toc_hir_db::db::HirDatabase;
use toc_salsa::salsa;
use toc_span::{FileId, Span};
use toc_vfs::FileLoader;
use toc_vfs_db::db::FileSystem;

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
    let mut db = MainDatabase::default();

    // Add the root path to the db
    let root_file = db.vfs.intern_path(path);

    // Set the source root
    let mut source_graph = SourceGraph::default();
    let _lib_id = source_graph.add_library(Library {
        artifact: toc_hir::library_graph::ArtifactKind::Binary,
        name: maybe_name,
        root: root_file,
    });
    db.set_source_graph(Arc::new(source_graph));
    db.invalidate_source_graph(&loader);

    // Dump requested information
    if let Some(dump_mode) = args.dump {
        match dump_mode {
            config::DumpMode::Ast => {
                // Show CST + dependencies for the current file
                let parsed = db.parse_file(root_file);
                let tree = parsed.result();
                let dependencies = db.parse_depends(root_file);

                println!("Parsed output: {}", tree.dump_tree());
                println!("Dependencies: {:#?}", dependencies.result());
            }
            config::DumpMode::Hir => {
                // Dump library graph
                println!("Libraries:");
                let library_graph = db.source_graph();

                for (lib_id, lib) in library_graph.all_libraries() {
                    let file = lib.root;
                    println!(
                        "{file:?}: {tree}",
                        tree = toc_hir_pretty::tree::pretty_print_tree(&db.library(lib_id))
                    );
                }
            }
            config::DumpMode::HirGraph => {
                let out = toc_hir_pretty::graph::pretty_print_graph(&db.source_graph(), |lib_id| {
                    db.library(lib_id)
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

        let start = db.map_byte_index_to_character(file, start).unwrap();
        let end = db.map_byte_index_to_character(file, end).unwrap();

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
                let value = ariadne::Source::from(&*self.db.file_source(*id).0);
                entry.insert(value)
            }
        })
    }

    fn display<'a>(&self, id: &'a FileId) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(self.db.file_path(*id)))
    }
}

#[salsa::database(
    toc_vfs_db::db::FileSystemStorage,
    toc_ast_db::db::SpanMappingStorage,
    toc_ast_db::db::SourceParserStorage,
    toc_hir_db::db::HirDatabaseStorage,
    toc_analysis::db::TypeInternStorage,
    toc_analysis::db::TypeDatabaseStorage,
    toc_analysis::db::ConstEvalStorage,
    toc_analysis::db::HirAnalysisStorage
)]
#[derive(Default)]
struct MainDatabase {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for MainDatabase {}

toc_vfs::impl_has_vfs!(MainDatabase, vfs);

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
