//! Dummy bin for running the new scanner and parser

use std::collections::HashMap;
use std::sync::Arc;
use std::{env, fs};

use toc_analysis::db::HirAnalysis;
use toc_ast_db::db::SpanMapping;
use toc_ast_db::db::{AstDatabaseExt, SourceParser};
use toc_ast_db::SourceGraph;
use toc_hir_db::db::HirDatabase;
use toc_salsa::salsa;
use toc_span::{FileId, Span};
use toc_vfs::db::FileSystem;
use toc_vfs::FileLoader;

fn main() {
    let loader = MainFileLoader::default();
    let str_path: String = env::args().nth(1).expect("Missing path to source file");
    let path = std::path::Path::new(&str_path);
    let path = loader.normalize_path(path).unwrap_or_else(|| path.into());
    let mut db = MainDatabase::default();

    // Add the root path to the db
    let root_file = db.vfs.intern_path(path.into());

    // Set the source root
    let mut source_graph = SourceGraph::default();
    source_graph.add_root(root_file);
    db.set_source_graph(Arc::new(source_graph));
    db.invalidate_source_graph(&loader);

    // Parse root CST & dump output
    // Note: this is only for temporary parse tree dumping
    {
        let parsed = db.parse_file(root_file);
        let tree = parsed.result();
        let dependencies = db.parse_depends(root_file);
        // TODO(toc_ast_db): Add tests for parsing dependencies

        println!("Parsed output: {}", tree.dump_tree());
        println!("Dependencies: {:#?}", dependencies.result());
    }

    // TODO(toc_hir_lowering): Deal with include globs

    // Dump library graph
    println!("Libraries:");
    let lower_res = db.library_graph();
    let library_graph = lower_res.result();

    for (file, lib) in library_graph.library_roots() {
        println!(
            "{:?}: {}",
            file,
            toc_hir_pretty::pretty_print_tree(&db.library(lib))
        );
    }

    // TODO: resolve imports between units
    let analyze_res = db.analyze_libraries();

    // We only need to get the messages for the queries at the end of the chain
    let msgs = analyze_res.messages();

    let mut has_errors = false;
    let mut cache = VfsCache::new(&db);

    // Report any library roots that loaded incorrectly
    for root in db.source_graph().library_roots() {
        let (_, err) = db.file_source(root);

        if let Some(err) = err {
            let path = db.vfs.lookup_path(root).display();

            ariadne::Report::<(FileId, std::ops::Range<usize>)>::build(
                ariadne::ReportKind::Error,
                root,
                0,
            )
            .with_message::<String>(format!("unable to load source for `{}`: {}", path, err))
            .finish()
            .print(&mut cache)
            .unwrap();
        }
    }

    for msg in msgs.iter() {
        has_errors |= matches!(msg.kind(), toc_reporting::AnnotateKind::Error);
        emit_message(&db, &mut cache, msg);
    }

    std::process::exit(if has_errors { -1 } else { 0 });
}

fn emit_message(db: &MainDatabase, cache: &mut VfsCache, msg: &toc_reporting::ReportMessage) {
    use ariadne::{Color, Config, Label, LabelAttach, ReportKind};
    use std::ops::Range;

    fn mk_range(db: &MainDatabase, span: Span) -> (FileId, Range<usize>) {
        let file = span.file.unwrap();
        let start: usize = span.range.start().into();
        let end: usize = span.range.end().into();

        let start = db.map_byte_index_to_character(file, start).unwrap();
        let end = db.map_byte_index_to_character(file, end).unwrap();

        (file, start..end)
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
    let (file, range) = mk_range(db, top_span);

    let config = Config::default().with_label_attach(LabelAttach::End);
    let mut builder = ariadne::Report::build(kind, file, range.start)
        .with_message(msg.message())
        .with_config(config);

    for (order, annotate) in msg.annotations().iter().enumerate() {
        let span = mk_range(db, annotate.span());

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
    toc_vfs::db::FileSystemStorage,
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
                std::io::ErrorKind::NotFound => Err(toc_vfs::LoadError::NotFound),
                _ => Err(toc_vfs::LoadError::Other(std::sync::Arc::new(
                    err.to_string(),
                ))),
            },
        }
    }

    fn normalize_path(&self, path: &std::path::Path) -> Option<std::path::PathBuf> {
        fs::canonicalize(path).ok()
    }
}
