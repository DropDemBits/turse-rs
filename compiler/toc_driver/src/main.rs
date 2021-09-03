//! Dummy bin for running the new scanner and parser

use std::collections::HashMap;
use std::{env, fs, io};

use toc_analysis::db::HirAnalysis;
use toc_ast_db::db::SourceParser;
use toc_ast_db::db::SpanMapping;
use toc_ast_db::SourceRoots;
use toc_hir_db::db::HirDatabase;
use toc_salsa::salsa;
use toc_span::{FileId, Span};
use toc_vfs::db::{FileSystem, VfsDatabaseExt};

fn load_contents(path: &str) -> io::Result<String> {
    let contents = fs::read(path)?;
    let contents = String::from_utf8_lossy(&contents).to_string();
    Ok(contents)
}

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = load_contents(&path).expect("Unable to load file");
    let mut db = MainDatabase::default();

    // Add the root path to the db
    let root_file = db.vfs.intern_path(path.into());
    db.update_file(root_file, Some(contents.into_bytes()));

    // Set the source root
    let source_roots = SourceRoots::new(vec![root_file]);
    db.set_source_roots(source_roots);

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
