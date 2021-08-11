//! Dummy bin for running the new scanner and parser

use std::ops::Range;
use std::{env, fs, io, sync::Arc};

use toc_common_db::SpanMapping;
use toc_hir::db;
use toc_salsa::salsa;
use toc_vfs::query::{FileSystem, VfsDatabaseExt};

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

    let hir_db = db::HirBuilder::new();

    // Parse root CST
    let parsed = {
        let source = db.file_source(root_file);
        let parsed = toc_parser::parse(Some(root_file), &source.0);
        let dependencies = toc_driver::gather_dependencies(Some(root_file), parsed.syntax());
        // TODO: Gather dependencies from root CST, and parse them

        println!("Parsed output: {}", parsed.dump_tree());
        println!("Dependencies: {:#?}", dependencies);

        parsed
    };

    // TODO: Deal with include globs

    let (validate_res, hir_res) = {
        let validate_res = toc_validate::validate_ast(Some(root_file), parsed.syntax());
        let hir_res = toc_hir_lowering::lower_ast(hir_db.clone(), Some(root_file), parsed.syntax());

        (validate_res, hir_res)
    };

    let hir_db = hir_db.finish();
    let root_unit = hir_db.get_unit(hir_res.id);
    println!("{:#?}", root_unit);

    // TODO: resolve imports between units

    let analyze_res = toc_analysis::analyze_unit(hir_db.clone(), hir_res.id);

    let mut msgs = parsed
        .messages()
        .iter()
        .chain(validate_res.messages().iter())
        .chain(hir_res.messages().iter())
        .chain(analyze_res.messages().iter())
        .collect::<Vec<_>>();

    // Sort by start order
    msgs.sort_by_key(|msg| msg.span().range.start());

    let mut has_errors = false;

    for msg in msgs {
        has_errors |= matches!(msg.kind(), toc_reporting::AnnotateKind::Error);
        let message = message_into_string(&db, msg);

        println!("{}", message);
    }

    std::process::exit(if has_errors { -1 } else { 0 });
}

fn message_into_string(db: &MainDatabase, msg: &toc_reporting::ReportMessage) -> String {
    use annotate_snippets::{
        display_list::{DisplayList, FormatOptions},
        snippet::*,
    };

    // Build a set of common snippets for consecutive annotations
    struct FileSpan {
        path: Arc<String>,
        source: Arc<(String, Option<toc_vfs::LoadError>)>,
        source_range: Range<usize>,
        line_range: Range<usize>,
    }

    let mut merged_spans = vec![msg.span()];

    // Merge spans together
    for annotation in msg.annotations() {
        let span = annotation.span();
        let last_span = merged_spans.last_mut().unwrap();

        if span.file == last_span.file {
            // Merge spans
            last_span.range = last_span.range.cover(span.range);
        } else {
            // Add a new span
            merged_spans.push(span);
        }
    }

    // Get line spans
    let file_spans: Vec<_> = merged_spans
        .into_iter()
        .map(|span| {
            let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

            let file_id = span.file.unwrap();
            let start_info = db.map_byte_index(file_id, start as usize).unwrap();
            let end_info = db.map_byte_index(file_id, end as usize).unwrap();

            let source = db.file_source(file_id);
            let source_range = start_info.line_span.start..end_info.line_span.end;
            let path = db.file_path(file_id);

            FileSpan {
                path,
                source,
                source_range,
                line_range: start_info.line..end_info.line,
            }
        })
        .collect();

    // Build snippet slices & footers
    fn annotate_kind_to_type(kind: toc_reporting::AnnotateKind) -> AnnotationType {
        match kind {
            toc_reporting::AnnotateKind::Note => AnnotationType::Note,
            toc_reporting::AnnotateKind::Info => AnnotationType::Info,
            toc_reporting::AnnotateKind::Warning => AnnotationType::Warning,
            toc_reporting::AnnotateKind::Error => AnnotationType::Error,
        }
    }

    fn span_into_annotation<'a, 'b>(
        annotate_type: AnnotationType,
        span: toc_span::Span,
        label: &'a str,
        file_span: &'b FileSpan,
    ) -> SourceAnnotation<'a> {
        let FileSpan {
            source,
            source_range,
            ..
        } = file_span;
        let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

        let snippet_slice = &source.0[source_range.clone()];
        let range_base = source_range.start;
        let real_slice = (start as usize - range_base)..(end as usize - range_base);

        // Get the real start & end, in characters
        // `annotate-snippets` requires that the range bounds are in characters, not byte indices
        let real_start = snippet_slice[0..real_slice.start].chars().count();
        let real_end = real_start + snippet_slice[real_slice].chars().count();

        SourceAnnotation {
            annotation_type: annotate_type,
            label,
            range: (real_start, real_end),
        }
    }

    fn create_snippet(file_span: &FileSpan) -> Slice {
        let FileSpan {
            path,
            source,
            source_range,
            line_range,
            ..
        } = file_span;

        let slice_text = &(source.0)[source_range.clone()];
        let can_fold = (line_range.end - line_range.start) > 10;

        Slice {
            source: slice_text,
            line_start: line_range.start + 1,
            origin: Some(path),
            annotations: vec![],
            fold: can_fold,
        }
    }

    let mut slices = vec![];
    let mut footer = vec![];
    let mut report_spans = file_spans.iter().peekable();

    // Insert the first slice
    let mut current_file = msg.span().file;

    {
        let annotation = span_into_annotation(
            annotate_kind_to_type(msg.kind()),
            msg.span(),
            "", // part of the larger message
            report_spans.peek().unwrap(),
        );

        let mut slice = create_snippet(report_spans.peek().unwrap());
        slice.annotations.push(annotation);
        slices.push(slice);
    }

    for annotate in msg.annotations() {
        let annotation = span_into_annotation(
            annotate_kind_to_type(annotate.kind()),
            annotate.span(),
            annotate.message(),
            report_spans.peek().unwrap(),
        );

        if current_file != annotate.span().file {
            current_file = annotate.span().file;

            let mut slice = create_snippet(report_spans.peek().unwrap());
            slice.annotations.push(annotation);
            slices.push(slice);
        } else {
            let slice = slices.last_mut().unwrap();
            slice.annotations.push(annotation);
        }
    }

    for annotate in msg.footer() {
        footer.push(Annotation {
            annotation_type: annotate_kind_to_type(annotate.kind()),
            id: None,
            label: Some(annotate.message()),
        });
    }

    let snippet = Snippet {
        title: Some(Annotation {
            label: Some(msg.message()),
            id: None,
            annotation_type: annotate_kind_to_type(msg.kind()),
        }),
        footer,
        slices,
        opt: FormatOptions {
            color: true,
            ..Default::default()
        },
    };

    DisplayList::from(snippet).to_string()
}

#[salsa::database(toc_vfs::query::FileSystemStorage, toc_common_db::SpanMappingStorage)]
#[derive(Default)]
struct MainDatabase {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for MainDatabase {}

impl toc_vfs::HasVfs for MainDatabase {
    fn get_vfs(&self) -> &toc_vfs::Vfs {
        &self.vfs
    }
}
