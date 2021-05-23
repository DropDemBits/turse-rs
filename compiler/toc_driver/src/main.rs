//! Dummy bin for running the new scanner and parser

use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::{env, fs, io, sync::Arc};

use toc_vfs::FileDb;

fn load_contents(path: &str) -> io::Result<String> {
    let contents = fs::read(path)?;
    let contents = String::from_utf8_lossy(&contents).to_string();
    Ok(contents)
}

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = load_contents(&path).expect("Unable to load file");
    let file_db = FileDb::new();

    // Add the root path to the file db
    let root_file = file_db.add_file(&path, &contents);
    let mut unit_map = toc_hir::UnitMapBuilder::new();

    // Parse root CST
    let parsed = {
        let info = file_db.get_file(root_file);
        let parsed = toc_parser::parse(Some(root_file), &info.source);
        let dependencies = toc_driver::gather_dependencies(Some(root_file), parsed.syntax());
        // TODO: Gather dependencies from root CST, and parse them

        println!("Parsed output: {}", parsed.dump_tree());
        println!("Dependencies: {:#?}", dependencies);

        parsed
    };

    // TODO: Deal with include globs

    let (validate_res, hir_res) = {
        let validate_res = toc_validate::validate_ast(Some(root_file), parsed.syntax());
        let hir_res = toc_hir_lowering::lower_ast(Some(root_file), parsed.syntax(), &mut unit_map);

        (validate_res, hir_res)
    };

    let unit_map = Arc::new(unit_map.finish());
    let root_unit = unit_map.get_unit(hir_res.id);
    println!("{:#?}", root_unit);

    // TODO: resolve imports between units

    let analyze_res = toc_analysis::analyze_unit(hir_res.id, unit_map);

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

    let span_mapper = SpanMapper::new(&file_db);

    for msg in msgs {
        has_errors |= matches!(msg.kind(), toc_reporting::AnnotateKind::Error);
        let snippet = span_mapper.message_into_snippet(msg);
        let display_list = annotate_snippets::display_list::DisplayList::from(snippet);

        println!("{}", display_list);
    }

    std::process::exit(if has_errors { -1 } else { 0 });
}

struct SpanMapper {
    files: HashMap<toc_span::FileId, (Arc<toc_vfs::FileInfo>, Vec<RangeInclusive<usize>>)>,
}

impl SpanMapper {
    fn new(file_db: &toc_vfs::FileDb) -> Self {
        let mut files = HashMap::new();

        for file in file_db.files() {
            let info = file_db.get_file(file);
            let line_ranges = Self::build_line_ranges(&info.source);

            files.insert(file, (info, line_ranges));
        }

        Self { files }
    }

    fn build_line_ranges(source: &str) -> Vec<RangeInclusive<usize>> {
        let mut line_ranges = vec![];
        let mut line_start = 0;
        let line_ends = source.char_indices().filter(|(_, c)| matches!(c, '\n'));

        for (at_newline, _) in line_ends {
            let line_end = at_newline + 1;
            line_ranges.push(line_start..=line_end);
            line_start = line_end;
        }

        // Use a line span covering the rest of the file
        line_ranges.push(line_start..=source.len());

        line_ranges
    }

    fn map_byte_index(
        &self,
        file: Option<toc_span::FileId>,
        byte_idx: usize,
    ) -> Option<(usize, RangeInclusive<usize>)> {
        self.files.get(file.as_ref()?).and_then(|(_, line_ranges)| {
            line_ranges
                .iter()
                .enumerate()
                .find(|(_line, range)| range.contains(&byte_idx))
                .map(|(line, range)| (line, range.clone()))
        })
    }

    fn message_into_snippet<'a>(
        &'a self,
        msg: &'a toc_reporting::ReportMessage,
    ) -> annotate_snippets::snippet::Snippet<'a> {
        use annotate_snippets::{display_list::FormatOptions, snippet::*};

        // Build a set of common snippets for consecutive messages

        // Improvements:
        // Could fold together spans per file
        // - in a report message, the file id should be the same
        // - if different files are needed, need to break up into separate reports

        // Build snippet slices & footers
        let mut slices = vec![];
        let mut footer = vec![];

        let span_into_slice = |annotate_type, span: toc_span::Span, label| {
            let file = span.file.unwrap();

            let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));
            let (start_line, start_range) = self.map_byte_index(span.file, start as usize).unwrap();
            let (end_line, end_range) = self.map_byte_index(span.file, end as usize).unwrap();

            let source = &self.files.get(&file).unwrap().0.source;
            let slice_text = *start_range.start()..*end_range.end();
            let slice_text = &source[slice_text];

            let range_base = start_range.start();
            let can_fold = (end_line - start_line) > 10;

            Slice {
                source: slice_text,
                line_start: start_line + 1,
                origin: Some(&self.files.get(&file).unwrap().0.path),
                annotations: vec![SourceAnnotation {
                    annotation_type: annotate_type,
                    label,
                    range: (start as usize - range_base, end as usize - range_base),
                }],
                fold: can_fold,
            }
        };

        fn annotate_kind_to_type(kind: toc_reporting::AnnotateKind) -> AnnotationType {
            match kind {
                toc_reporting::AnnotateKind::Note => AnnotationType::Note,
                toc_reporting::AnnotateKind::Info => AnnotationType::Info,
                toc_reporting::AnnotateKind::Warning => AnnotationType::Warning,
                toc_reporting::AnnotateKind::Error => AnnotationType::Error,
            }
        }

        // Insert the first slice
        slices.push(span_into_slice(
            annotate_kind_to_type(msg.kind()),
            msg.span(),
            "", // part of the larger message
        ));

        for annotate in msg.annotations() {
            slices.push(span_into_slice(
                annotate_kind_to_type(annotate.kind()),
                annotate.span(),
                annotate.message(),
            ));
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

        snippet
    }
}
