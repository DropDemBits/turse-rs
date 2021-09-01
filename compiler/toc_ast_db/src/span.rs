//! Span mapping related queries & structures

use std::{ops::Range, sync::Arc};

use toc_salsa::salsa;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LspPosition {
    pub line: u32,
    pub column: u32,
}

impl LspPosition {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineInfo {
    pub line: usize,
    pub line_span: Range<usize>,
}

#[salsa::query_group(SpanMappingStorage)]
pub trait SpanMapping: toc_vfs::db::FileSystem {
    fn line_ranges(&self, file_id: toc_span::FileId) -> Arc<Vec<Range<usize>>>;

    fn file_path(&self, file_id: toc_span::FileId) -> Arc<String>;

    fn map_byte_index(&self, file: toc_span::FileId, byte_idx: usize) -> Option<LineInfo>;

    fn map_byte_index_to_position(
        &self,
        file: toc_span::FileId,
        byte_idx: usize,
    ) -> Option<LspPosition>;
}

fn line_ranges(db: &dyn SpanMapping, file_id: toc_span::FileId) -> Arc<Vec<Range<usize>>> {
    let source = &db.file_source(file_id).0;
    let mut line_ranges = vec![];
    let mut line_start = 0;
    let line_ends = source.char_indices().filter(|(_, c)| matches!(c, '\n'));

    for (at_newline, _) in line_ends {
        let line_end = at_newline + 1;
        line_ranges.push(line_start..line_end);
        line_start = line_end;
    }

    // Use a line span covering the rest of the file
    line_ranges.push(line_start..source.len());

    Arc::new(line_ranges)
}

fn file_path(db: &dyn SpanMapping, file_id: toc_span::FileId) -> Arc<String> {
    Arc::new(db.get_vfs().lookup_path(file_id).display().to_string())
}

fn map_byte_index(
    db: &dyn SpanMapping,
    file_id: toc_span::FileId,
    byte_idx: usize,
) -> Option<LineInfo> {
    db.line_ranges(file_id)
        .iter()
        .enumerate()
        .find(|(_line, range)| (range.start..=range.end).contains(&byte_idx))
        .map(|(line, range)| LineInfo {
            line,
            line_span: range.clone(),
        })
}

fn map_byte_index_to_position(
    db: &dyn SpanMapping,
    file_id: toc_span::FileId,
    byte_idx: usize,
) -> Option<LspPosition> {
    let info = db.map_byte_index(file_id, byte_idx).unwrap();

    let source_slice = &db.file_source(file_id).0[info.line_span.start..byte_idx];

    // Get character count in UTF-16 chars
    let column_offset = source_slice.encode_utf16().count();

    Some(LspPosition::new(info.line as u32, column_offset as u32))
}
