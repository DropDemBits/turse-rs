//! Span mapping related structures

use std::ops::Range;
use std::sync::Arc;

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

impl LineInfo {
    pub fn new(line: usize, line_span: Range<usize>) -> Self {
        Self { line, line_span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineMapping {
    source: Arc<String>,
    line_starts: Vec<usize>,
}

impl LineMapping {
    fn from_source(source: Arc<String>) -> Self {
        let mut line_starts = vec![];
        let mut start = 0;
        let line_ends = source.char_indices().filter(|(_, c)| matches!(c, '\n'));

        for (at_newline, _) in line_ends {
            line_starts.push(start);
            start = at_newline + 1;
        }

        // Use a line start covering the rest of the file
        line_starts.push(start);
        Self {
            source,
            line_starts,
        }
    }

    fn map_index(&self, index: usize) -> Option<LineInfo> {
        if !self.source.is_char_boundary(index) {
            // Outside of the file or not a char boundary
            return None;
        }

        let (line, end) = self
            .line_starts
            .iter()
            .enumerate()
            .find(|(_line, start)| index < **start)
            .map(|(line, start)| (line - 1, *start))
            .unwrap_or((self.line_starts.len() - 1, self.source.len()));

        // `line_starts` is always greater than 1, it's ok to unwrap
        // also bounded by `line_starts.len()`
        let start = *self.line_starts.get(line).expect("no line infos");

        Some(LineInfo {
            line,
            line_span: start..end,
        })
    }

    fn map_index_to_position(&self, index: usize) -> Option<LspPosition> {
        let info = self.map_index(index)?;
        let source_slice = &self.source[info.line_span.start..index];

        // Get character count in UTF-16 chars
        let column_offset = source_slice.encode_utf16().count();

        Some(LspPosition::new(info.line as u32, column_offset as u32))
    }
}

pub(crate) mod query {
    //! Span query impls
    use std::sync::Arc;

    use crate::db;
    use crate::span::{LineInfo, LineMapping, LspPosition};

    pub(crate) fn line_mapping(
        db: &dyn db::SpanMapping,
        file_id: toc_span::FileId,
    ) -> Arc<LineMapping> {
        let source = db.file_source(file_id).0;
        Arc::new(LineMapping::from_source(source))
    }

    pub(crate) fn file_path(db: &dyn db::SpanMapping, file_id: toc_span::FileId) -> Arc<String> {
        Arc::new(db.get_vfs().lookup_path(file_id).display().to_string())
    }

    pub(crate) fn map_byte_index(
        db: &dyn db::SpanMapping,
        file_id: toc_span::FileId,
        index: usize,
    ) -> Option<LineInfo> {
        db.line_mapping(file_id).map_index(index)
    }

    pub(crate) fn map_byte_index_to_position(
        db: &dyn db::SpanMapping,
        file_id: toc_span::FileId,
        index: usize,
    ) -> Option<LspPosition> {
        db.line_mapping(file_id).map_index_to_position(index)
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use crate::span::{LineInfo, LineMapping, LspPosition};

    #[test]
    fn empty_file() {
        let map = LineMapping::from_source(Arc::new("".into()));

        assert_eq!(map.map_index(0), Some(LineInfo::new(0, 0..0)));
        assert_eq!(map.map_index(1), None);
        assert_eq!(map.map_index(2), None);

        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        assert_eq!(map.map_index_to_position(1), None);
        assert_eq!(map.map_index_to_position(2), None);
    }

    #[test]
    fn single_line() {
        let source = Arc::new("abcdefg".to_string());
        let map = LineMapping::from_source(source.clone());

        assert_eq!(map.map_index(0), Some(LineInfo::new(0, 0..source.len())));
        assert_eq!(map.map_index(1), Some(LineInfo::new(0, 0..source.len())));
        assert_eq!(map.map_index(2), Some(LineInfo::new(0, 0..source.len())));
        assert_eq!(
            map.map_index(source.len()),
            Some(LineInfo::new(0, 0..source.len()))
        );
        assert_eq!(map.map_index(source.len() + 1), None);

        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        assert_eq!(map.map_index_to_position(1), Some(LspPosition::new(0, 1)));
        assert_eq!(map.map_index_to_position(2), Some(LspPosition::new(0, 2)));
        assert_eq!(
            map.map_index_to_position(source.len()),
            Some(LspPosition::new(0, source.len() as u32))
        );
    }

    #[test]
    fn only_newline() {
        let source = Arc::new("\n".to_string());
        let map = LineMapping::from_source(source);

        assert_eq!(map.map_index(0), Some(LineInfo::new(0, 0..1)));
        assert_eq!(map.map_index(1), Some(LineInfo::new(1, 1..1)));
        assert_eq!(map.map_index(2), None);

        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        assert_eq!(map.map_index_to_position(1), Some(LspPosition::new(1, 0)));
    }

    #[test]
    fn newline_and_text() {
        let source = Arc::new("abcd\nab".to_string());
        let map = LineMapping::from_source(source);

        assert_eq!(map.map_index(0), Some(LineInfo::new(0, 0..5)));
        assert_eq!(map.map_index(1), Some(LineInfo::new(0, 0..5)));
        assert_eq!(map.map_index(2), Some(LineInfo::new(0, 0..5)));
        assert_eq!(map.map_index(3), Some(LineInfo::new(0, 0..5)));
        assert_eq!(map.map_index(4), Some(LineInfo::new(0, 0..5)));
        assert_eq!(map.map_index(5), Some(LineInfo::new(1, 5..7)));
        assert_eq!(map.map_index(6), Some(LineInfo::new(1, 5..7)));
        assert_eq!(map.map_index(7), Some(LineInfo::new(1, 5..7)));
        assert_eq!(map.map_index(8), None);

        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        assert_eq!(map.map_index_to_position(1), Some(LspPosition::new(0, 1)));
        assert_eq!(map.map_index_to_position(5), Some(LspPosition::new(1, 0)));
        assert_eq!(map.map_index_to_position(6), Some(LspPosition::new(1, 1)));
    }

    #[test]
    fn with_unicode() {
        // |a|£|_|£|_|💖|_|_|_|a|\n|a|
        let source = Arc::new("a££💖a\na".to_string());
        let map = LineMapping::from_source(source.clone());

        // start of first 'a'
        assert_eq!(map.map_index(0), Some(LineInfo::new(0, 0..11)));
        // start of first pound
        assert_eq!(map.map_index(1), Some(LineInfo::new(0, 0..11)));
        assert_eq!(map.map_index(2), None);
        // start of second pound
        assert_eq!(map.map_index(3), Some(LineInfo::new(0, 0..11)));
        assert_eq!(map.map_index(4), None);
        // start of heart
        assert_eq!(map.map_index(5), Some(LineInfo::new(0, 0..11)));
        for i in 6..9 {
            assert_eq!(map.map_index(i), None);
        }
        // start of second 'a'
        assert_eq!(map.map_index(9), Some(LineInfo::new(0, 0..11)));
        // newline
        assert_eq!(map.map_index(10), Some(LineInfo::new(0, 0..11)));
        // start of third 'a'
        assert_eq!(map.map_index(11), Some(LineInfo::new(1, 11..12)));
        // eof
        assert_eq!(map.map_index(12), Some(LineInfo::new(1, 11..12)));
        assert_eq!(map.map_index(13), None);

        // a
        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        // £
        assert_eq!(map.map_index_to_position(1), Some(LspPosition::new(0, 1)));
        // £
        assert_eq!(map.map_index_to_position(3), Some(LspPosition::new(0, 2)));
        // 💖
        assert_eq!(map.map_index_to_position(5), Some(LspPosition::new(0, 3)));
        // a
        assert_eq!(map.map_index_to_position(9), Some(LspPosition::new(0, 5)));
        // \n
        assert_eq!(map.map_index_to_position(10), Some(LspPosition::new(0, 6)));
        // a
        assert_eq!(map.map_index_to_position(11), Some(LspPosition::new(1, 0)));
        // eof
        assert_eq!(map.map_index_to_position(12), Some(LspPosition::new(1, 1)));
    }
}
