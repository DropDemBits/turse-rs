//! Span mapping related structures

use std::{ops::Range, sync::Arc};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineInfo {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl LineInfo {
    pub fn line_span(&self) -> Range<usize> {
        self.start..self.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineMapping {
    source: Arc<String>,
    line_infos: Vec<LineInfo>,
}

impl LineMapping {
    fn from_source(source: Arc<String>) -> Self {
        let mut line_infos = vec![];
        let mut line = 0;
        let mut start = 0;
        let line_ends = source.char_indices().filter(|(_, c)| matches!(c, '\n'));

        for (at_newline, _) in line_ends {
            let end = at_newline + 1;
            line_infos.push(LineInfo { line, start, end });
            start = end;
            line += 1;
        }

        // Use a line start covering the rest of the file
        debug_assert_eq!(line_infos.len(), line);
        line_infos.push(LineInfo {
            line,
            start,
            end: source.len(),
        });

        Self { source, line_infos }
    }

    fn map_index(&self, index: usize) -> Option<LineInfo> {
        if !self.source.is_char_boundary(index) {
            // Outside of the file or not a char boundary
            return None;
        }

        let line = self
            .line_infos
            .iter()
            .enumerate()
            .find_map(|(line, info)| (index < info.start).then(|| line - 1))
            .unwrap_or(self.line_infos.len() - 1);

        // `line_starts` is always greater than 1, it's ok to unwrap
        // also bounded by `line_starts.len()`
        //
        // info should also exist at this point
        let info = *self.line_infos.get(line).expect("no line infos");

        Some(info)
    }

    fn map_index_to_position(&self, index: usize) -> Option<LspPosition> {
        let info = self.map_index(index)?;
        let source_slice = &self.source[info.start..index];

        // Get character count in UTF-16 chars
        let column_offset = source_slice.encode_utf16().count();

        Some(LspPosition::new(info.line as u32, column_offset as u32))
    }

    fn map_index_to_character(&self, index: usize) -> Option<usize> {
        if !self.source.is_char_boundary(index) {
            return None;
        }

        let source_slice = &self.source[0..index];

        // Get character count in UTF-32 chars
        // Exclude `\r` because `ariadne` doesn't include it in its character counts
        let char_index = source_slice.chars().filter(|c| *c != '\r').count();

        Some(char_index)
    }
}

pub(crate) mod query {
    //! Span query impls
    use std::sync::Arc;

    use crate::{
        db,
        span::{LineInfo, LineMapping, LspPosition},
    };

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

    pub(crate) fn map_byte_index_to_character(
        db: &dyn db::SpanMapping,
        file_id: toc_span::FileId,
        index: usize,
    ) -> Option<usize> {
        db.line_mapping(file_id).map_index_to_character(index)
    }
}

#[cfg(test)]
mod test {
    use std::{ops::Range, sync::Arc};

    use crate::span::{LineInfo, LineMapping, LspPosition};

    fn line_span(info: LineInfo) -> (usize, Range<usize>) {
        (info.line, info.line_span())
    }

    #[test]
    fn empty_file() {
        let map = LineMapping::from_source(Arc::new("".into()));

        assert_eq!(map.map_index(0).map(line_span), Some((0, 0..0)));
        assert_eq!(map.map_index(1).map(line_span), None);
        assert_eq!(map.map_index(2).map(line_span), None);

        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        assert_eq!(map.map_index_to_position(1), None);
        assert_eq!(map.map_index_to_position(2), None);
    }

    #[test]
    fn single_line() {
        let source = Arc::new("abcdefg".to_string());
        let map = LineMapping::from_source(source.clone());

        assert_eq!(map.map_index(0).map(line_span), Some((0, 0..source.len())));
        assert_eq!(map.map_index(1).map(line_span), Some((0, 0..source.len())));
        assert_eq!(map.map_index(2).map(line_span), Some((0, 0..source.len())));
        assert_eq!(
            map.map_index(source.len()).map(line_span),
            Some((0, 0..source.len()))
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

        assert_eq!(map.map_index(0).map(line_span), Some((0, 0..1)));
        assert_eq!(map.map_index(1).map(line_span), Some((1, 1..1)));
        assert_eq!(map.map_index(2).map(line_span), None);

        assert_eq!(map.map_index_to_position(0), Some(LspPosition::new(0, 0)));
        assert_eq!(map.map_index_to_position(1), Some(LspPosition::new(1, 0)));
    }

    #[test]
    fn newline_and_text() {
        let source = Arc::new("abcd\nab".to_string());
        let map = LineMapping::from_source(source);

        assert_eq!(map.map_index(0).map(line_span), Some((0, 0..5)));
        assert_eq!(map.map_index(1).map(line_span), Some((0, 0..5)));
        assert_eq!(map.map_index(2).map(line_span), Some((0, 0..5)));
        assert_eq!(map.map_index(3).map(line_span), Some((0, 0..5)));
        assert_eq!(map.map_index(4).map(line_span), Some((0, 0..5)));
        assert_eq!(map.map_index(5).map(line_span), Some((1, 5..7)));
        assert_eq!(map.map_index(6).map(line_span), Some((1, 5..7)));
        assert_eq!(map.map_index(7).map(line_span), Some((1, 5..7)));
        assert_eq!(map.map_index(8).map(line_span), None);

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
        assert_eq!(map.map_index(0).map(line_span), Some((0, 0..11)));
        // start of first pound
        assert_eq!(map.map_index(1).map(line_span), Some((0, 0..11)));
        assert_eq!(map.map_index(2).map(line_span), None);
        // start of second pound
        assert_eq!(map.map_index(3).map(line_span), Some((0, 0..11)));
        assert_eq!(map.map_index(4).map(line_span), None);
        // start of heart
        assert_eq!(map.map_index(5).map(line_span), Some((0, 0..11)));
        for i in 6..9 {
            assert_eq!(map.map_index(i), None);
        }
        // start of second 'a'
        assert_eq!(map.map_index(9).map(line_span), Some((0, 0..11)));
        // newline
        assert_eq!(map.map_index(10).map(line_span), Some((0, 0..11)));
        // start of third 'a'
        assert_eq!(map.map_index(11).map(line_span), Some((1, 11..12)));
        // eof
        assert_eq!(map.map_index(12).map(line_span), Some((1, 11..12)));
        assert_eq!(map.map_index(13).map(line_span), None);

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

    #[test]
    fn ignore_cr_in_char_count() {
        let source = Arc::new("a\r\nb".to_string());
        let map = LineMapping::from_source(source.clone());

        assert_eq!(map.map_index_to_character(0), Some(0));
        assert_eq!(map.map_index_to_character(1), Some(1));
        assert_eq!(map.map_index_to_character(2), Some(1));
        assert_eq!(map.map_index_to_character(3), Some(2));
    }
}
