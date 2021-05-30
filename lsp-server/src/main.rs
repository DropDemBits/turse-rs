use std::collections::HashMap;
use std::error::Error;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, InitializeParams, Location,
    Position, PublishDiagnosticsParams, ServerCapabilities, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, VersionedTextDocumentIdentifier,
};

type DynError = Box<dyn Error + Sync + Send>;

fn main() -> Result<(), DynError> {
    // logging on stderr
    eprintln!("Starting LSP server");

    let (connection, io_threads) = Connection::stdio();

    let server_caps = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full)),
        ..ServerCapabilities::default()
    })
    .unwrap();
    let init_params = connection.initialize(server_caps)?;

    main_loop(&connection, init_params)?;
    io_threads.join()?;

    eprintln!("Shutting down LSP server");
    Ok(())
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("listening for messages");

    for msg in &connection.receiver {
        eprintln!("recv {:?}", msg);

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(connection, req)?;
            }
            Message::Response(_resp) => {}
            Message::Notification(notify) => handle_notify(connection, notify)?,
        }
    }

    Ok(())
}

fn handle_request(_connection: &Connection, req: Request) -> Result<(), DynError> {
    fn _cast<Kind: lsp_types::request::Request>(
        req: &mut Option<Request>,
    ) -> Option<(RequestId, Kind::Params)> {
        match req.take().unwrap().extract::<Kind::Params>(Kind::METHOD) {
            Ok(value) => Some(value),
            Err(owned) => {
                *req = Some(owned);
                None
            }
        }
    }
    let req = Some(req);

    eprintln!("recv req {:?}", req);

    Ok(())
}

fn handle_notify(connection: &Connection, notify: Notification) -> Result<(), DynError> {
    fn cast<Kind: lsp_types::notification::Notification>(
        notify: &mut Option<Notification>,
    ) -> Option<Kind::Params> {
        match notify.take().unwrap().extract::<Kind::Params>(Kind::METHOD) {
            Ok(value) => Some(value),
            Err(owned) => {
                *notify = Some(owned);
                None
            }
        }
    }
    let mut notify = Some(notify);

    eprintln!("recv notify {:?}", notify);

    if let Some(params) = cast::<DidOpenTextDocument>(&mut notify) {
        let TextDocumentItem { text, uri, .. } = params.text_document;

        eprintln!("open, sending diagnostics @ {:?}", uri.as_str());
        check_document(connection, uri, &text)?;
    } else if let Some(mut params) = cast::<DidChangeTextDocument>(&mut notify) {
        let VersionedTextDocumentIdentifier { uri, .. } = params.text_document;
        let text = &params.content_changes.pop().unwrap().text;

        eprintln!("change, sending diagnostics @ {:?}", uri.as_str());
        check_document(connection, uri, &text)?;
    }

    Ok(())
}

fn check_document(
    connection: &Connection,
    uri: lsp_types::Url,
    contents: &str,
) -> Result<(), DynError> {
    // Collect diagnostics
    let diagnostics = check_file(&uri, contents);

    connection
        .sender
        .send(Message::Notification(Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            },
        )))?;

    Ok(())
}

fn check_file(uri: &lsp_types::Url, contents: &str) -> Vec<Diagnostic> {
    let path = uri.path();
    let file_db = toc_vfs::FileDb::new();

    // Add the root path to the file db
    let root_file = file_db.add_file(path, contents);
    let mut unit_map = toc_hir::UnitMapBuilder::new();

    // Parse root CST
    let (parsed, dep_messages) = {
        let info = file_db.get_file(root_file);
        let parsed = toc_parser::parse(Some(root_file), &info.source);
        let (_dependencies, messages) =
            toc_driver::gather_dependencies(Some(root_file), parsed.syntax());

        (parsed, messages.finish())
    };

    eprintln!("finished parse @ {:?}", uri.as_str());

    let (validate_res, hir_res) = {
        let validate_res = toc_validate::validate_ast(Some(root_file), parsed.syntax());
        let hir_res = toc_hir_lowering::lower_ast(Some(root_file), parsed.syntax(), &mut unit_map);

        (validate_res, hir_res)
    };

    eprintln!("finished CST validate & lower @ {:?}", uri.as_str());

    let unit_map = Arc::new(unit_map.finish());
    let analyze_res = toc_analysis::analyze_unit(hir_res.id, unit_map);

    eprintln!("finished analysis @ {:?}", uri.as_str());

    let mut msgs = parsed
        .messages()
        .iter()
        .chain(dep_messages.iter())
        .chain(validate_res.messages().iter())
        .chain(hir_res.messages().iter())
        .chain(analyze_res.messages().iter())
        .collect::<Vec<_>>();

    // Sort by start order
    msgs.sort_by_key(|msg| msg.span().range.start());

    eprintln!("finished compile @ {:?}", uri.as_str());

    let span_mapper = SpanMapper::new(&file_db);

    fn to_diag_level(kind: toc_reporting::AnnotateKind) -> DiagnosticSeverity {
        use toc_reporting::AnnotateKind;

        match kind {
            AnnotateKind::Note => DiagnosticSeverity::Hint,
            AnnotateKind::Info => DiagnosticSeverity::Information,
            AnnotateKind::Warning => DiagnosticSeverity::Warning,
            AnnotateKind::Error => DiagnosticSeverity::Error,
        }
    }

    // Convert into `Diagnostic`s
    msgs.iter()
        .map(|msg| {
            let range = span_mapper.map_span_to_location(msg.span()).range;
            let severity = to_diag_level(msg.kind());
            let annotations = msg
                .annotations()
                .iter()
                .map(|annotate| DiagnosticRelatedInformation {
                    location: span_mapper.map_span_to_location(annotate.span()),
                    message: annotate.message().to_string(),
                })
                .collect();

            Diagnostic::new(
                range,
                Some(severity),
                None,
                None,
                msg.message().to_string(),
                Some(annotations),
                None,
            )
        })
        .collect()
}

struct SpanMapper {
    files: HashMap<toc_span::FileId, (Arc<toc_vfs::FileInfo>, Vec<Range<usize>>)>,
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

    fn build_line_ranges(source: &str) -> Vec<Range<usize>> {
        let mut line_ranges = vec![];
        let mut line_start = 0;
        let line_ends = source.char_indices().filter(|(_, c)| matches!(c, '\n'));

        for (at_newline, _) in line_ends {
            let line_end = at_newline + 1;
            line_ranges.push(line_start..line_end);
            line_start = line_end;
        }

        // Use a line span covering the rest of the file
        line_ranges.push(line_start..source.len() + 1);

        line_ranges
    }

    fn map_byte_index_to_position(
        &self,
        file: Option<toc_span::FileId>,
        byte_idx: usize,
    ) -> Option<Position> {
        let (info, line_ranges) = self.files.get(&file?)?;

        let (line, slice) = line_ranges
            .iter()
            .enumerate()
            .find(|(_line, range)| range.contains(&byte_idx))
            .map(|(line, range)| (line, range.clone()))
            .unwrap();

        let source_slice = &info.source[slice.start..byte_idx];

        // Get character count in UTF-16 chars
        let column_offset = source_slice.encode_utf16().count();

        Some(Position::new(line as u32, column_offset as u32))
    }

    fn map_span_to_location(&self, span: toc_span::Span) -> Location {
        let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

        let start = self
            .map_byte_index_to_position(span.file, start as usize)
            .unwrap();
        let end = self
            .map_byte_index_to_position(span.file, end as usize)
            .unwrap();

        let (info, _) = self.files.get(&span.file.unwrap()).unwrap();
        let path = Path::new(&info.path);

        Location::new(
            lsp_types::Url::from_file_path(path).unwrap(),
            lsp_types::Range::new(start, end),
        )
    }
}
