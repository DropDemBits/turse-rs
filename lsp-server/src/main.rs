use std::error::Error;
use std::ops::Range;
use std::sync::Arc;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, InitializeParams, Location,
    Position, PublishDiagnosticsParams, ServerCapabilities, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, VersionedTextDocumentIdentifier,
};
use toc_hir::db;
use toc_vfs::query::{FileSystem, VfsDatabaseExt};

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
        check_document(connection, uri, text)?;
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
    let mut db = LspDatabase::default();

    // Add the root path to the file db
    let root_file = db.vfs.intern_path(path.into());
    db.update_file(root_file, Some(contents.into()));

    let hir_db = db::HirBuilder::new();

    // Parse root CST
    let (parsed, dep_messages) = {
        let source = &db.file_source(root_file).0;
        let parsed = toc_parser::parse(Some(root_file), source);
        let (_dependencies, messages) =
            toc_driver::gather_dependencies(Some(root_file), parsed.syntax());

        (parsed, messages.finish())
    };

    eprintln!("finished parse @ {:?}", uri.as_str());

    let (validate_res, hir_res) = {
        let validate_res = toc_validate::validate_ast(Some(root_file), parsed.syntax());
        let hir_res = toc_hir_lowering::lower_ast(hir_db.clone(), Some(root_file), parsed.syntax());

        (validate_res, hir_res)
    };

    eprintln!("finished CST validate & lower @ {:?}", uri.as_str());

    let hir_db = hir_db.finish();
    let analyze_res = toc_analysis::analyze_unit(hir_db, hir_res.id);

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
            let range = db.map_span_to_location(msg.span()).range;
            let severity = to_diag_level(msg.kind());
            let annotations = msg
                .annotations()
                .iter()
                .map(|annotate| DiagnosticRelatedInformation {
                    location: db.map_span_to_location(annotate.span()),
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

#[salsa::query_group(SpanMappingStorage)]
trait SpanMapping: toc_vfs::query::FileSystem {
    fn line_ranges(&self, file_id: toc_span::FileId) -> Arc<Vec<Range<usize>>>;

    fn file_path(&self, file_id: toc_span::FileId) -> Arc<String>;

    fn map_byte_index(
        &self,
        file: toc_span::FileId,
        byte_idx: usize,
    ) -> Option<(usize, Range<usize>)>;

    fn map_byte_index_to_position(
        &self,
        file: toc_span::FileId,
        byte_idx: usize,
    ) -> Option<Position>;

    fn map_span_to_location(&self, span: toc_span::Span) -> Location;
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
    line_ranges.push(line_start..source.len() + 1);

    Arc::new(line_ranges)
}

fn file_path(db: &dyn SpanMapping, file_id: toc_span::FileId) -> Arc<String> {
    Arc::new(db.get_vfs().lookup_path(file_id).display().to_string())
}

fn map_byte_index(
    db: &dyn SpanMapping,
    file_id: toc_span::FileId,
    byte_idx: usize,
) -> Option<(usize, Range<usize>)> {
    db.line_ranges(file_id)
        .iter()
        .enumerate()
        .find(|(_line, range)| range.contains(&byte_idx))
        .map(|(line, range)| (line, range.clone()))
}

fn map_byte_index_to_position(
    db: &dyn SpanMapping,
    file_id: toc_span::FileId,
    byte_idx: usize,
) -> Option<Position> {
    let (line, slice) = db.map_byte_index(file_id, byte_idx).unwrap();

    let source_slice = &db.file_source(file_id).0[slice.start..byte_idx];

    // Get character count in UTF-16 chars
    let column_offset = source_slice.encode_utf16().count();

    Some(Position::new(line as u32, column_offset as u32))
}

fn map_span_to_location(db: &dyn SpanMapping, span: toc_span::Span) -> Location {
    let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

    let file = span.file.unwrap();

    let start = db.map_byte_index_to_position(file, start as usize).unwrap();
    let end = db.map_byte_index_to_position(file, end as usize).unwrap();

    let path = &db.file_path(file);

    Location::new(
        lsp_types::Url::from_file_path(path.as_str()).unwrap(),
        lsp_types::Range::new(start, end),
    )
}

#[salsa::database(toc_vfs::query::FileSystemStorage, SpanMappingStorage)]
#[derive(Default)]
struct LspDatabase {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for LspDatabase {}

impl toc_vfs::HasVfs for LspDatabase {
    fn get_vfs(&self) -> &toc_vfs::Vfs {
        &self.vfs
    }
}
