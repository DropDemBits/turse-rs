use std::error::Error;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, InitializeParams, Location,
    Position, PublishDiagnosticsParams, ServerCapabilities, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, VersionedTextDocumentIdentifier,
};
use toc_common_db::source::SourceParser;
use toc_common_db::span::SpanMapping;
use toc_hir::db;
use toc_salsa::salsa;
use toc_vfs::query::VfsDatabaseExt;

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
        let parsed = db.parse_file(root_file);
        let (_dependencies, messages) =
            toc_driver::gather_dependencies(Some(root_file), parsed.result().syntax());

        (parsed, messages.finish())
    };

    eprintln!("finished parse @ {:?}", uri.as_str());

    let (validate_res, hir_res) = {
        let validate_res = db.validate_file(root_file);
        let hir_res =
            toc_hir_lowering::lower_ast(hir_db.clone(), Some(root_file), parsed.result().syntax());

        (validate_res, hir_res)
    };

    eprintln!("finished CST validate & lower @ {:?}", uri.as_str());

    let hir_db = hir_db.finish();
    let analyze_res = toc_analysis::analyze_unit(hir_db, *hir_res.result());

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
            let range = map_span_to_location(&db, msg.span()).range;
            let severity = to_diag_level(msg.kind());
            let annotations = msg
                .annotations()
                .iter()
                .map(|annotate| DiagnosticRelatedInformation {
                    location: map_span_to_location(&db, annotate.span()),
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

fn map_span_to_location(db: &LspDatabase, span: toc_span::Span) -> Location {
    let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

    let file = span.file.unwrap();

    let start = db.map_byte_index_to_position(file, start as usize).unwrap();
    let end = db.map_byte_index_to_position(file, end as usize).unwrap();

    let path = &db.file_path(file);

    Location::new(
        lsp_types::Url::from_file_path(path.as_str()).unwrap(),
        lsp_types::Range::new(start.into_position(), end.into_position()),
    )
}

trait IntoPosition {
    fn into_position(self) -> Position;
}

impl IntoPosition for toc_common_db::span::LspPosition {
    fn into_position(self) -> Position {
        Position::new(self.line, self.column)
    }
}

#[salsa::database(
    toc_vfs::query::FileSystemStorage,
    toc_common_db::span::SpanMappingStorage,
    toc_common_db::source::SourceParserStorage
)]
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
