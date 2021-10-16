use std::error::Error;
use std::sync::Arc;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
};
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, InitializeParams, Location,
    Position, PublishDiagnosticsParams, ServerCapabilities, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind,
    VersionedTextDocumentIdentifier,
};
use toc_analysis::db::HirAnalysis;
use toc_ast_db::db::{AstDatabaseExt, SourceParser, SpanMapping};
use toc_ast_db::SourceGraph;
use toc_salsa::salsa;
use toc_vfs::LoadStatus;
use toc_vfs_db::db::VfsDatabaseExt;

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
    let mut state = ServerState::default();
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("listening for messages");

    for msg in &connection.receiver {
        eprintln!("recv {:?}", msg);

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(&mut state, connection, req)?;
            }
            Message::Response(_resp) => {}
            Message::Notification(notify) => handle_notify(&mut state, connection, notify)?,
        }
    }

    Ok(())
}

fn handle_request(
    _state: &mut ServerState,
    _connection: &Connection,
    req: Request,
) -> Result<(), DynError> {
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

fn handle_notify(
    state: &mut ServerState,
    connection: &Connection,
    notify: Notification,
) -> Result<(), DynError> {
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
        let TextDocumentItem {
            text, uri, version, ..
        } = params.text_document;

        // update file store representation first
        eprintln!("open, updating file store @ {:?}", uri.as_str());
        state.open_file(&uri, version, text);

        // pub diagnostics
        eprintln!("post-open, sending diagnostics @ {:?}", uri.as_str());
        check_document(state, connection, uri)?;
    } else if let Some(mut params) = cast::<DidChangeTextDocument>(&mut notify) {
        let VersionedTextDocumentIdentifier { uri, .. } = params.text_document;

        // update contents first
        let text = params.content_changes.pop().unwrap().text;
        state.update_file(&uri, text);

        // pub diagnostics
        eprintln!("change, sending diagnostics @ {:?}", uri.as_str());
        check_document(state, connection, uri)?;
    } else if let Some(params) = cast::<DidCloseTextDocument>(&mut notify) {
        let TextDocumentIdentifier { uri } = params.text_document;

        eprintln!("closing, updating file store");
        state.close_file(&uri);
    }

    Ok(())
}

fn check_document(
    state: &mut ServerState,
    connection: &Connection,
    uri: lsp_types::Url,
) -> Result<(), DynError> {
    use lsp_types::notification::Notification as NotificationTrait;
    // Collect diagnostics
    let diagnostics = state.collect_diagnostics();
    eprintln!("finished analysis @ {:?}", uri.as_str());

    connection
        .sender
        .send(Message::Notification(Notification::new(
            PublishDiagnostics::METHOD.into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            },
        )))?;

    Ok(())
}

#[derive(Default)]
struct ServerState {
    db: LspDatabase,
}

impl ServerState {
    fn open_file(&mut self, uri: &lsp_types::Url, _version: i32, text: String) {
        // This is where we update the source graph, as well as the file sources, and the file store
        self.update_file(uri, text);
    }

    fn close_file(&mut self, _uri: &lsp_types::Url) {
        // This is where we remove files from the source graph (if applicable / non-root) and from the file store
        //todo!()
    }

    fn update_file(&mut self, uri: &lsp_types::Url, contents: String) {
        let path = uri.path();
        let db = &mut self.db;

        // Add the root path to the file db
        let root_file = db.vfs.intern_path(path.into());
        db.update_file(root_file, Ok(LoadStatus::Modified(contents.into())));
        // Setup source graph
        let mut source_graph = SourceGraph::default();
        source_graph.add_root(root_file);
        db.set_source_graph(Arc::new(source_graph));

        // TODO: Recursively load in files, respecting already loaded files
        db.invalidate_source_graph(&toc_vfs::DummyFileLoader);
    }

    /// Collect diagnostics for all libraries
    fn collect_diagnostics(&self) -> Vec<Diagnostic> {
        let analyze_res = self.db.analyze_libraries();
        let msgs = analyze_res.messages();

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
                let range = self.map_span_to_location(msg.span()).range;
                let severity = to_diag_level(msg.kind());
                let annotations = msg
                    .annotations()
                    .iter()
                    .map(|annotate| DiagnosticRelatedInformation {
                        location: self.map_span_to_location(annotate.span()),
                        message: annotate.message().to_string(),
                    })
                    .collect();
                let message = if !msg.footer().is_empty() {
                    // Push all footer infos into the main message
                    let mut message = msg.message().to_string();

                    for annotate in msg.footer() {
                        message.push('\n');
                        message.push_str(annotate.message());
                    }

                    message
                } else {
                    msg.message().to_string()
                };

                Diagnostic::new(
                    range,
                    Some(severity),
                    None,
                    None,
                    message,
                    Some(annotations),
                    None,
                )
            })
            .collect()
    }

    fn map_span_to_location(&self, span: toc_span::Span) -> Location {
        let db = &self.db;
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
}

trait IntoPosition {
    fn into_position(self) -> Position;
}

impl IntoPosition for toc_ast_db::span::LspPosition {
    fn into_position(self) -> Position {
        Position::new(self.line, self.column)
    }
}

#[salsa::database(
    toc_vfs_db::db::FileSystemStorage,
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
struct LspDatabase {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for LspDatabase {}

toc_vfs::impl_has_vfs!(LspDatabase, vfs);
