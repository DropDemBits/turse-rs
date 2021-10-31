use std::error::Error;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
};
use lsp_types::{
    InitializeParams, PublishDiagnosticsParams, ServerCapabilities, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind,
    VersionedTextDocumentIdentifier,
};

mod state;

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
    let mut state = state::ServerState::default();
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
    _state: &mut state::ServerState,
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
    state: &mut state::ServerState,
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
    } else if let Some(params) = cast::<DidChangeTextDocument>(&mut notify) {
        let VersionedTextDocumentIdentifier { uri, version } = params.text_document;

        state.apply_changes(&uri, version, params.content_changes);

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
    state: &mut state::ServerState,
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
