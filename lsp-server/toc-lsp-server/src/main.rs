use std::sync::Arc;

use lsp_types::{
    notification::PublishDiagnostics, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, InitializeParams, InitializeResult, InitializedParams,
    PublishDiagnosticsParams, ServerCapabilities, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, VersionedTextDocumentIdentifier,
};
use toc_ide_db::ServerState;
use tokio::{sync::Mutex, task::block_in_place};
use tower_lsp::{jsonrpc, Client, LanguageServer, LspService, Server};
use tracing::{debug, info, trace};
use tracing_subscriber::EnvFilter;

struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: Default::default(),
        }
    }

    async fn check_document(&self, url: &Url) {
        debug!("starting analysis @ {:?}", url.as_str());

        // Collect diagnostics
        let diagnostics = block_in_place(|| {
            let state = self.state.blocking_lock();
            state.collect_diagnostics()
        });

        debug!("finished analysis @ {:?}", url.as_str());
        trace!("gathered diagnostics: {diagnostics:#?}");

        for (path, bundle) in diagnostics {
            let uri =
                lsp_types::Url::from_file_path(path).expect("path wasn't absolute or a valid url");

            self.client
                .send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                    uri,
                    diagnostics: bundle,
                    version: None,
                })
                .await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {}

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        info!("Shutting down LSP server");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let TextDocumentItem {
            text, uri, version, ..
        } = params.text_document;

        // update file store representation first
        {
            debug!("open, updating file store @ {:?}", uri.as_str());
            let mut state = self.state.lock().await;
            state.open_file(&uri, version, text);
        }

        // pub diagnostics
        // TODO: use db snapshot
        debug!("post-open, sending diagnostics @ {:?}", uri.as_str());
        self.check_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let VersionedTextDocumentIdentifier { uri, version } = params.text_document;

        {
            let mut state = self.state.lock().await;
            state.apply_changes(&uri, version, params.content_changes);
        }

        // pub diagnostics
        debug!("change, sending diagnostics @ {:?}", uri.as_str());
        self.check_document(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let TextDocumentIdentifier { uri } = params.text_document;

        {
            debug!("open, updating file store @ {:?}", uri.as_str());
            let mut state = self.state.lock().await;
            state.close_file(&uri);
        }
    }
}

#[tokio::main]
async fn main() {
    // logging on stderr
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("failed to setup global logger");

    info!("Starting LSP server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(Backend::new).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
