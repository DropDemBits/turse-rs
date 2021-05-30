import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // Server is an external binary
    let serverExe = context.asAbsolutePath(
        path.join('server', 'turing-lsp-server')
    );

    // Use the default launch options
    let serverOptions: ServerOptions = {
        run: { command: serverExe },
        debug: { command: serverExe },
    };

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for Turing code files
        documentSelector: [{ scheme: 'file', language: 'turing' }],
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'langServerTuring',
        'Turing Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
