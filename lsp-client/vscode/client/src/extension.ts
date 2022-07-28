import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const logLevel = workspace.getConfiguration('turing.logging').get('level') ?? 'info';
    console.log(logLevel);
    
    // Server is an external binary
    let serverExe = context.asAbsolutePath(
        path.join('server', 'turing-lsp-server')
    );

    // Use the default launch options
    let serverExec: Executable = {
        command: serverExe ,
        options: {
            env: {
                ... process.env,
                // Literal name of the env var
                // eslint-disable-next-line @typescript-eslint/naming-convention
                'RUST_LOG': `${logLevel}`,
            }
        }
    };
    let serverOptions: ServerOptions = {
        debug: serverExec,
        run: serverExec
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
