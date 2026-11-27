import { type ExtensionContext, type LogOutputChannel, commands, window, workspace } from 'vscode';
import { type Executable, LanguageClient, State } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let log: LogOutputChannel;

async function startClient(): Promise<void> {
  const config = workspace.getConfiguration('elara');

  const server: Executable = {
    command: config.get<string>('serverPath', 'elara'),
    args: config.get<string[]>('serverArgs', ['lsp', '--stdio']),
  };

  log.info(`Starting the language server: ${server.command} ${server.args?.join(' ')}`);

  client = new LanguageClient('elara', 'Elara', server,
    {
      documentSelector: [{ language: 'elara' }],
      outputChannel: log
    });

  client.onDidChangeState(({ newState }) =>
    log.info(`Language server ${State[newState].toLowerCase()}`));
  
  try {
    await client.start();
  } catch (error) {
    log.error(`The language server failed to start: ${error}`);
    const choice = await window.showErrorMessage(
      `Elara: couldn't start the language server (${server.command}). Is elara installed, or elara.serverPath set?`,
      'Show Log',
    );
    if (choice) log.show();
  }
}

async function restartClient(): Promise<void> {
  log.info('Restarting the language server');
  const old = client;
  client = undefined;
  if (old && old.isRunning()) {
    await old.stop();
  }
  await startClient();
}

export function activate(context: ExtensionContext): Promise<void> {
  log = window.createOutputChannel('Elara', { log: true });
  context.subscriptions.push(
    log,
    workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration('elara.serverPath') || event.affectsConfiguration('elara.serverArgs')) {
        log.info('Server settings changed');
        void restartClient();
      }
    }),
    commands.registerCommand('elara.restartServer', restartClient),
    commands.registerCommand('elara.showLog', () => log.show()),
  );
  return startClient();
}

export function deactivate(): Promise<void> | undefined {
  return client?.stop();
}
