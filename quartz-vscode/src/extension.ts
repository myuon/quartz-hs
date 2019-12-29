import * as vscode from "vscode";
import * as child_process from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(child_process.execFile);

const fullDocumentRange = (document: vscode.TextDocument): vscode.Range => {
  const last = document.lineCount - 1;
  return new vscode.Range(0, 0, last, document.lineAt(last).text.length);
};

const runQzfmt = async (
  command: string,
  input: { filepath?: string; content?: string }
): Promise<string> => {
  const runProcess = async () => {
    if (input.filepath) {
      return execFileAsync(command, [input.filepath]);
    } else if (input.content) {
      const process = execFileAsync(command, []);
      process.child.stdin?.write(input.content);
      process.child.stdin?.end();

      return process;
    } else {
      throw new Error("unreachable");
    }
  };

  const { stdout, stderr } = await runProcess();
  if (stderr.length > 0) {
    throw new Error(stderr.toString());
  }
  return stdout.toString();
};

export const activate = (context: vscode.ExtensionContext) => {
  const config = vscode.workspace.getConfiguration("quartz");
  const format = async (
    document: vscode.TextDocument
  ): Promise<vscode.TextEdit[]> => {
    const fullRange = fullDocumentRange(document);
    const result = await runQzfmt(
      config.fmtPath || "qzfmt",
      document.isDirty
        ? { content: document.getText() }
        : { filepath: document.uri.fsPath }
    );

    return [vscode.TextEdit.replace(fullRange, result)];
  };

  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider("quartz", {
      provideDocumentFormattingEdits: format
    }),
    vscode.languages.registerDocumentRangeFormattingEditProvider("quartz", {
      provideDocumentRangeFormattingEdits: format
    })
  );
};
