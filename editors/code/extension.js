const vscode = require('vscode');

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    console.log('Toka extension is now active!');

    context.subscriptions.push(
        vscode.languages.registerDocumentSymbolProvider(
            { scheme: 'file', language: 'toka' },
            new TokaDocumentSymbolProvider()
        )
    );
}

class TokaDocumentSymbolProvider {
    provideDocumentSymbols(document, token) {
        return new Promise((resolve, reject) => {
            const symbols = [];

            // Regex patterns
            const fnRegex = /^\s*(?:pub\s+)?fn\s+([a-zA-Z_]\w*)/;
            const shapeRegex = /^\s*(?:pub\s+)?shape\s+([a-zA-Z_]\w*)/;
            const implRegex = /^\s*impl\s+(?:([a-zA-Z_]\w*)\s+for\s+)?([a-zA-Z_]\w*)/;
            const aliasRegex = /^\s*(?:pub\s+)?alias\s+([a-zA-Z_]\w*)/;

            for (let i = 0; i < document.lineCount; i++) {
                const line = document.lineAt(i);
                const text = line.text;

                if (text.trim() === '') continue;

                let match = fnRegex.exec(text);
                if (match) {
                    this.addSymbol(symbols, match[1], vscode.SymbolKind.Function, i, text, match[0]);
                    continue;
                }

                match = shapeRegex.exec(text);
                if (match) {
                    this.addSymbol(symbols, match[1], vscode.SymbolKind.Struct, i, text, match[0]);
                    continue;
                }

                match = implRegex.exec(text);
                if (match) {
                    const name = match[1] ? `${match[1]} for ${match[2]}` : match[2];
                    this.addSymbol(symbols, name, vscode.SymbolKind.Class, i, text, match[0]);
                    continue;
                }

                match = aliasRegex.exec(text);
                if (match) {
                    this.addSymbol(symbols, match[1], vscode.SymbolKind.Variable, i, text, match[0]);
                    continue;
                }
            }

            resolve(symbols);
        });
    }

    addSymbol(symbols, name, kind, lineIndex, lineText, matchText) {
        const range = new vscode.Range(lineIndex, 0, lineIndex, lineText.length);
        const selectionRange = new vscode.Range(lineIndex, 0, lineIndex, lineText.length);

        const symbol = new vscode.DocumentSymbol(
            name,
            matchText.trim(),
            kind,
            range,
            selectionRange
        );

        symbols.push(symbol);
    }
}

function deactivate() { }

module.exports = {
    activate,
    deactivate
};
