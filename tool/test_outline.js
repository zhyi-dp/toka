const fs = require('fs');
const path = require('path');

// Mock vscode components
const vscode = {
    SymbolKind: {
        Function: 11,
        Struct: 22,
        Class: 4,
        Variable: 12
    }
};

class TokaDocumentSymbolProvider {
    provideDocumentSymbols(text) {
        return new Promise((resolve, reject) => {
            const symbols = [];
            const lines = text.split('\n');

            // Regex patterns from extension.js
            const fnRegex = /^\s*(?:pub\s+)?fn\s+([a-zA-Z_]\w*)/;
            const shapeRegex = /^\s*(?:pub\s+)?shape\s+([a-zA-Z_]\w*)/;
            const implRegex = /^\s*impl\s+(?:([a-zA-Z_]\w*)\s+for\s+)?([a-zA-Z_]\w*)/;
            const aliasRegex = /^\s*(?:pub\s+)?alias\s+([a-zA-Z_]\w*)/;

            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];

                let match = fnRegex.exec(line);
                if (match) {
                    this.addSymbol(symbols, match[1], vscode.SymbolKind.Function, i, line, match[0]);
                    continue;
                }

                match = shapeRegex.exec(line);
                if (match) {
                    this.addSymbol(symbols, match[1], vscode.SymbolKind.Struct, i, line, match[0]);
                    continue;
                }

                match = implRegex.exec(line);
                if (match) {
                    const name = match[1] ? `${match[1]} for ${match[2]}` : match[2];
                    this.addSymbol(symbols, name, vscode.SymbolKind.Class, i, line, match[0]);
                    continue;
                }

                match = aliasRegex.exec(line);
                if (match) {
                    this.addSymbol(symbols, match[1], vscode.SymbolKind.Variable, i, line, match[0]);
                    continue;
                }
            }

            resolve(symbols);
        });
    }

    addSymbol(symbols, name, kind, lineIndex, lineText, matchText) {
        symbols.push({
            name,
            kind,
            line: lineIndex + 1, // 1-based for display
            matchText: matchText.trim()
        });
    }
}

// Read fs.tk
const filePath = 'tests/pass/string_pythonic.tk'; // Default fallback
const targetFile = process.argv[2] || 'lib/std/fs.tk';

try {
    const content = fs.readFileSync(targetFile, 'utf8');
    const provider = new TokaDocumentSymbolProvider();
    provider.provideDocumentSymbols(content).then(symbols => {
        console.log(`Found ${symbols.length} symbols in ${targetFile}:`);
        symbols.forEach(s => console.log(`[Line ${s.line}] ${s.name} (${s.kind})`));
        if (symbols.length === 0) {
            console.log("No symbols found!");
        }
    }).catch(err => {
        console.error("Error providing symbols:", err);
    });
} catch (err) {
    console.error(`Error reading file ${targetFile}:`, err);
}
