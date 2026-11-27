// Listens to stdin and tokenises any input strings with `vscode-textmate` and the grammar generated in `./grammar.ts`,
// outputting a JSON string containing the TextMate lexing output, formatted as a nested list of :
//  - 1 list per input line
//    - for each "logical" line (i.e. an `\n`)
//      - a "tuple" of lexed tokens [startIndex, endIndex, [scopes]], with `startIndex` _always_ starting from 0 (i.e. each line is independent)
// The input must be encoded as a JSON string rather than a raw string to properly handle escapes
// For compatibilty we output the indexes in terms of UTF-8/codepoints, despite vscode-textmate treating them as UTF-16
import { INITIAL, Registry, parseRawGrammar, type StateStack } from 'vscode-textmate';
import { OnigScanner, OnigString, loadWASM } from 'vscode-oniguruma';
import {grammarJson} from './grammar.ts';

await loadWASM(await Bun.file(Bun.resolveSync('vscode-oniguruma/release/onig.wasm', import.meta.dir)).arrayBuffer());

const registry = new Registry({
  onigLib: Promise.resolve({
    createOnigScanner: (patterns) => new OnigScanner(patterns),
    createOnigString: (s) => new OnigString(s),
  }),
  loadGrammar: async () => parseRawGrammar(grammarJson, 'elara.tmLanguage.json'),
});
const grammar = await registry.loadGrammar('source.elara')


function tokenizeFile(text: string): [number, number, string[]][][] {
  if (!grammar) throw new Error('grammar.ts has no source.elara grammar');

  let stack: StateStack = INITIAL;
  return text.split('\n').map((line) => {
    const result = grammar.tokenizeLine(line, stack);
    stack = result.ruleStack;
    // convert utf-16 to code points
    const codePoints = (i: number) => Array.from(line.slice(0, i)).length;
    return result.tokens.map((t) => [codePoints(t.startIndex), codePoints(t.endIndex), t.scopes]);
  });
}

for await (const line of console) {
  if (line) {
    process.stdout.write(JSON.stringify(tokenizeFile(JSON.parse(line))) + '\n');
  }
}
