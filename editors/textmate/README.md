# Elara TextMate grammar

The grammar is hand-maintained but with some basic tooling to help us out.

- `grammar.ts` generates the TextMate grammar JSON file
  - `bun grammar.ts > elara.tmLanguage.json` writes the generated JSON file for anything that needs a real grammar (e.g. VSCode plugin)
- `Conformance.hs` (the `elara-grammar` test suite) generates arbitrary source strings and checks that the TextMate grammar processes
  every character roughly the same as the real lexer processes it
- `tokenise.ts` listens to `stdin` and uses `vscode-textmate` to actually run the TextMate grammar on some real input


**Run the tests before and after making any changes to the grammar or associated tooling:**
```sh
just test-grammar
```
