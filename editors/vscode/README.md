# Elara for VS Code

Provides syntax highlighting and diagnostics from `elara lsp`.

## Building

Requires **npm** _and_ **bun**

```sh
npm install
npm run check     # typecheck
npm run package   # build elara-lsp-<version>.vsix (needs bun)
code --install-extension elara-lsp-*.vsix
```
