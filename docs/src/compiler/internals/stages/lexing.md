# Lexing

The Lexing stage is the first major stage of the compiler. It takes raw source code as input and produces a stream of tokens as output.

## Overview

The lexer is built with the [Alex](https://haskell-alex.readthedocs.io/) lexer generator, and is responsible for turning source code text into a stream of `Lexeme` values.

A `Lexeme` encapsulates a `Token` (the kind of token, e.g., `TokenInt`, `TokenVariableIdentifier`, etc.) along with a `SourceRegion` that indicates where in the source code the token was found

The lexer also importantly handles indentation / layout processing by inserting synthetic structural tokens

## Implementation

### Tokenisation

The Alex lexer generator handles most of this. We process the source code character by character, processing it into well-defined tokens. This includes parsing number literals, string literals, handling comments, and operator symbols.

### Layout Processing

Because Elara supports lightweight, indentation-sensitive syntax, the lexer cannot just output what it reads, it must also synthesise structural tokens to represent the layout of the code. 
These are the `TokenIndent`, `TokenDedent`, and `TokenLineSeparator` tokens, which represent the start of an indented block, the end of an indented block, and a separator between statements in a layout block respectively.

This is the most complex part of the lexer by far and requires a lot of state management in the [`ParseState`]({{hackage_base}}/Elara-Lexer-Utils.html#t:ParseState) type.

Certain tokens such as `if`, `then`, `else`, `with`, `->`, etc trigger a `LayoutExpectation`
This signals to the lexer that the next line should be evaluated for a new indentation block.
> [!NOTE]
> This doesn't include keywords like `let` and `match`

The lexer then maintains an `indentStack` tracking the column depth of nested blocks. When it encounters a new line, it compares the new line's indentation against the top of the stack:

- If the new line is _more_ indented, the lexer pushes a new level onto the stack and emits a `TokenIndent`. If no `LayoutExpectation` was active, the increased indentation is simply ignored as a line continuation.
- If the new line is _less_ indented, the lexer pops levels from the stack until the current indentation aligns with a previous block, emitting a `TokenDedent` for each popped level. If it perfectly matches an existing level, it also emits a `TokenLineSeparator` to begin the next statement
- If the new line is _equal_, the lexer emits a `TokenLineSeparator` if and only if the previous token is capable of ending an expression (`tokenEndsExpr`). This prevents line breaks in normal expressions from being treated as statement separators when they shouldn't be

#### Explicit Scoping

Layout rules are scoped by explicit delimiters (`()`, `[]`, and `{}`). In other words, any indentation block opened inside a set of delimiters is forcefully flushed and dedented (`flushLayoutBeforeCloser`) immediately before the closing token is emitted.
This prevents layout blocks from accidentally spanning across explicit scopes, which can cause unpredictable behaviour.

### Error Handling

When the lexer encounters invalid syntax or malformed layout, it immediately halts and produces an error:

- `TooMuchIndentation`: Raised when a line's indentation decreases but fails to align with any currently open block on the stack
- `UnterminatedStringLiteral`: Fairly self-explanatory, raised when the lexer reaches the end of the file while still inside an open string literal
- `GenericAlexError`: Raised when the underlying Alex scanner encounters an unrecognised character or invalid sequence. We are slowly trying to make this more user-friendly.