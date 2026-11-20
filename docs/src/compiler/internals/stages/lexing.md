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

#### Positions of Layout Tokens

`TokenDedent` (`<DEDENT>`) and `TokenLineSeparator` (`<LINESEP>`) don't correspond to anything written in the source, so they have to be given a location somehow. We mark them with an empty (zero-width) region at the end of the last "real" token before them.

The lexer tracks this with `prevTokenEnd` in [`ParseState`]({{hackage_base}}/Elara-Lexer-Utils.html#t:ParseState), which is updated by `recordTokenEnd` whenever a real token is emitted.

Because of this, errors at a layout break point to the end of the line that was left unfinished.

`TokenIndent` (`<INDENT>`) doesn't follow this rule. It's emitted right before the first real token of the block, so it's located at the start of that token.

##### Dedents before a closing delimiter

Most layout tokens are produced while we read the whitespace at the start of a new line (but their location is adjusted to match the location of the previous real token as described above). 


When delimiters (e.g. parentheses) are used, we treat dedents before a closing delimiter a bit differently since there's no newline involved:

```elara
f (\x ->
    x)
```

This lexes to `f ( \ x -> <INDENT> x <DEDENT> )`. The block opened by `->` is still open when we reach the `)`, so the `)` is what closes it (see [Explicit Scoping](#explicit-scoping)).

The dedent has to end where `x` ends, not where `)` ends, since it comes _before_ the `)` and belongs to the block that `)` is closing. To get this right, `emitCloser` has to do things in a specific order:

1. Flush the open blocks, emitting the dedents. These read `prevTokenEnd`, which is still the end of `x`
2. Update `prevTokenEnd` with the end of the `)`

Swapping these would put the dedent after the `)`, and anything ending at that dedent would wrongly include the closing delimiter.

#### End of Input

The lexer doesn't close layout blocks that are still open at the end of the input, and it won't emit any `TokenDedent` / `TokenLineSeparator` tokens at the very end of the stream.

This is purely for better error messages. If we _did_ try to emit layout tokens at an EOF, the amount of layout tokens we'd emit depends on how the file happens to end (trailing newlines, blank lines, comments, etc), which tells us nothing useful and 
In most cases the real problem is just that the file ended somewhere unexpected, so we want the error to say that. 

Instead, the parser treats the end of input as closing any open indented block (see `blockExpr` in `Elara.Parse.Indents`).

Consider these two error examples to demonstrate the value of this approach:

**Program**:
```elara
-- preamble truncated for clarity
let main = [1, 2, 3
```

**Before**:
Emitting layout tokens at the EOF pushes the error forward, making the diagnostic
point to irrelevant code.
```
[error E0005]: error-unclosed-list.elr:5:1:
   5 |
     | ^
   unexpected end of line
   expecting ']' to close list, expression, or operator
```

**After**:
Now the diagnostic points to the actually relevant part of the code
```
[error E0005]: error-unclosed-list.elr:4:20:
   4 | let main = [1, 2, 3
     |                    ^
   unexpected end of input
   expecting ']' to close list, expression, or operator
```

#### Explicit Scoping

Layout rules are scoped by explicit delimiters (`()`, `[]`, and `{}`). In other words, any indentation block opened inside a set of delimiters is forcefully flushed and dedented (`flushLayoutBeforeCloser`) immediately before the closing token is emitted.
This prevents layout blocks from accidentally spanning across explicit scopes, which can cause unpredictable behaviour.

### Error Handling

When the lexer encounters invalid syntax or malformed layout, it immediately halts and produces an error:

- `TooMuchIndentation`: Raised when a line's indentation decreases but fails to align with any currently open block on the stack
- `UnterminatedStringLiteral`: Fairly self-explanatory, raised when the lexer reaches the end of the file while still inside an open string literal
- `GenericAlexError`: Raised when the underlying Alex scanner encounters an unrecognised character or invalid sequence. We are slowly trying to make this more user-friendly.