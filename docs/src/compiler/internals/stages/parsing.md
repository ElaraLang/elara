# Parsing

The parsing stage is responsible for taking the flat stream of tokens produced by the Lexer and constructing a Frontend Abstract Syntax Tree (AST). This AST is the compiler's closest representation to the original source syntax, deliberately preserving much of the syntax sugar that will be transformed or stripped away in later phases like [Desugaring](../stages/desugaring.md) and [Renaming](../stages/renaming.md).

## Core Mechanisms

The parser is built with [Megaparsec](https://hackage.haskell.org/package/megaparsec) parser combinators, over a custom `TokenStream` collecting output from the [lexer](../stages/lexing.md).

There's nothing really that interesting that's worth mentioning in the parser that can't be seen by just looking at the code, with the main exception being the knot-tying approach we take to Expression parsing:

Because the parsers for expressions, declarations, patterns, and types are inherently mutually recursive, they are tied together using a "grammar knot" pattern in `Parse.Grammar` (currently somewhat incomplete).
This originally came as a failed debugging attempt about a deadlock in the test suite, which we hypothesised was caused by a CAF deadlock with the mutually recursive parsers. While this doesn't seem to be the case, it's still a somewhat good practice and makes the mutual recursion more explicit and less likely to cause issues in the future.

## Error Handling

When the parser encounters invalid syntax, it halts and throws a custom `ElaraParseError`.
>[!NOTE]
>Unlike the rest of the [effects stack](../architecture/effects.md), this is integrated into the `ParsecT` monad 

These aim to provide more helpful errors than the standard Megaparsec errors, and include things like:

- `KeywordUsedAsName`: Attempting to bind a value or type to a reserved keyword.
- `InfixPrecTooHigh`: Defining an operator with a precedence outside the valid 0–9 range.
- `ModuleNameMismatch`: The file path does not correspond to the module declaration at the top of the file (see [Module System](../../../language/modules.md)).
- Nicer parse errors for common mistakes like `EmptyRecord` or `EmptyLambda`
- `InvalidConstantExpression`: Using a dynamic expression in a context that requires compile-time evaluation (see [Constant Expressions](../../../language/syntax/constant-expressions.md)).
