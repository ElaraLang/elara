# Summary

# Elara Documentation

- [Introduction](./compiler/introduction.md)

# Language Reference

- [Syntax](./language/syntax.md)
  - [Lexical Structure](./language/syntax/lexical-structure.md)
    - [Layout](./language/syntax/lexical-structure/layout.md)
  - [Annotations](./language/syntax/annotations.md)
  - [Constant Expressions](./language/syntax/constant-expressions.md)
  - [Comments](./language/syntax/comments.md)
- [Modules](./language/modules.md)

# Compiler Usage

- [CLI](./compiler/usage/cli.md)

# Compiler Internals

- [Setup](./compiler/internals/setup.md)
- [Compiler Architecture](./compiler/internals/architecture.md)

  - [Effects](./compiler/internals/architecture/effects.md)
  - [Queries](./compiler/internals/architecture/queries.md)
  - [Errors](./compiler/internals/architecture/errors.md)
  - [Primitives](./compiler/internals/architecture/primitives.md)

- [Stages](./compiler/internals/stages.md)
  - [Lexing](./compiler/internals/stages/lexing.md)
  - [Parsing](./compiler/internals/stages/parsing.md)
  - [Desugaring](./compiler/internals/stages/desugaring.md)
  - [Renaming](./compiler/internals/stages/renaming.md)
  - [Shunting](./compiler/internals/stages/shunting.md)
  - [Type Inference](./compiler/internals/stages/type-inference.md)
  - [ToCore](./compiler/internals/stages/to-core.md)
  - [Core-to-Core](./compiler/internals/stages/core-to-core.md)
  - [JVM Backend](./compiler/internals/stages/jvm-backend.md)
