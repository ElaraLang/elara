# Stages

The compiler is divided into a series of stages, each responsible for a specific part of the compilation process. Each stage (usually) takes input from the previous stage and produces output for the next stage. The inputs are fetched using the [Query System](./architecture/queries.md)

The stages are as follows:

1. **[Lexing](./stages/lexing.md)**: The source code is tokenized into a stream of tokens.
2. **[Parsing](./stages/parsing.md)**: The token stream is parsed into a Frontend AST.
3. **[Desugaring](./stages/desugaring.md)**: High-level syntax is simplified into a somewhat more uniform Desugared AST. 
4. **[Renaming](./stages/renaming.md)**: Names are resolved and qualified, some syntax sugar replaced by explicit primitive references.
5. **[Shunting](./stages/shunting.md)**: Operator precedence and associativity are resolved.
6. **[Type Inference](./stages/type-inference.md)**: Types are inferred and checked; kind inference is performed.
7. **[ToCore](./stages/to-core.md)**: The typed AST is lowered to the Core intermediate representation.
8. **[Core-to-Core](./stages/core-to-core.md)**: A series of Core transformation passes: optimisation, ANF conversion, closure lifting, and a second type checking pass.
9. **[JVM Backend](./stages/jvm-backend.md)**: Finalised Core is lowered to JVM IR, then emitted as class files.