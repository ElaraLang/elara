# Stages

The compiler is divided into a series of stages, each responsible for a specific part of the compilation process. Each stage (usually) takes input from the previous stage and produces output for the next stage. The inputs are fetched using the [Query System](./architecture/queries.md)

The stages are as follows:
1. **Lexing**: The source code is tokenized into a stream of tokens. This stage is implemented in [Lexing](./stages/lexing.md).