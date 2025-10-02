# Compiler Architecture

This document provides a high-level overview of the architecture of the Elara compiler. It is intended for those who wish to understand the internal workings of the compiler, contribute to its development, or extend its functionality.

## Design Principles

**Query-Based**: The compiler is designed around the [Rock](https://github.com/ollef/rock) query system between compilation stages, enabling memoisation and incremental compilation (not implemented yet). Each compilation stage is implemented as a query or group of queries that can fetch results from other stages.

**Effectful**: The the [Effectful](https://hackage.haskell.org/package/effectful) library is used to manage side effects in a structured way. For more information on how effects are used in the compiler, see the [Effects](./architecture/effects.md) section.

**Trees that Grow**: The compiler adopts the [Trees that Grow](https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/trees-that-grow.pdf) pattern to avoid unnecessary AST duplication between compilation stages. Every [Stage](./stages.md) until [ToCore](./stages/to-core.md) uses the same AST type with different extensions to represent the information available at that stage.
