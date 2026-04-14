# Effects

The compiler uses the [Effectful](https://hackage.haskell.org/package/effectful) library to manage side effects in a structured, composable way.

## Overview

- All compiler operations are expressed as `Eff es a` computations with explicit effect constraints
- The full effect stack is assembled in `Elara.withCompilerEnv` and `app/Main.hs`
- Effects use membership constraints (e.g. `FileSystem :> es`) rather than concrete stacks, so functions only declare the effects they need
- Reusable type aliases like `ConsQueryEffects` and `QueryEffects` bundle common effect sets

> [!NOTE]
> While technically orthogonal, the effect system is closely tied to the [query engine](../architecture/queries.md), with each query having access to a specific set of effects.

## Effect Stack

We have a few different sets of key effects:

[**Minimum**]({{hackage_base}}//Elara-Query-Effects.html#t:MinimumQueryEffects): the only effects that every query _must_ have available
- `StructuredDebug` - for debug logging in queries
- `Rock` - the ability to make other queries

[**Standard**]({{hackage_base}}//Elara-Query-Effects.html#t:StandardQueryEffects): effects that almost every query will need, but some pure ones can avoid
- All the minimum effects, plus:
- `FileSystem` - for file IO. This is a mainly due to transitivity, as almost all queries will ultimately depend on a query that needs to read source files. _TODO: we should try to abstract this away so that pure queries don't get this constraint / extra power_
- `Error SomeReportableError` - the ability to raise reportable errors
- `DiagnosticWriter (Doc AnsiStyle)` - the ability to write diagnostics (errors or warnings) for error reporting
- `UniqueGen` - the ability to generate fresh unique numbers

Other queries may require additional effects, which they can add to their constraints as needed. 

## Individual Effects

### Error

This is just the standard `Effectful.Error.Static` effect, but we use an existential wrapper `SomeReportableError` to allow any type of error that implements our `ReportableError` class.

### DiagnosticWriter

Defined in [`src/Elara/Error/Effect.hs`]({{repo_base}/src/Elara/Error/Effect.hs), which wraps the functionality of the `diagnose` library. This is essentially a specialised writer effect for `Diagnostic` values that also adds a few helpers for adding file contexts, etc.


### UniqueGen

Defined in [`src/Elara/Data/Unique/Effect.hs`]({{repo_base}/src/Elara/Data/Unique/Effect.hs). It provides a simple interface for generating fresh unique numbers, which can be used as unique identifiers.

It has a few interpretations:
- `uniqueGenToGlobalIO` - a global counter in IO, used in the main compiler environment
- `uniqueGenToState` - reinterpret as a pure state effect

### StructuredDebug

Defined in [`src/Elara/Logging.hs`]({{repo_base}/src/Elara/Logging.hs)

Provides a structured logging interface for logging messages of all levels (not just debug), with namespaces and hierarchical contexts. 

Interpretations:
- `structuredDebugToLogWith` - logs to `co-log` sink
- `ignoreStructuredDebug` - discards all output

### Rock (Query)

Defined in [`src/Rock/Memo.hs`]({{repo_base}/src/Rock/Memo.hs)

This is an improved version of the [Rock](https://hackage.haskell.org/package/rock) query library,
ported to use `Effectful` by [`expipiplus1`](https://github.com/ollef/rock/issues/12#issuecomment-2336637410) on GitHub.

Our additions include improving the memoisation to make it more behind the scenes rather than needing `IOE` in every query, and adding the ability to trace query dependencies for debugging and visualisation.

See the [Queries Documentation](../architecture/queries.md) for more details on how we use this in the compiler.

