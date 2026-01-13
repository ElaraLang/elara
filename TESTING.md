# Test Coverage Improvements

This document summarizes the test coverage improvements made to the Elara compiler codebase.

## Overview

The Elara compiler previously had tests for core functionality (lexing, parsing, type inference, operator shunting, and closure lifting), but some complex algorithms and edge cases were not thoroughly tested. This PR adds focused test coverage for components with non-trivial behavior that benefits from automated testing.

## Newly Tested Modules

### Complex Data Structures and Algorithms

#### 1. `Elara.Data.TopologicalGraph` (test/TopologicalGraph.hs)
- Tests for graph creation with various dependency patterns
- Tests for topological traversal order verification
- Tests for handling cyclic and acyclic dependencies
- Tests for independent nodes
- Tests for graph transformations
- **Why this matters**: Topological sorting is complex and getting the order wrong can break compilation

#### 2. `Elara.Shunt.Operator` (test/ShuntOperator.hs)
- Tests for precedence validation (must be 0-9)
- Tests for error cases (negative precedence, precedence >= 10)
- Tests for `OpInfo` data type
- Tests for precedence and associativity ordering
- **Why this matters**: Error boundaries need explicit testing; the precedence bounds are business logic, not just type safety

## Test Statistics

- **New test modules**: 2
- **New test cases**: ~25
- **Test types**: Unit tests and property-based tests (using Hedgehog)
- **Testing framework**: sydtest + hedgehog (consistent with existing tests)

## Why Not Test Everything?

Many utility functions and simple data structures were considered but not tested because:

1. **Referential transparency**: Pure functions with simple, obvious implementations don't benefit from tests
2. **Type safety**: Haskell's type system already guarantees much of the behavior
3. **Trivial implementations**: Functions like `curry3` and `uncurry3` are so simple that tests would just restate the definition
4. **Maintenance burden**: Tests that don't catch real bugs add maintenance cost without value

### Examples of What We Don't Test

- **Simple utility functions** (`Elara.Utils.curry3`, `Elara.Utils.uncurry3`)
  - These are one-liners that are obvious from their types
  
- **Typeclass instances** (`Elara.Data.Unwrap`)
  - Testing `unwrap (Identity x) === x` is just testing the definition
  
- **Constants** (`Elara.Width.defaultWidth`)
  - A test that `defaultWidth == 80` adds no value
  
- **Trivial interpreters** (`Elara.ConstExpr`)
  - When the interpreter just returns what you pass in, tests are redundant

## Untested Components

The following components remain untested, primarily because they have complex dependencies on the Query system or require a full compilation pipeline:

### Components Requiring Query System Refactoring

1. **`Elara.SCC`** - Strongly connected component analysis
   - Depends on `Rock.Query` system
   - Would benefit from dependency injection or mock Query implementation

2. **`Elara.Desugar`** - Desugaring transformations
   - Heavy use of effectful Query operations
   - Requires mock module resolution

3. **`Elara.Rename`** - Variable and type renaming
   - Complex Query dependencies
   - Needs isolated test fixtures

4. **`Elara.ToCore`** - AST to Core IR conversion
   - Depends on type checking results
   - Requires full pipeline context

### Components Requiring Pipeline Refactoring

5. **`Elara.JVM.*`** - JVM bytecode generation
   - Requires complete Core IR input
   - Would benefit from golden tests with known bytecode outputs

6. **`Elara.Interpreter`** - Expression evaluation
   - Requires complete compilation context
   - Could use lightweight interpreter tests with Core IR fixtures

7. **`Elara.Pipeline`** - Compilation pipeline
   - Integration component
   - Best tested via end-to-end golden tests (already exists)

## Recommendations for Future Testing

### Short-term
1. Add more edge cases for topological graph algorithms
2. Add tests for operator precedence conflicts
3. Expand golden tests for end-to-end behavior

### Medium-term
1. Refactor `Elara.Query` to support dependency injection
2. Create test fixtures for common AST patterns
3. Add integration tests for pipeline stages

### Long-term
1. Implement mock Query system for testing complex modules
2. Consider property-based testing for type checker invariants
3. Set up continuous test coverage tracking (for test quality, not quantity)

## Testing Philosophy

We follow these principles:

1. **Test behavior, not implementation**: Focus on observable behavior and edge cases
2. **Test what can fail**: Don't test trivial functions or type-guaranteed properties
3. **Value over coverage**: 100% test coverage is not the goal; catching real bugs is
4. **Property-based where appropriate**: Use Hedgehog for testing invariants and algorithms
5. **Keep tests maintainable**: Tests should be easy to understand and update

## Build and Test

Tests can be run using:
```bash
cabal test
```

Or with the justfile:
```bash
just test
```

Note: The project uses Nix for reproducible builds. See `.github/workflows/ci.yaml` for CI configuration.
