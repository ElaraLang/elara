# Elara Compiler - AI Coding Instructions

## Project Overview

Elara is a **purely functional programming language** targeting the JVM, written in Haskell. It features Haskell/F#-inspired syntax, Hindley-Milner type inference, and IO monads. The compiler transforms source code through multiple AST stages, eventually emitting JVM bytecode.

## Architecture: Multi-Stage Compilation Pipeline

The compiler uses a **query-based architecture** (inspired by Rock/Salsa) with lazy memoized queries:

```
Frontend → Desugared → Renamed → Shunted → Typed → Core → ANF → ClosureLifted → JVM IR → Bytecode
```

### Key Stages ([src/Elara/AST/Select.hs](src/Elara/AST/Select.hs))
- **Frontend**: Raw parsed AST with all syntax sugar
- **Desugared**: Simplified AST (e.g., `where` → `let`)
- **Renamed**: Names resolved to qualified references
- **Shunted**: Operator precedence resolved via shunting-yard
- **Typed**: Hindley-Milner type inference complete
- **Core**: Simplified lambda calculus IR (see [src/Elara/Core.hs](src/Elara/Core.hs))
- **ANF**: A-Normal Form for easier code generation
- **JVM IR**: Abstract JVM instructions before bytecode emission

## Query System (Rock)

Queries are the core mechanism for inter-stage communication. Defined in [src/Elara/Query.hs](src/Elara/Query.hs), implemented in [src/Elara/Rules.hs](src/Elara/Rules.hs).

```haskell
-- Fetching triggers all dependent stages automatically
coreModule <- Rock.fetch (Elara.Query.GetCoreModule moduleName)

-- Key queries and their dependencies:
GetCoreModule mn        -- requires TypeCheckedModule
GetOptimisedCoreModule  -- requires GetCoreModule, applies CoreToCore passes
GetANFCoreModule        -- requires GetOptimisedCoreModule, converts to ANF
GetClosureLiftedModule  -- requires GetANFCoreModule, lifts closures to top-level
GetFinalisedCoreModule  -- requires GetClosureLiftedModule, type-checks result
```

**Adding a new query:**
1. Add constructor to `Query` GADT in `Query.hs` with effect requirements
2. Implement handler in `rules` function in `Rules.hs`
3. Use `Rock.fetch` to invoke from other stages

## Type Inference

Located in `src/Elara/TypeInfer/`. Uses constraint-based Hindley-Milner with:
- **Constraint generation** ([ConstraintGeneration.hs](src/Elara/TypeInfer/ConstraintGeneration.hs)): Walks AST, emits equality constraints
- **Unification**: Solves constraints via substitution
- **Generalization**: Quantifies free type variables at let-bindings

```haskell
-- The Infer effect stack (from Monad.hs):
type Infer loc r =
    ( Writer (Constraint loc) :> r      -- Emit constraints
    , State (LocalTypeEnvironment loc) :> r  -- Local bindings (lambdas)
    , State (TypeEnvironment loc) :> r  -- Global type environment
    , Error (InferError loc) :> r
    , Rock.Rock Elara.Query.Query :> r  -- Query other modules
    )
```

Type inference processes declarations in **strongly connected components** (SCCs) to handle mutual recursion correctly.

## JVM Lowering

Located in `src/Elara/JVM/`. Two-phase process:

1. **Lower** ([Lower.hs](src/Elara/JVM/Lower.hs)): Core → JVM IR
   - Each module becomes a class, each binding becomes a static method
   - Handles lambda lifting, eta expansion, pattern matching
   - ADTs become separate classes with `equals`/`toString` methods

2. **Emit** ([Emit.hs](src/Elara/JVM/Emit.hs)): JVM IR → Bytecode
   - Converts abstract IR to concrete JVM instructions
   - Handles lambda creation via `invokedynamic`

```haskell
-- Lower effect constraint (from Lower/Monad.hs):
type Lower r = (UniqueGen :> r, StructuredDebug :> r)

-- Inner lowering (within method bodies) adds instruction emission:
type InnerLower r = (Lower r, InstructionWriter :> r)
```

## Effect System: Effectful

Uses **Effectful** library (not MTL or Polysemy). Key patterns:

```haskell
-- Effect constraint style (row polymorphism):
lowerModule :: Lower r => CoreModule CoreBind -> Eff r IR.Module

-- Type alias for common effect sets:
type LiftClosures r = (UniqueGen :> r, Writer [LiftedClosure] :> r, StructuredDebug :> r)

-- Running effects:
runWriter @[LiftedClosure] $ do ...
runError @MyError $ do ...
```

Common effects: `UniqueGen`, `StructuredDebug`, `Error e`, `State s`, `Writer w`, `Rock.Rock Query`

## AST Design: Type-Level Stage Selection

The AST is **parametric over compilation stages** using type families ([src/Elara/AST/Generic/Types.hs](src/Elara/AST/Generic/Types.hs)):

```haskell
type family Select (s :: ASTSelector) (ast :: LocatedAST) :: Type
-- Example: Select ASTVarRef Frontend ≠ Select ASTVarRef Typed
```

This allows the same `Expr` type to represent different stages with different field types.

## Build & Test Commands

```bash
# Build
cabal build

# Run tests
cabal test elara-test

# Run compiler
cabal run elara -- --run          # Interpret
cabal run elara -- --run-jvm      # Compile & run via JVM

# Debug flags (also available as env vars ELARA_DUMP_*)
--dump-core --dump-typed --dump-ir --dump-jvm
```

Before running, rebuild JVM stdlib if changed:
```bash
cd jvm-stdlib && javac Elara/*.java && cd ..
```

## Code Conventions

- **Custom Prelude**: [src/Prelude.hs](src/Prelude.hs) re-exports Relude + Optics. Use `identity` not `id`.
- **Optics**: Uses `optics` library (not `lens`). Operators: `^.`, `^..`, `%~`, `.~`
- **Pretty printing**: Use `Elara.Data.Pretty` module, `pretty` function
- **Error handling**: `ReportableError` typeclass for diagnostic output

## Key Directories

- `src/Elara/AST/` - All AST types for each stage
- `src/Elara/Core/` - Core IR and transformations (ANF, closure lifting)
- `src/Elara/JVM/` - JVM lowering, IR, and bytecode emission
- `src/Elara/TypeInfer/` - Hindley-Milner type inference
- `stdlib/` - Elara standard library (`.elr` files)
- `jvm-stdlib/` - Java runtime support classes
- `source.elr` - Main entry point file (must contain `Main` module)

## Common Patterns

### Adding a new Core transformation
1. Create module in `src/Elara/Core/`
2. Add query to `Query.hs`, implement in `Rules.hs`
3. Wire into pipeline in `CoreToCore.hs`

### Extending the AST for a new stage
1. Add stage to `LocatedAST` in `Select.hs`
2. Define `Select` instances for stage-specific types
3. Implement transformation pass

## Testing

Tests use **sydtest** framework. Golden tests compare output against fixtures in `test/test_resources/`.
