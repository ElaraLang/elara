# Core-to-Core

After the initial [ToCore](./to-core.md) translation, the Core representation passes through several transformation passes before being handed to a backend. These passes are chained via the [Rock query system](../architecture/query-system.md) rather than being explicitly sequenced; each pass fetches its input from the previous pass's query.

## Overview

The pipeline consists of four main passes: optimisation, ANF conversion, closure lifting, and a final type-checking sanity pass. [`runGetFinalisedCoreModuleQuery`]({{hackage_base}}/Elara-CoreToCore.html#v:runGetFinalisedCoreModuleQuery) is the final step in this chain, which fetches the closure-lifted module from the query system, runs a type-checking pass to verify correctness, and converts from ANF back to regular Core via [`unANF`]({{hackage_base}}/Elara-CoreToCore.html#v:unANF).

## Passes

### Optimised Core

Optimisation passes are applied to each binding via [`fullCoreToCoreExpr`]({{hackage_base}}/Elara-CoreToCore.html#v:fullCoreToCoreExpr), which composes all individual passes and iterates them to a fixed point (i.e. until the expression stops changing).

#### Beta Reduction

[`betaReduce`]({{hackage_base}}/Elara-CoreToCore.html#v:betaReduce) rewrites `(λx.e) a` to `e[x/a]` using structural substitution. Because Core variables have already been uniquified during [renaming](./renaming.md), we can do a direct substitution without worrying about capture. Yay!

#### Constant Folding

[`constantFold`]({{hackage_base}}/Elara-CoreToCore.html#v:constantFold) evaluates a limited set of `PrimOp` applications on known `Int` literals, specifically addition, subtraction, and multiplication.

This will be expanded in the future.

#### Inlining

- [`pipeInline`]({{hackage_base}}/Elara-CoreToCore.html#v:pipeInline) desugars the pipe operator `|>` into direct function application (`a |> b` becomes `b a`)
- [`uselessLetInline`]({{hackage_base}}/Elara-CoreToCore.html#v:uselessLetInline) removes trivial `let x = e in x` bindings

These will both be generalised in the future to a general inlining pass

### ANF Conversion

ANF (Administrative Normal Form) makes evaluation order explicit by naming all intermediate results. The ANF representation separates expressions into three levels:

- [`AExpr b`]({{hackage_base}}/Elara-Core-ANF.html#t:AExpr) - atomic values: `Var`, `Lit`, `Lam`, `TyApp`, `TyLam`, `ANFPrimOp`
- [`CExpr b`]({{hackage_base}}/Elara-Core-ANF.html#t:CExpr) - compound operations: `App`, `Match`, or an atomic wrapper (`AExpr`)
- [`Expr b`]({{hackage_base}}/Elara-Core-ANF.html#t:Expr) - sequences of `Let` bindings ending in a `CExpr`

Conversion uses continuation-passing style, where complex sub-expressions are extracted into `let` bindings via explicit continuations. For example, `print(f(g(1)))` becomes `let v1 = g(1) in let v2 = f(v1) in print(v2)`.

### Closure Lifting

Closure lifting moves all nested lambdas to top-level functions. Free variables captured from the enclosing environment become extra parameters on the lifted function. Fresh names are generated for each lifted function (using [`makeUnique`]({{hackage_base}}/Elara-Unique.html#v:makeUnique) with descriptive base names derived from the enclosing context), and call sites are updated to pass the captured values as additional arguments.

Free variable analysis is performed via the [`FreeCoreVars`]({{hackage_base}}/Elara-Core-Analysis.html#t:FreeCoreVars) typeclass, which has instances for all Core and ANF expression types.

### Finalised Core

After all passes, [`unANF`]({{hackage_base}}/Elara-CoreToCore.html#v:unANF) converts ANF back to regular Core expressions. A type-checking sanity pass runs on the closure-lifted ANF code (before `unANF`) via [`typeCheckCoreModule`]({{hackage_base}}/Elara-Core-TypeCheck.html#v:typeCheckCoreModule). This is purely validation, not inference, and serves to catch any bugs introduced by the optimisation and transformation passes. As such, it has much simpler error handling than the main type checker.

## Input / Output per Pass

| Pass | Input | Output |
|------|-------|--------|
| Optimisation | `CoreModule CoreBind` | `CoreModule CoreBind` (optimised) |
| ANF Conversion | `CoreModule CoreBind` (optimised) | ANF module |
| Closure Lifting | ANF module | ANF module (no nested lambdas) |
| Finalisation | ANF module (lifted) | `CoreModule CoreBind` (via `unANF` + type check) |
