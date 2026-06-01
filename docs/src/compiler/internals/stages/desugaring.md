# Desugaring

## Overview


The desugaring stage transforms the parsed Frontend AST into a simplified Desugared AST. Despite the name, this stage is fairly conservative, and a lot of more substantial desugaring happens in later stages.

Its primary functions are unifying separate type signatures and value bindings into a single declaration representation, and stripping away a few specific pieces of syntactic sugar, such as multi-parameter lambdas.

## Core Mechanisms

Because the frontend AST allows type signatures and value declarations to exist independently and out of order, this module requires a multi-pass approach. We keep track of this in the [`DesugarState`]({{hackage_base}}/Elara-Desugar.html#v:DesugarState) data structure to track and merge declarations.

This process completes in a few distinct passes:

1. Generating Partials (`genPartials`)
The compiler iterates through all module declarations and converts them into [`PartialDeclaration`]({{hackage_base}}/Elara-Desugar-Error.html#t:PartialDeclaration) states, keyed by their identifier.

These represent incomplete declarations, that may be just a type signature, just a value declaration, or another type of declaration, which is more or less ignored.

This pass merges the partials it generates incrementally:
- If a `def` and `let` are both present, we merge them into an `AllDecl` value
- If only a `def` is present, we create a `JustDef` value
- If only a `let` is present, we create a `JustLet` value
- Other declaration types have their ASTs desugared directly into `Immediate` values which are already complete and require no merging

2. Completing Partials (`completePartials`)

After the first pass, we have a map of all identifiers to their partial declarations. In this pass, we iterate through this map and transform the partials back into AST declarations, or throw errors

If we encounter `JustDef`, we throw an error because we have a type signature without a corresponding value.

If we encounter a `JustLet` or an `AllDecl`, we create a complete `ValueDeclaration` AST node, merging the type signature and value declaration as necessary.

Again, `Immediate` values are already transformed and can be emitted as-is.


## AST Transformations

Many parts of the AST are representationally equivalent and so have instances of `PhaseCoerce`.

However we still do a traversal for most of the AST, which handles some specific desugarings:

- Multi parameter lambdas desugar into nested single-parameter lambdas, e.g. `\x y -> x` becomes `\x -> \y -> x`
- `let` bindings with multiple parameters are also desugared into bindings with nested lambdas, e.g. `let f x y = x` becomes `let f = \x -> \y -> x`
- Tuple pattterns are checked that they have at least length 2. This is mostly a sanity check because the parser should never emit a 1-tuple pattern

Everything else is more or less the identity traversal