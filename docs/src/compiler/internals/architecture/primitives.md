# Primitives

As with all programming languages, some primitive types and operations are built into the compiler, and necessary for the language to function.
Elara divides these into two categories: **Opaque Primitives** (types with no source definition) and **Wired In Primitives** (types defined as normal source code, but with special compiler handling).

These are defined in and managed by a central registry in [`src/Elara/Prim.hs`]({{repo_base}}/src/Elara/Prim.hs).

## Opaque Primitives (`OpaquePrim`)

Opaque primitives are types or operations that have no definition in source code. They exist entirely as compiler built-ins because they cannot be implemented in Elara itself. For example, `Int` must be a primitive type. The specific implementation of these is left to the backend.

Each OpaquePrim has an internal compiler name (e.g., `Prim_Int`) to prevent accidental shadowing by user code, and a user-facing alias defined in [`Elara.Prim.elr`]({{repo_base}}/stdlib/Elara.Prim.elr) (e.g., `type Int = Prim_Int`).

**Compiler handling**:
- Opaque primitives are pre-seeded into the [Rename](../stages/renaming.md) state ([`primitiveRenameState`]({{hackage_base}}/Elara-Prim-Rename.html#v:primitiveRenameState)), so they are in scope before any module imports are processed.
- Handled directly by the JVM backend which decides how to represent them


## Wired In Primitives (`WiredInPrim`)

Wired-in Primitives are defined as normal source code in [`stdlib/Elara.Prim.elr`]({{repo_base}}/stdlib/Elara.Prim.elr). The compiler treats them as special not because they have any intrinsic properties, but because they are structurally required for the language to function. This includes things like `Bool`, `List`, and `()`. Certain syntax elements (e.g. `if` expressions, list literals) desugar to these types, so the compiler has to be aware of their existence, but they are not fundamentally different to any other user-defined type.


**Compiler handling**:
Wired-in primitives are not treated any differently to any other user-defined code. 
However, the compiler assumes their existence and references them with a fully qualified name - so shadowing them locally is not possible, nor is removing them entirely. 

## Primitive Erasure

Backends are free to treat _both_ kinds of primitives as they see fit. For example, the JVM backend provides direct support for all `OpaquePrim`s, but also rewrites some `WiredInPrim`s such as `Bool` to JVM primitives for efficiency. 

To maintain binary compatibility, backends must not rewrite public APIs that reference these primitives, but are free to rewrite internal code as they see fit. For example, a public function that returns `Bool` must return an actual `Bool`, but internal code that uses `Bool` can be rewritten to use JVM primitives and then re-boxed to `Bool` before being returned.

<div class="note">
This policy may be revisited in the future and replaced with things like monomorphisation
</div>