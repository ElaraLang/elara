# Module System Redesign

The Elara module system is designed to be simple, predictable, and flexible, drawing inspiration from Haskell and Rust.

## Core Principles

1.  **File System as Truth**: The identity of a module is primarily determined by its location in the file system relative to defined **source roots**.
2.  **No Scans**: The compiler does not scan all files to discover modules. It resolves modules on-demand based on their import paths.
3.  **Optional Declarations**: The `module Name` declaration in the file is optional. If omitted, the module name is inferred from the file path. If present, it must match the inferred name (checked by the compiler).

## Source Roots

A project defines a set of **source roots** (e.g., `src`, `lib`, `test`, `stdlib`).
When resolving a module name like `Data.List`, the compiler looks for corresponding files in these roots in order.

## Module Resolution

To resolve a module named `A.B.C`, the compiler searches the source roots for the following files (in order of preference):

1.  **Nested**: `root/A/B/C.elr`
2.  **Rust-style**: `root/A/B/C/mod.elr`
3.  **Flat**: `root/A.B.C.elr`

This hybrid approach allows for both organized nested structures and flat directory layouts where preferred.

### Examples

Given source root `src/`:

*   `import Data.List` looks for:
    *   `src/Data/List.elr`
    *   `src/Data/List/mod.elr`
    *   `src/Data.List.elr`

*   `import Main` looks for:
    *   `src/Main.elr`
    *   `src/Main/mod.elr`
    *   `src/Main.elr` (Flat check, same as nested for top-level)

## The `mod.elr` File

Inspired by Rust's `mod.rs`, a `mod.elr` file represents the directory it resides in.
For example, `src/Data/mod.elr` defines the module `Data`. This allows defining a module that also serves as a namespace for submodules (e.g., `src/Data/List.elr`).

## Module Header

The module header is now optional.

```elara
-- src/Math/Utils.elr

-- Optional:
-- module Math.Utils 

let add x y = x + y
```

If the `module` declaration is provided, the compiler will verify that it matches the expected name derived from the file path. A mismatch results in a compile error.

## Refactoring

Renaming a module is as simple as renaming the file or directory. Since the module declaration inside the file is optional, you often don't need to touch the file content at all.

## Principal Type Import

When importing a module `M` qualified (e.g. `import M qualified`), if the module exports a type with the same name as the module (i.e. `type M = ...`), that type is imported **unqualified**. All other members must be accessed with the qualifier (e.g. `M.foo`).

This is particularly useful for types like `Result` or `Option` where the module name matches the main type name.

```elara
-- In Result.elr
module Result
type Result e a = Ok a | Err e
let map f r = ...

-- In Main.elr
import Result qualified

let x : Result String Int = Result.Ok 1 -- Result is unqualified, Ok is qualified
let y = Result.map (\x -> x + 1) x
```