# Renaming

## Overview

The renaming stage resolves all names in the desugared AST to either qualified global references or alpha-unique local references. It processes module imports, builds a scope from imported declarations, and assigns unique identifiers to all locally-bound names to eliminate shadowing ambiguity.

## Core Mechanisms

The [`rename`]({{hackage_base}}/Elara-Rename.html#v:rename) function takes a [`Module SourceRegion Desugared`]({{hackage_base}}/Elara-AST-Module.html#t:Module) and produces a [`Module SourceRegion Renamed`]({{hackage_base}}/Elara-AST-Module.html#t:Module), transforming every name into either a [`Global (Located (Qualified name))`]({{hackage_base}}/Elara-AST-VarRef.html#v:Global) or a [`Local (Located (Unique name))`]({{hackage_base}}/Elara-AST-VarRef.html#v:Local).

### Name Resolution

Unqualified names (e.g. `foo`) are looked up in the [`RenameState`]({{hackage_base}}/Elara-Rename-State.html#t:RenameState) scope maps, which map simple names to all possible references. Qualified names (e.g. `Module.foo`) are validated against the referenced module's exports via [`ensureExistsAndExposed`]({{hackage_base}}/Elara-Rename.html#v:ensureExistsAndExposed), which checks both that the name exists and that it is part of the module's public exports.

### Module Imports

Import processing in [`addModuleToContext`]({{hackage_base}}/Elara-Rename.html#v:addModuleToContext) operates in two phases:

1. **Filter exports**: The imported module's public exports are computed by filtering its declarations against its exposing list.
2. **Apply import clause**: The filtered exports are then matched against the import list with validation that explicitly named imports are actually exported.

Unqualified imports add declarations directly to scope. Qualified imports only add the principal type (a type whose name matches the last segment of the module name) to the unqualified scope, and everything else is only accessible via qualified references.

### Primitive Injection

[`primitiveRenameState`]({{hackage_base}}/Elara-Prim-Rename.html#v:primitiveRenameState) pre-populates the scope with all opaque primitive types (Int, Float, Double, String, Char, IO) as `Global` references. 

No variables or constructors are pre-populated: wired-in types such as `Bool`, `List`, `Tuple2`, and `Unit` are resolved through normal import processing or syntactic sugar desugaring.

### Syntactic Sugar

Some syntactic sugar is desugared at rename time:

- `TUnit` types are desugared to `Elara.Prim.Unit`
- Tuple expressions are desugared to applications of `Elara.Prim.Tuple2` (and `Tuple3`, etc.)
- List syntax is desugared to chains of `Elara.Prim.Cons` terminated by `Elara.Prim.Nil`

## Scope Management

The [`RenameState`]({{hackage_base}}/Elara-Rename-State.html#t:RenameState) maintains three scope maps:

- `varNames`: `Map VarName (NonEmpty (VarRef VarName))` - value and constructor names
- `typeNames`: `Map TypeName (NonEmpty (VarRef TypeName))` - type names
- `typeVars`: `Map LowerAlphaName (Unique LowerAlphaName)` - type variables

Declarations are wrapped with `scoped` to isolate their bindings. Let bindings, lambda parameters, and pattern matches add local entries with `locally`. Type variables in type declarations and annotations are uniquified and added to `typeVars`. Data constructors are added to `varNames` during import processing and module declaration scanning.

## Unique Generation

The [`uniquify`]({{hackage_base}}/Elara-Rename.html#v:uniquify) function calls [`makeUnique`]({{hackage_base}}/Elara-Unique.html#v:makeUnique) to assign a fresh unique suffix to each bound variable, preventing name clashes from shadowing. Source location metadata is preserved on the uniquified name.

## Error Handling

When the renamer encounters problems, it throws a [`RenameError`]({{hackage_base}}/Elara-Rename-Error.html#t:RenameError):

- `UnknownName` - name not found in scope
- `AmbiguousVarName` / `AmbiguousTypeName` - name matches multiple unqualified imports
- `UnknownModule` - imported module does not exist
- `ModuleNameMismatch` - module name does not match file path
- `NonExistentModuleDeclaration` - module declaration does not exist
- `UnknownTypeVariable` - type variable not in scope
- `RecursiveTypeAlias` - a type alias directly or indirectly references itself
- `BlockEndsWithLet` - a block ends with a let binding, we can't desugar this in any meaningful way
- `QualifiedInWrongModule` - a qualified declaration is used in the wrong module
- `UnknownCurrentModule` - internal error when the current module cannot be determined
