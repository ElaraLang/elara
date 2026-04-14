# ToCore

## Overview

The ToCore stage lowers the typed AST into the [Core](../architecture/core.md) intermediate representation: a small, explicitly-typed lambda calculus with let bindings, case expressions, and primitive operations. This is heavily inspired by GHC's Core language.
While types are not _erased_, they are represented very differently from the surface syntax: identifiers have types attached to them, and everything else is done with explicit type applications and abstractions.

Pattern matching is also compiled from nested patterns into flat case trees using Maranget's algorithm.
