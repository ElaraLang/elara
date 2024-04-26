
# Elara Programming Language

![CI](https://github.com/ElaraLang/elara/actions/workflows/ci.yaml/badge.svg)
![Static Badge](https://img.shields.io/badge/Code%20Quality-A%2B-blue)


Elara is a multi-paradigm, primarily-functional programming language targeting the JVM. It features a Haskell & F# inspired syntax, a complete Hindley-Milner type system, functional purity enforcement, and can interoperate with Java code.

**Please note that Elara is currently a work in progress and is not yet ready for production use.**

## Features

-   **Syntax:** Elara's syntax draws inspiration from Haskell and F#, offering an expressive and concise coding style.
-   **Type System:** Elara uses a Hindley-Milner type system for robust type checking and inference.
-   **Functional Purity:** The language enforces functional purity, promoting clean and side-effect-free code.
-   **Interoperability:** Elara seamlessly works with Java code, enabling integration with existing Java libraries.

## Getting Involved

If you're interested in Elara or contributing to its development, join our [Discord server](https://discord.gg/xu5gSTV) to connect with the community.

## Building & Running
Elara is extremely buggy and temperamental at the moment, but it *should* function!
The recommended workflow to build is with Nix, as this will ensure you have the correct versions of all dependencies.
If you don't have / want Nix, you *should* be able to get away with GHC 9.4.8 and Stack

### Running Prerequisites
1. To run Elara you need a JRE. Anything above Java 8 should work
2. Before running, and if you change the files, make sure to rebuild the Java standard library:
```sh
cd jvm-stdlib
javac elara/Error.java elara/Prelude.java elara/Func.java elara/Int.java elara/EList.java elara/IO.java elara/EList.java
cd ../
```

### Building with Nix
1. Run `nix build` to build
2. You should be able to access Elara the executable from `./result/bin/elara`

### Hacking with Nix
1. Run `nix develop` to enter a shell with all dependencies
2. You can type `, run` to setup GHCIde and continuously build & run the project whenever a file changes
3. To run unit tests, simply run `stack test`

### Building without Nix
1. Run `stack build` to build

### Running without Nix
1. Run `stack run` to run
2. Run `stack test` to run unit tests



## Library Acknowledgments

Elara owes its existence to numerous open-source projects. We want to express our gratitude, especially to [Grace](https://github.com/Gabriella439/grace) for its invaluable contribution to type checking.

## License

Elara is released under the [MIT License](LICENSE).

## Technical Details

### How the compiler works
The Elara compiler is fairly simple, composed of multiple passes on the source code. These are as follows:
1. **Lexing:** The source code is converted into a list of tokens, layout rules are converted into braces and semicolons, and comments are removed.
2. **Parsing:** The list of tokens is converted into the Frontend abstract syntax tree (AST). The Frontend AST almost directly mirrors the syntax of the language with very few transformations applied. The Parser checks for any syntax errors and reports them.
3. **Desugaring:** The Frontend AST is converted into the Desugared AST. The Desugared AST is a bit misleading in that it is actually fairly conservative in its desugaring. The main differences are that multi-argument lambdas are converted into nested single-argument lambdas, let bindings have their parameters removed and are converted into lambdas, and declarations with identical names (i.e. `def` and `let`s) are converted into a single declaration.
4. **Renaming:** Conversion to Renamed AST with name resolution and unique-ification. Every name is either resolved to a fully qualified name, or local variables are generated a unique name to avoid collisions.
5. **Shunting:** Conversion to Shunted AST with operator precedence and associativity applied. A simple reassociation of the AST to match the precedence and associativity of operators. The notion of a "binary operator" is removed as they are all turned into prefix function calls. 
6. **Type Checking:** The Shunted AST is converted into the Typed AST by inferring and checking the types of every expression.
7. **ToCore:** The Typed AST is converted into the Core AST. The Core AST is a very simple AST that is essentially a typed lambda calculus, very similar to GHC's Core language. Very extensive desugaring is done here, converting the entire language to the 8 constructors of the Core AST.
8. **CoreToCore:** Repeated transformations of the Core AST, mainly performing optimisations.
9.  **Emitting:** The Core AST is converted into JVM bytecode and written to a class file.
------
