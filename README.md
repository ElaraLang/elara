
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

-   **Languages:** Primarily implemented in Haskell (95%), with contributions from Nix (1.3%) and Lex (1.3%).

------
