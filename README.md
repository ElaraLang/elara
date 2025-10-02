# Elara Programming Language

![CI](https://github.com/ElaraLang/elara/actions/workflows/ci.yaml/badge.svg)

Elara is a purely functional programming language targeting the JVM. It features a Haskell & F# inspired syntax, a complete Hindley-Milner type system and pure functions in the type system (i.e. an IO monad).

**Please note that Elara is currently a work in progress and is not yet ready for production use.**

## Examples

### Hello World

```fs
def main : IO ()
let main = print "Hello, World!"
```

### Factorial

```fs
def factorial : Int -> Int
let factorial n = if n == 0 then 1 else n * factorial (n - 1)
```

### Lists

```fs
def sum : [Int] -> Int
let sum ls =
    match ls with
        [] -> 0
        x::xs -> x + sum xs

def main : IO ()
let main = print (sum [1, 2, 3])
-- Prints 6
```

### Higher-Order Functions

```fs
def map : (a -> b) -> [a] -> [b]
let map f ls =
    match ls with
        [] -> []
        x::xs -> f x :: map f xs

def main : IO ()
let main = print (map (\x -> x * 2) [1, 2, 3])
-- Prints [2, 4, 6]
```

### Data Types

```fs
type Option a = None | Some a

def map : (a -> b) -> Option a -> Option b
let map f opt =
    match opt with
        None -> None
        Some x -> Some (f x)

def main : IO ()
let main = print (map (\x -> x * 2) (Some 3))
-- Prints Some 6
```

## Getting Involved

If you're interested in Elara or contributing to its development, join our [Discord server](https://discord.gg/xu5gSTV) for frequent updates and discussions.

## Building & Running

Elara is extremely buggy and temperamental at the moment, but it _should_ function!
The recommended workflow to build is with Nix, as this will ensure you have the correct versions of all dependencies.
If you don't have / want Nix, you _should_ be able to get away with a manually installed GHC 9.12.2 and Cabal

### Running Prerequisites

1. To run Elara you need a JRE. Anything above Java 8 should work
2. Before running, and if you change the files, make sure to rebuild the Java standard library:

```sh
cd jvm-stdlib
javac Elara/Error.java Elara/Func.java Elara/Func2.java Elara/IO.java Elara/Int.java Elara/Prelude.java Elara/Unit.java Elara/Func0.java
cd ../
```

### Building with Nix

1. Run `nix build` to build
2. You should be able to access Elara the executable from `./result/bin/elara`

### Hacking with Nix

1. Run `nix develop` to enter a shell with all dependencies
2. Use `just run` to run in development mode (with an interpreter)
3. To run unit tests, run `just test`

### Building without Nix

1. Run `cabal build` to build

### Running without Nix

1. Run `cabal run` to run
2. Run `cabal test` to run unit tests

#### Flags

Elara supports a few flags for debugging and development:
- `--dump-lexed` - Dumps a list of tokens after lexing
- `--dump-parsed` - Dumps the Frontend AST after parsing
- `--dump-desugared` - Dumps the Desugared AST after desugaring
- `--dump-renamed` - Dumps the Renamed AST after renaming
- `--dump-shunted` - Dumps the Shunted AST after shunting
- `--dump-typed` - Dumps the Typed AST after type checking
- `--dump-core` - Dumps the Core AST after converting to Core

These will all dump to the generated `build/` directory, matching the module name where possible.


**Important Caveat:** Currently the `--run` flag must be used to run the program in interpreted mode. You will receive a warning if you do not use this flag and no code will be run. The `just` commands above already include this flag.

## Code Structure

Currently the structure of an Elara program is very rigid: the `source.elr` file in the root directory _must_ contain a `Main` module with a `main` function of type `IO ()`, and all other code must be part of the standard library, in the `stdlib` directory.

## Library Acknowledgments

Elara owes its existence to numerous open-source projects, particularly to [Grace](https://github.com/Gabriella439/grace) for its use as a reference implementation for type checking.

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
9. **Emitting:** The Core AST is converted into JVM bytecode and written to a class file.

---
