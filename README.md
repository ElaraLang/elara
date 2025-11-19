# Elara Programming Language

![CI](https://github.com/ElaraLang/elara/actions/workflows/ci.yaml/badge.svg)

Elara is a **purely functional programming language** targeting the JVM. It draws inspiration from Haskell and F#, featuring a **Hindley-Milner type system** and **pure functions in the type system** (i.e., an IO monad).  

> ⚠️ **Work in Progress:** Elara is experimental and **not ready for production use**.  

---

## Examples

### Hello World

```fs
def main : IO ()
let main = print "Hello, World!"

Factorial

def factorial : Int -> Int
let factorial n = if n == 0 then 1 else n * factorial (n - 1)

Lists

def sum : [Int] -> Int
let sum ls =
    match ls with
        [] -> 0
        x::xs -> x + sum xs

def main : IO ()
let main = print (sum [1, 2, 3])
-- Prints 6

Higher-Order Functions

def map : (a -> b) -> [a] -> [b]
let map f ls =
    match ls with
        [] -> []
        x::xs -> f x :: map f xs

def main : IO ()
let main = print (map (\x -> x * 2) [1, 2, 3])
-- Prints [2, 4, 6]

Data Types

type Option a = None | Some a

def map : (a -> b) -> Option a -> Option b
let map f opt =
    match opt with
        None -> None
        Some x -> Some (f x)

def main : IO ()
let main = print (map (\x -> x * 2) (Some 3))
-- Prints Some 6

Getting Involved

Interested in Elara or contributing? Join our Discord server

for updates, discussions, and collaboration.
Building & Running

Elara is still buggy and experimental, but it should function with the following prerequisites:
Prerequisites

    Java Runtime Environment (JRE): Java 8 or higher.

    Rebuild the JVM standard library (especially after modifying files):

cd jvm-stdlib
javac Elara/Error.java Elara/Func.java Elara/Func2.java Elara/IO.java Elara/Int.java Elara/Prelude.java Elara/Unit.java Elara/Func0.java
cd ../

Code Structure

Elara programs currently require a rigid structure:

    The root source.elr file must contain a Main module with a main function of type IO ().

    All other code resides in the stdlib directory (standard library).

Library Acknowledgments

Elara is inspired by and owes thanks to numerous open-source projects, particularly Grace

for its type checking implementation.
License

Elara is released under the MIT License

.
Technical Details
Compiler Overview

The Elara compiler is structured as multiple passes over the source code:

    Lexing: Converts source code into tokens, handles layout rules, and strips comments.

    Parsing: Produces the Frontend AST and checks for syntax errors.

    Desugaring: Converts Frontend AST to Desugared AST:

        Multi-argument lambdas → nested single-argument lambdas

        Let bindings → lambdas

        Duplicate declarations merged

    Renaming: Generates unique names and resolves references in the Renamed AST.

    Shunting: Applies operator precedence and associativity, transforming binary operators into prefix calls.

    Type Checking: Infers and validates types, producing the Typed AST.

    ToCore: Converts Typed AST to Core AST (typed lambda calculus) with heavy desugaring.

    CoreToCore: Performs optimizations on the Core AST.

    Emitting: Generates JVM bytecode and writes class files.


This version is **ready to paste directly into your `README.md`** and looks clean on GitHub with proper headings, code blocks, and badges.  

If you want, I can also **add a “Contributing” section with beginner-friendly tasks** to encourage new contributors. Do you want me to do that?

