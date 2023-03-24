#import "template.typ": *
#import "functions.typ": *
// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "Elara Language Specification",
  authors: (
    "Alexander Wood",
  ),
  date: "March 23, 2023",
)

= Introduction
Elara is a statically-typed multi-paradigm programming language targeting the JVM and based on the Hindley-Milner type system. It supports a succinct,
Haskell-like syntax while preserving readability and ease of use.

Elara focuses on the purely functional paradigm, but also supports Object Oriented
programming and imperative programming.

Elara's notable features include:
- Structural pattern matching with exhaustiveness checking
- Type classes for polymorphism
- Complete sound type inference with higher-kinded and higher-rank types

== Code Examples
While all the following examples are syntactically correct, they may assume the existence of functions not provided in the examples in order to compile.

=== Hello World
```ocaml
let main = println "Hello World!"
```

=== Pattern Matching over Lists and Higher Order Functions
```ocaml
let map f list =
    match list with
        [] -> []
        (x :: xs) -> f x :: map xs f
let main = 
    let list = [1, 2, 3, 4]
    let doubleNum i = i * 2
    println (map doubleNum list)  
```

=== Custom Data Types
Elara has a very flexible type system which allows for many different forms of data types to be defined. \
```ocaml
type Name = String // Simple type alias

type Animal = Cat | Dog // Simple Discriminated Union

type Person = { // Record Type
    name : Name,
    age : Int,
}

type RequestState = // Complex Discriminated Union with different constructor arities
      Connected String
    | Pending
    | Failed Error

type Option a = // Polymorphic (generic) data types
      Some a
    | None

type Fix f = Fix (f (Fix f)) // Recursive, higher-kinded data types

// Combination of multiple type features
type JSONElement = 
      JSONString String
    | JSONNumber Int
    | JSONNull
    | JSONArray [JSONElement]
    | JSONObject [{ // record syntax can be used anonymously
        key : String,
        value : JSONElement
    }] 
```

=== Pattern Matching on Data Types

```ocaml
type JSONElement =
      JSONString String
    | JSONNumber Int
    | JSONNull
    | JSONArray [JSONElement]
    | JSONObject [{
        key : String,
        value : JSONElement
    }]

let jsonToString elem = match elem with
    JSONNull -> "null"
    JSONString str -> str
    JSONNumber num -> toString num
    JSONArray arr -> "[" <> (String.join ", " (map jsonToString arr)) <> "]"
    JSONObject components -> 
        let componentToString { key, value } = 
            "\"" <> key <> "\" : " <> jsonToString value
        in "{" <> String.join ", " (map componentToString components) <> "}"
```

=== Type Classes
```ocaml
type String = [Char]

type class ToString a where
    toString : a -> String

instance ToString String where
    toString s = s

instance ToString Char where
    toString c = [c]

-- Impure function taking any s with an instance ToString s, returns Unit
def print : ToString s => s -> IO () 
let print s = println (toString s)

let main = 
    print "Hello"
    print 'c'
    print 123 // Doesn't compile, no ToString instance for Int
```

=== Monad Comprehension / Notation
```ocaml
def sequenceActions : Monad m => [m a] -> m [a]
def sequenceActions list = match list with
    [] -> pure []
    (x:xs) ->
        let! x' = x
        let! xs' = sequenceActions xs
        pure (x' : xs')

def sequenceActions_ : Monad m => [m a] -> m ()
def sequenceActions_ list = match list with
    [] -> pure ()
    (x:xs) ->
        do! x
        sequenceActions_ xs
```

== Syntax
=== Multi-line Environments <multi-line-environments>
Some syntactic structures in Elara can create multi-line environments. Formally, this means that rather than a single expression, a semicolon-separated list of _statement_\s, surrounded by braces, can be used where a multi-line environment is permitted. \
Practically, this allows the imperative idea of "blocks" of code to be used, rather than having a binding be a single long expression.
Note that this feature is merely syntax-sugar and does not change the purely-functional semantics of Elara.
When in a multi-line environment, the syntax is extended to allow imperative statements:

- Standalone let bindings: \
    `let x = 1;`
- Monadic let expressions: \
    `let! x = action in x + 1;`
- Monadic let bindings: \
    `let! x = action;`
- Monadic do statements: \
    `do! action;`
- Monadic (applicative) return statements: \
    `return! 1;`

=== Lightweight Syntax
Elara allows lightweight syntax, a feature heavily inspired by F\#. This makes newlines and indentation significant, allowing the omission of many braces and other tokens.
Its use is recommended in almost every case.

Note that the tokens that can be ignored when using lightweight syntax may still be written manually, making the use of lightweight syntax effectively optional.
The only difference is that the lexer should insert them implicitly _if and only if they are missing_ when using lightweight syntax.

==== Lightweight Syntax Rules by Example
The following describes the lightweight syntax rules in an informal, example-based manner.

===== Optional Semicolons

In normal syntax, semicolons are required to separate statements and must appear at the end of every declaration or statement. In lightweight mode, semicolons are optional and are inferred by the presence of a newline (`\n`) character.

#beforeAndAfter[
*Normal Syntax*
```ocaml
    let x = 1;
    let y = 2;
    let main = println (x + y);
```
][
*Lightweight Syntax*  
```ocaml
    let x = 1
    let y = 2
    let main = println (x + y)
```
]

===== Optional Braces

In normal syntax, braces are required when beginning a multi-line environment (see @multi-line-environments)
or in a few other cases (such as the body of a match expression)

In lightweight syntax, braces are optional and can be inferred by newlines and indentation.

#beforeAndAfter[
*Normal Syntax*
```ocaml
let x = {
    1;
}
```
][
*Lightweight Syntax*
```ocaml
let x = 
    1
```
]
#beforeAndAfter[
```ocaml
let y = \x -> {
    1;
}
```
][
```ocaml
let y = \x ->
    1
```
]

#beforeAndAfter[
```ocaml
let main = match x with {
    1 -> {
        println "it's 1";
    };
    _ -> {
        println "it's not 1";
    };
}
```
][
```ocaml
let main = match x with
    1 ->
        println "it's 1"
    _ ->
        println "it's not 1"
```
]

#beforeAndAfter[
```ocaml
let test x = {
    if x then {
        1;
    } else {
        2;
    }
}
```
][
```ocaml
let test x = 
    if x then 
        1
    else
        2
```
]

==== Offside Rule
When using lightweight syntax, the indentation is flexible but not arbitrary. The "offside rule" is used to determine which columns code should be indented to. The offside rule marks specific columns in the source code as "offside" for a given syntactic construct. 
Code on proceeding lines must be indented to the same column to be considered part of the same syntactic construct.
If the code is indented further and a new offside rule cannot be triggered, an error is raised.
If the code is indented less, the offside rule is exited and the code is considered to be part of the parent syntactic construct

The following tokens trigger the offside rule at their respective columns:
- The first non-whitespace token after the `=` token of a `let` construct
- The first non-whitespace token after the `->` token of a lambda
- The first non-whitespace token after the `then` token of an `if` expression
- The first non-whitespace token after the `else` token of an `if` expression
- The first non-whitespace token after the `with` token of an `match` expression
- The first non-whitespace token after the `->` token of an `match` case
- The first non-whitespace token after a `{` token
- The start of a `let`, `if` or `module` token


===== The Offside Rule in Action
The following examples demonstrate the offside rule in action, noting when it is used incorrectly and errors should be raised.
\

```ocaml
module Main       | module keyword triggers the offside rule
let x = 1         | marks offside column
  let y = 2       | considered part of x, error!
let z = 3         | correctly indented
```
As the `module` keyword triggers the offside rule, all top-level declarations must be indented to the same column.
This can produce some interesting, but correct results. All top-level declarations can be indented as long as it's consistent:
```ocaml
module Main     | module keyword triggers the offside rule
  let x = 1     | marks offside column
  let y = 2     | same column, fine 
let z = 3       | bad! not considered part of the module, error!
```

```ocaml
let x =            | = triggers the offside rule
    let y = 1      | correctly indented
      let z = 2    | bad! indented too much
    y + z          | correctly indented
```

```ocaml
let main =                | = triggers the offside rule
  match [1, 2, 3] with    | with triggers the offside rule
    x :: [] ->  "one"     | marks offside column
   x :: xs -> "many"      | bad! not indented enough
     [] -> "empty list"   | bad! too indented
```

