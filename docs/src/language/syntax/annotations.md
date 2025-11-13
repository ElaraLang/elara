# Annotations

Annotations provide a way to attach metadata to various syntactic constructs in Elara.

## Syntax

Annotations are always specified using the `#` symbol followed by an annotation name and optional parameters in parentheses. The eventual goal is that an annotation can be applied to any syntactic construct, however this is a work in progress.

### Operator Information

When declaring custom operators, eg

```fs
def (++) : String -> String -> String
let (++) = ...
```

we use annotations to specify the operator's metadata:

```fs
#LeftAssociative
#Fixity 6
def (++) : String -> String -> String
```
## Annotation Arguments

Annotations can also take arguments. For example, the `Fixity` annotation above takes a single integer argument specifying the operator's precedence level.

The syntax for expression arguments is a subset of Elara where every expression must evaluate to a constant value at compile time. Specifically, this permits:
- Literal values (integers, strings, booleans, etc.)
- Constructor application where all arguments are constant values
- Tuples and lists of constant values


## Defining Annotations

Currently, annotations are identical to data types. That is to say, every data type which only accepts constant values can be used as an annotation. For example, we can define an annotation to specify that a function should be memoised:

```fs
type Memoise = Memoise
```

The aforementioned `LeftAssociative` annotation is simply defined as:

```fs
type Associativity =
    LeftAssociative
    | RightAssociative
    | NonAssociative
```

TODO: I think we can extend this system in a lot of ways : for example, compile time metaprogramming, allowing annotations restrict where certain constructs can be used, aspect oriented programming, etc.
could we make annotations first class / functions? eg `annotation Memoise : (a -> b) -> (a -> b)`
