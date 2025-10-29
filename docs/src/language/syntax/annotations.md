# Annotations

Annotations provide a way to attach metadata to various syntactic constructs in Elara. 

## Syntax

Annotations are always specified using the `@` symbol followed by an annotation name and optional parameters in parentheses. The eventual goal is that an annotation can be applied to any syntactic construct, however this is a work in progress.

## Operator Information

When declaring custom operators, eg 
```fs
def (++) : String -> String -> String
let (++) = ...
```

we use annotations to specify the operator's metadata:

```fs
#leftAssociative
#fixity 6
def (++) : String -> String -> String
```

## Defining Annotations

Annotations themselves can be defined using the `annotation` keyword:

```fs
annotation targets (target : [AnnotationTarget])

#targets [OperatorDecl]
annotation leftAssociative

#targets [OperatorDecl]
annotation fixity (precedence : Int)
```

TODO: I think we can extend this system in a lot of ways : for example, compile time metaprogramming, allowing annotations restrict where certain constructs can be used, aspect oriented programming, etc.
could we make annotations first class / functions? eg `annotation Memoise : (a -> b) -> (a -> b)`