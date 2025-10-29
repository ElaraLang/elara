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
@LeftAssociative
@Fixity(6)
def (++) : String -> String -> String
```

