# Primitives

Elara has a small set of built-in primitive types. These have no definition in the language and are hardcoded into the compiler. The primitive types are:
- `Int_Prim`: Represents integer numbers.
- `Float_Prim`: Represents floating-point numbers.
- `Char_Prim`: Represents single characters.
- `IO_Prim`: Represents input/output operations.
- `String_Prim`: Represents sequences of characters.
- `()`: The unit type

Each primitive type has a public facing alias defined in the `Elara.Prim` module, eg `type Int = Int_Prim`.

The compiler will always desugar relevant values (e.g. unit literals) to the _primitive_ type.
For example, integer literals are treated as being of type `Int_Prim`.

This system should probably be redesigned in the future - I like that primitives have normal definitions but we are currently very inconsistent internally about how they're handled which causes a lot of spaghetti.