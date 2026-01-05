{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Select where

type data LocatedAST = Frontend | Desugared | Renamed | Shunted | MidKinded | Kinded | Typed | Core

type data UnlocatedAST = UnlocatedFrontend | UnlocatedDesugared | UnlocatedRenamed | UnlocatedShunted | UnlocatedTyped

type family LocatedToUnlocated (ast :: LocatedAST) :: UnlocatedAST where
    LocatedToUnlocated Frontend = UnlocatedFrontend
    LocatedToUnlocated Desugared = UnlocatedDesugared
    LocatedToUnlocated Renamed = UnlocatedRenamed
    LocatedToUnlocated Shunted = UnlocatedShunted
    LocatedToUnlocated Typed = UnlocatedTyped

type family UnlocatedToLocated (ast :: UnlocatedAST) :: LocatedAST where
    UnlocatedToLocated UnlocatedFrontend = Frontend
    UnlocatedToLocated UnlocatedDesugared = Desugared
    UnlocatedToLocated UnlocatedRenamed = Renamed
    UnlocatedToLocated UnlocatedShunted = Shunted
    UnlocatedToLocated UnlocatedTyped = Typed

{- | Elements of the AST that are parametric by their AST stage
This type acts as a selector for the 'Select' type family for these elements
-}
type data ASTSelector
    = -- | The type used for references to variables
      ASTVarRef
    | -- | References to type / data constructors
      ConRef
    | -- | Patterns that can be bound in a lambda expression (eg @\[a] (b, c) -> ...@)
      LambdaPattern
    | -- | The value stored in type application nodes (usually a 'Type')
      TypeApplication
    | -- | The type used to represent an explicit type declaration for patterns, e.g. @[Int]@ in @f ([y] :: [Int])@
      PatternType
    | -- | Value of binary operator nodes. Replaced with 'NoFieldValue' after shunting
      ASTBinaryOperator
    | -- | Value of list expressions
      List
    | -- | Type used for the name of a value bound by a @let@ expression
      LetParamName
    | -- | Patterns that can be bound in a @let@ expression
      LetPattern
    | -- | Type used for the name of a constructor within its declaration only. Often differs to 'ConRef' in what types of qualification it allows
      ConstructorName
    | -- | Type of parameters for a data constructor in its declaration, e.g.the second @a@ in @type Box a = Box a@
      ADTParam
    | -- | Type used for the RHS of a type alias declaration
      Alias
    | -- | Type of annotations for a selected AST node type
      Annotations ForSelector
    | -- | Type for Kind annotations upon a type declaration only
      KindAnnotation
    | -- | Type of type variables used in any way
      ASTTypeVar
    | -- | Type for the actual kind of a type stored within the 'Type', i.e. at use sites. Will probably be merged with 'KindAnnotation' as they aren't clearly distinguishable
      TypeKind
    | -- | Type for a reference to a user defined type, i.e. a named type that is not a primitive. Usually similar/identical to 'ConRef'
      UserDefinedType
    | -- | Type of a name of a specific element
      ASTName ForSelector
    | -- | Type of pattern(s) that can be bound by a specific element
      Patterns ForSelector
    | -- | Type of types of specific elements
      ASTType ForSelector
    | -- | Type used for the value of variable patterns, i.e. a variable name
      VarPat
    | -- | Type used for the value of constructor patterns, i.e. a constructor name
      ConPat
    | -- | Type used for the name of an annotation
      AnnotationName
    | -- | Type used for the <type> part in a @def <name> : <type>@ statement
      ValueTypeDef
    | -- | Type of a tuple expression
      Tuple
    | -- | Type of an expression wrapped in parenthesis
      InParens
    | -- | A list literal pattern
      ListPattern
    | -- | A tuple literal pattern
      TuplePattern
    | -- | A cons literal pattern
      ConsPattern
    | -- | An operator's name that is symbolic, i.e. made of symbols
      SymOp
    | -- | The name of the "operator" in an infix (e.g. @a `f` b@) expression
      Infixed

type data ForSelector
    = ForType
    | ForTypeDecl
    | ForValueDecl
    | ForExpr
