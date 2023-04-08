{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Typed where

import Control.Lens hiding (List)
import Data.Data (Data)
import Elara.AST.Name (LowerAlphaName, ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Unlocated.Typed qualified as Unlocated
import Elara.AST.VarRef
import Elara.Data.Pretty
import Prettyprinter hiding (Pretty(..))
import Elara.Data.Unique
import Elara.TypeInfer.Type (Type)
import Prelude hiding (Op, group)

{- | Typed AST Type
This is very similar to 'Elara.AST.Shunted.Expr' except:

- Everything has a type!
-}
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (VarRef VarName))
    | Constructor (Located (Qualified TypeName))
    | Lambda (Located (Unique VarName)) Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Located (Unique VarName)) Expr Expr
    | Let (Located (Unique VarName)) Expr
    | Block (NonEmpty Expr)
    | Tuple (NonEmpty Expr)
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr', Type SourceRegion)
    deriving (Show, Eq)

data Pattern'
    = VarPattern (Located (VarRef VarName))
    | ConstructorPattern (Located (Qualified TypeName)) [Pattern]
    | ListPattern [Pattern]
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern', Type SourceRegion)
    deriving (Show, Eq)

data TypeAnnotation t = TypeAnnotation (Located (Qualified Name)) t
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

newtype Declaration = Declaration (Located Declaration')
    deriving (Show, Eq)

data Declaration' = Declaration'
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located (Qualified Name)
    , _declaration'Body :: DeclarationBody
    }
    deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
    deriving (Show, Eq)

data DeclarationBody'
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr
        }
    | -- | type <name> <vars> = <type>
      TypeDeclaration [Located (Unique LowerAlphaName)] (Located TypeDeclaration) -- No difference to old AST
    deriving (Show, Eq)

data TypeDeclaration
    = ADT (NonEmpty (Located (Qualified TypeName), [Type SourceRegion]))
    | Alias (Type SourceRegion)
    deriving (Show, Eq)

makePrisms ''Declaration
makePrisms ''Declaration'
makeLenses ''Declaration'
makePrisms ''DeclarationBody
makePrisms ''DeclarationBody'
makeLenses ''DeclarationBody
makeLenses ''DeclarationBody'
makePrisms ''Expr
makePrisms ''Expr'
makePrisms ''VarRef'
makePrisms ''Pattern

instance StripLocation Expr Unlocated.Expr where
    stripLocation (Expr (e, t)) = Unlocated.Expr (stripLocation $ stripLocation e, stripLocation t)

instance StripLocation Expr' Unlocated.Expr' where
    stripLocation e = case e of
        Int i -> Unlocated.Int i
        Float f -> Unlocated.Float f
        String s -> Unlocated.String s
        Char c -> Unlocated.Char c
        Unit -> Unlocated.Unit
        Var lv -> Unlocated.Var (stripLocation $ stripLocation lv)
        Constructor q -> Unlocated.Constructor (stripLocation q)
        Lambda (Located _ u) e' -> Unlocated.Lambda u (stripLocation e')
        FunctionCall e1 e2 -> Unlocated.FunctionCall (stripLocation e1) (stripLocation e2)
        If e1 e2 e3 -> Unlocated.If (stripLocation e1) (stripLocation e2) (stripLocation e3)
        List es -> Unlocated.List (stripLocation es)
        Match e' pes -> Unlocated.Match (stripLocation e') (stripLocation pes)
        LetIn (Located _ u) e1 e2 -> Unlocated.LetIn u (stripLocation e1) (stripLocation e2)
        Let (Located _ u) e' -> Unlocated.Let u (stripLocation e')
        Block es -> Unlocated.Block (stripLocation es)
        Tuple es -> Unlocated.Tuple (stripLocation es)

instance StripLocation Pattern Unlocated.Pattern where
    stripLocation (Pattern (p, t)) = Unlocated.Pattern (stripLocation $ stripLocation p, stripLocation t)

instance StripLocation Pattern' Unlocated.Pattern' where
    stripLocation p = case p of
        VarPattern v -> Unlocated.VarPattern (stripLocation $ stripLocation v)
        ConstructorPattern q ps -> Unlocated.ConstructorPattern (stripLocation q) (stripLocation ps)
        WildcardPattern -> Unlocated.WildcardPattern
        IntegerPattern i -> Unlocated.IntegerPattern i
        FloatPattern f -> Unlocated.FloatPattern f
        StringPattern s -> Unlocated.StringPattern s
        CharPattern c -> Unlocated.CharPattern c
        ListPattern c -> Unlocated.ListPattern (stripLocation c)

-- Pretty Printing :D

instance Pretty Declaration where
    pretty (Declaration ldb) = pretty ldb

instance Pretty Declaration' where
    pretty (Declaration' _ n b) = prettyDB (n ^. unlocated) (b ^. _DeclarationBody . unlocated)

prettyDB :: Qualified Name -> DeclarationBody' -> Doc AnsiStyle
prettyDB name (Value (Expr (e, t))) =
    vsep
        [ (pretty name) <+> ":" <+> pretty t
        , -- , "let" <+> pretty name <+> "="
          -- , indent indentDepth (pretty e)
          "" -- add a newline
        ]
prettyDB name (TypeDeclaration vars t) =
    vsep
        [ keyword "type" <+> typeName (pretty name) <+> hsep (varName . pretty <$> vars)
        , indent indentDepth (pretty t)
        ]

instance Pretty TypeDeclaration where
    pretty (Alias t) = "=" <+> pretty t
    pretty (ADT constructors) = group $ encloseSep "= " "" (flatAlt "| " " | ") (prettyCtor <$> toList constructors)
      where
        prettyCtor (name, args) = hsep (typeName (pretty name) : (pretty <$> args))

instance Pretty Expr where
    pretty e = pretty (stripLocation e)
instance Pretty Expr' where
    pretty e = pretty (stripLocation e)

instance (Pretty t) => Pretty (TypeAnnotation t) where
    pretty (TypeAnnotation _ t) = pretty t
