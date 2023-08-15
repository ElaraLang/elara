module Elara.AST.Unlocated.Frontend where

import Elara.AST.Name (LowerAlphaName, MaybeQualified, ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Pretty
import Elara.Data.Pretty
import Prelude hiding (Op)

{- | Frontend AST without location information.
     Trees that grow was getting quite frustrating, so we're stuck with this for now.
     I apologise to future me.
-}
data Expr
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (MaybeQualified VarName)
    | Constructor (MaybeQualified TypeName)
    | Lambda [Pattern] Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn VarName [Pattern] Expr Expr
    | Let VarName [Pattern] Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    | Tuple (NonEmpty Expr)
    deriving (Show, Eq, Generic)

data Pattern
    = VarPattern VarName
    | ConstructorPattern (MaybeQualified TypeName) [Pattern]
    | ListPattern [Pattern]
    | ConsPattern Pattern Pattern
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

data BinaryOperator
    = Op (MaybeQualified OpName)
    | Infixed (MaybeQualified VarName)
    deriving (Show, Eq)

data Type
    = TypeVar LowerAlphaName
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (MaybeQualified TypeName)
    | RecordType (NonEmpty (VarName, Type))
    | TupleType (NonEmpty Type)
    | ListType Type
    deriving (Show, Eq)

data Declaration = Declaration
    { _declarationModule' :: ModuleName
    , _declarationName :: Name
    , _declarationBody :: DeclarationBody
    }
    deriving (Show, Eq)

data DeclarationBody
    = -- | let <p> = <e>
      Value
        { _expression :: Expr
        , _patterns :: [Pattern]
        }
    | -- | def <name> : <type>.
      ValueTypeDef Type
    | -- | type <name> <args> = <type>
      TypeDeclaration [LowerAlphaName] TypeDeclaration
    deriving (Show, Eq)

data TypeDeclaration
    = ADT (NonEmpty (TypeName, [Type]))
    | Alias Type
    deriving (Show, Eq)

instance Pretty Expr where
    pretty (Int i) = pretty i
    pretty (Float f) = pretty f
    pretty (String s) = pretty '\"' <> pretty s <> pretty '\"'
    pretty (Char c) = "'" <> escapeChar c <> "'"
    pretty Unit = "()"
    pretty (Var v) = pretty v
    pretty (Constructor c) = pretty c
    pretty (Lambda ps e) = prettyLambdaExpr ps e
    pretty (FunctionCall e1 e2) = prettyFunctionCallExpr e1 e2
    pretty (If e1 e2 e3) = prettyIfExpr e1 e2 e3
    pretty (BinaryOperator o e1 e2) = prettyBinaryOperatorExpr e1 o e2
    pretty (List l) = prettyListExpr l
    pretty (Match e m) = prettyMatchExpr e m
    pretty (LetIn v ps e1 e2) = prettyLetInExpr v ps e1 (Just e2)
    pretty (Let v ps e) = prettyLetExpr v ps e
    pretty (Block b) = bracedBlock (toList b)
    pretty (InParens e) = parens (pretty e)
    pretty (Tuple t) = parens (hsep (punctuate "," (pretty <$> toList t)))

instance Pretty Pattern where
    pretty (VarPattern v) = pretty v
    pretty (ConstructorPattern c p) = parens (pretty c <+> hsep (pretty <$> p))
    pretty (ListPattern p) = list (pretty <$> p)
    pretty (ConsPattern p1 p2) = parens (pretty p1 <+> "::" <+> pretty p2)
    pretty WildcardPattern = "_"
    pretty (IntegerPattern i) = pretty i
    pretty (FloatPattern f) = pretty f
    pretty (StringPattern s) = pretty s
    pretty (CharPattern c) = pretty c

instance Pretty BinaryOperator where
    pretty (Op o) = pretty o
    pretty (Infixed i) = "`" <> pretty i <> "`"
