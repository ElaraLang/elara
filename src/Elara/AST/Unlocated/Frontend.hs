module Elara.AST.Unlocated.Frontend where

import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (LowerAlphaName, MaybeQualified, ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Pretty 
import Elara.AST.Region
import Elara.AST.StripLocation
import Elara.Data.Pretty
import TODO (todo)
import Prelude hiding (Op)

-- | Frontend AST without location information.
--     Trees that grow was getting quite frustrating, so we're stuck with this for now.
--     I apologise to future me.
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
  deriving (Show, Eq)

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
  { _declarationModule' :: ModuleName,
    _declarationName :: Name,
    _declarationBody :: DeclarationBody
  }
  deriving (Show, Eq)

data DeclarationBody
  = -- | let <p> = <e>
    Value
      { _expression :: Expr,
        _patterns :: [Pattern]
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

instance StripLocation Frontend.Expr Expr where
  stripLocation (Frontend.Expr (Located _ expr)) = case expr of
    Frontend.Int i -> Int i
    Frontend.Float f -> Float f
    Frontend.String s -> String s
    Frontend.Char c -> Char c
    Frontend.Unit -> Unit
    Frontend.Var v -> Var (stripLocation v)
    Frontend.Constructor c -> Constructor (stripLocation c)
    Frontend.Lambda p e -> Lambda (stripLocation p) (stripLocation e)
    Frontend.FunctionCall e1 e2 -> FunctionCall (stripLocation e1) (stripLocation e2)
    Frontend.If e1 e2 e3 -> If (stripLocation e1) (stripLocation e2) (stripLocation e3)
    Frontend.BinaryOperator o e1 e2 -> BinaryOperator (stripLocation o) (stripLocation e1) (stripLocation e2)
    Frontend.List l -> List (stripLocation l)
    Frontend.Match e m -> Match (stripLocation e) (stripLocation m)
    Frontend.LetIn v p e1 e2 -> LetIn (stripLocation v) (stripLocation p) (stripLocation e1) (stripLocation e2)
    Frontend.Let v p e -> Let (stripLocation v) (stripLocation p) (stripLocation e)
    Frontend.Block b -> Block (stripLocation b)
    Frontend.InParens e -> InParens (stripLocation e)
    Frontend.Tuple l -> Tuple (stripLocation l)

instance StripLocation Frontend.Pattern Pattern where
  stripLocation (Frontend.Pattern (Located _ pat)) = case pat of
    Frontend.VarPattern n -> VarPattern (stripLocation n)
    Frontend.ConstructorPattern c p -> ConstructorPattern (stripLocation c) (stripLocation p)
    Frontend.ListPattern p -> ListPattern (stripLocation p)
    Frontend.ConsPattern p1 p2 -> ConsPattern (stripLocation p1) (stripLocation p2)
    Frontend.WildcardPattern -> WildcardPattern
    Frontend.IntegerPattern i -> IntegerPattern i
    Frontend.FloatPattern f -> FloatPattern f
    Frontend.StringPattern s -> StringPattern s
    Frontend.CharPattern c -> CharPattern c

instance StripLocation Frontend.BinaryOperator BinaryOperator where
  stripLocation (Frontend.MkBinaryOperator (Located _ op)) = case op of
    Frontend.Op o -> Op (stripLocation o)
    Frontend.Infixed i -> Infixed (stripLocation i)

instance StripLocation Frontend.Type Type where
  stripLocation (Frontend.TypeVar t) = TypeVar (stripLocation t)
  stripLocation (Frontend.FunctionType t1 t2) = FunctionType (stripLocation t1) (stripLocation t2)
  stripLocation Frontend.UnitType = UnitType
  stripLocation (Frontend.TypeConstructorApplication t1 t2) = TypeConstructorApplication (stripLocation t1) (stripLocation t2)
  stripLocation (Frontend.UserDefinedType t) = UserDefinedType (stripLocation t)
  stripLocation (Frontend.RecordType r) = RecordType (stripLocation r)
  stripLocation (Frontend.TupleType t) = TupleType (stripLocation t)
  stripLocation (Frontend.ListType t) = ListType (stripLocation t)

instance StripLocation Frontend.Declaration Declaration where
  stripLocation (Frontend.Declaration d) = stripLocation d

instance StripLocation Frontend.Declaration' Declaration where
  stripLocation (Frontend.Declaration' m n b) = Declaration (stripLocation m) (stripLocation n) (stripLocation b)

instance StripLocation Frontend.DeclarationBody DeclarationBody where
  stripLocation (Frontend.DeclarationBody d) = stripLocation d

instance StripLocation Frontend.DeclarationBody' DeclarationBody where
  stripLocation (Frontend.Value e p) = Value (stripLocation e) (stripLocation p)
  stripLocation (Frontend.ValueTypeDef t) = ValueTypeDef (stripLocation t)
  stripLocation (Frontend.TypeDeclaration args t) = TypeDeclaration (stripLocation args) (stripLocation t)

instance StripLocation Frontend.TypeDeclaration TypeDeclaration where
  stripLocation (Frontend.ADT t) = ADT (stripLocation t)
  stripLocation (Frontend.Alias t) = Alias (stripLocation t)

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
  pretty (BinaryOperator o e1 e2) = prettyBinaryOperatorExpr o e1 e2
  pretty (List l) = prettyListExpr l
  pretty (Match e m) = prettyMatchExpr e m
  pretty (LetIn v ps e1 e2) = prettyLetInExpr v ps e1 e2
  pretty (Let v ps e) = prettyLetExpr v ps e
  pretty (Block b) = prettyBlockExpr b
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
