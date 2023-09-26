{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Elara.AST.Generic where

-- import Elara.AST.Frontend qualified as Frontend

import Control.Lens (Each (each), Plated (plate), Traversal', traverseOf, view, (^.), _1, _2)
import Control.Lens.Extras (template, uniplate)
import Data.Data (Data)
import Data.Generics.Wrapped
import Data.Kind qualified as Kind
import Elara.AST.Generic.Utils
import Elara.AST.Name (LowerAlphaName, ModuleName, VarName (..))
import Elara.AST.Pretty
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select (LocatedAST, UnlocatedAST)
import Elara.AST.StripLocation (StripLocation (..))
import Elara.Data.Pretty
import GHC.TypeLits
import Relude.Extra (bimapF)
import Prelude hiding (group)

-- | Used to select a field type for a given AST.
--
-- Conventions for usage:
-- If a selection is likely to be one of the "principal" newtypes ('Expr', 'Pattern', etc), it should not be wrapped in 'ASTLocate',
-- as this increases friction and creates redundant 'Located' wrappers.
-- This means that implementations should manually wrap in 'Locate' if not using one of the principle newtypes
type family Select (s :: Symbol) (ast :: a) = (v :: Kind.Type)

data Expr' (ast :: a)
  = Int Integer
  | Float Double
  | String Text
  | Char Char
  | Unit
  | Var (ASTLocate ast (Select "VarRef" ast))
  | Constructor (ASTLocate ast (Select "ConRef" ast))
  | Lambda
      (ASTLocate ast (Select "LambdaPattern" ast))
      (Expr ast)
  | FunctionCall (Expr ast) (Expr ast)
  | TypeApplication (Expr ast) (Select "ExprType" ast)
  | If (Expr ast) (Expr ast) (Expr ast)
  | BinaryOperator !(Select "BinaryOperator" ast)
  | List [Expr ast]
  | Match (Expr ast) [(Pattern ast, Expr ast)]
  | LetIn
      (ASTLocate ast (Select "LetParamName" ast))
      (Select "LetPattern" ast)
      (Expr ast)
      (Expr ast)
  | Let
      (ASTLocate ast (Select "LetParamName" ast))
      (Select "LetPattern" ast)
      (Expr ast)
  | Block (NonEmpty (Expr ast))
  | InParens !(Select "InParens" ast)
  | Tuple (NonEmpty (Expr ast))
  deriving (Generic)

newtype Expr (ast :: a) = Expr (ASTLocate ast (Expr' ast), Select "ExprType" ast)
  deriving (Generic, Typeable)

typeOf :: forall ast. Expr ast -> Select "ExprType" ast
typeOf (Expr (_, t)) = t

data Pattern' ast
  = VarPattern (ASTLocate ast (Select "VarPat" ast))
  | ConstructorPattern (ASTLocate ast (Select "ConPat" ast)) [Pattern ast]
  | ListPattern [Pattern ast]
  | ConsPattern (Pattern ast) (Pattern ast)
  | WildcardPattern
  | IntegerPattern Integer
  | FloatPattern Double
  | StringPattern Text
  | CharPattern Char
  | UnitPattern
  deriving (Generic)

newtype Pattern ast = Pattern (ASTLocate ast (Pattern' ast), Select "PatternType" ast)
  deriving (Generic)

data BinaryOperator' (ast :: a)
  = SymOp (ASTLocate ast (Select "SymOp" ast))
  | Infixed (Select "Infixed" ast)
  deriving (Generic)

newtype BinaryOperator ast = MkBinaryOperator (ASTLocate ast (BinaryOperator' ast))
  deriving (Generic)

newtype Declaration ast = Declaration (ASTLocate ast (Declaration' ast))
  deriving (Generic)

data Declaration' (ast :: a) = Declaration'
  { moduleName :: ASTLocate ast ModuleName,
    name :: ASTLocate ast (Select "DeclarationName" ast),
    body :: DeclarationBody ast
  }
  deriving (Generic)

newtype DeclarationBody (ast :: a) = DeclarationBody (ASTLocate ast (DeclarationBody' ast))
  deriving (Generic)

data DeclarationBody' (ast :: a)
  = -- | let <p> = <e>
    Value
      { _expression :: Expr ast,
        _patterns :: Select "ValuePatterns" ast,
        _valueType :: Select "ValueType" ast
      }
  | -- | def <name> : <type>.
    ValueTypeDef !(Select "ValueTypeDef" ast)
  | -- | type <name> <vars> = <type>
    TypeDeclaration
      [ASTLocate ast (Select "TypeVar" ast)]
      (ASTLocate ast (TypeDeclaration ast))
  deriving (Generic)

data TypeDeclaration ast
  = ADT (NonEmpty (ASTLocate ast (Select "ConstructorName" ast), [Type ast]))
  | Alias (Type ast)
  deriving (Generic)

newtype Type ast = Type (ASTLocate ast (Type' ast))
  deriving (Generic)

data Type' ast
  = TypeVar (ASTLocate ast (Select "TypeVar" ast))
  | FunctionType (Type ast) (Type ast)
  | UnitType
  | TypeConstructorApplication (Type ast) (Type ast)
  | UserDefinedType (ASTLocate ast (Select "UserDefinedType" ast))
  | RecordType (NonEmpty (ASTLocate ast LowerAlphaName, Type ast))
  | TupleType (NonEmpty (Type ast))
  | ListType (Type ast)
  deriving (Generic)

-- Ttg stuff

type RUnlocate :: ast -> Kind.Constraint
class RUnlocate ast where
  rUnlocate ::
    forall a.
    (CleanupLocated (Located a) ~ Located a) =>
    ASTLocate ast a ->
    a

  fmapUnlocated ::
    forall a b.
    (CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
    (a -> b) ->
    ASTLocate ast a ->
    ASTLocate ast b

  traverseUnlocated ::
    forall f a b.
    (Applicative f, CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
    (a -> f b) ->
    ASTLocate ast a ->
    f (ASTLocate ast b)

instance (ASTLocate' ast ~ Located) => RUnlocate (ast :: LocatedAST) where
  rUnlocate = view unlocated
  fmapUnlocated = fmap
  traverseUnlocated = traverse

instance (ASTLocate' ast ~ Unlocated) => RUnlocate (ast :: UnlocatedAST) where
  rUnlocate = identity
  fmapUnlocated f = f
  traverseUnlocated f = f

type ASTLocate :: a -> Kind.Type -> Kind.Type
type ASTLocate ast a = CleanupLocated (ASTLocate' ast a)

type FullASTQual :: a -> Kind.Type -> Kind.Type
type FullASTQual ast a = ((ASTLocate ast) (ASTQual ast a))

newtype Unlocated a = Unlocated a

-- | Unwraps a single layer of 'Unlocated' from a type.
type family CleanupLocated g where
  CleanupLocated (Unlocated a) = a
  CleanupLocated (Located (Located a)) = CleanupLocated a
  -- Remove located wrappers for the newtypes
  CleanupLocated (Located (Expr a)) = CleanupLocated (Expr a)
  CleanupLocated (Located (Pattern a)) = CleanupLocated (Pattern a)
  CleanupLocated (Located (BinaryOperator a)) = CleanupLocated (BinaryOperator a)
  CleanupLocated (Located (Declaration a)) = CleanupLocated (Declaration a)
  CleanupLocated (Located (DeclarationBody a)) = CleanupLocated (DeclarationBody a)
  CleanupLocated (Located (Type a)) = CleanupLocated (Type a)
  CleanupLocated a = a

type family ASTLocate' (ast :: a) :: Kind.Type -> Kind.Type

type family ASTQual (ast :: a) :: Kind.Type -> Kind.Type

-- Coercions

coerceTypeDeclaration :: (_) => TypeDeclaration ast1 -> TypeDeclaration ast2
coerceTypeDeclaration (Alias a) = Alias (coerceType a)
coerceTypeDeclaration (ADT a) = ADT (fmap coerceType <<$>> a)

coerceType :: (_) => Type ast1 -> Type ast2
coerceType (Type a) = Type (coerceType' <$> a)

coerceType' :: (_) => Type' ast1 -> Type' ast2
coerceType' (TypeVar a) = TypeVar a
coerceType' (FunctionType a b) = FunctionType (coerceType a) (coerceType b)
coerceType' UnitType = UnitType
coerceType' (TypeConstructorApplication a b) = TypeConstructorApplication (coerceType a) (coerceType b)
coerceType' (UserDefinedType a) = UserDefinedType a
coerceType' (RecordType a) = RecordType (fmap coerceType <$> a)
coerceType' (TupleType a) = TupleType (coerceType <$> a)
coerceType' (ListType a) = ListType (coerceType a)

-- Pretty printing

deriving newtype instance (Pretty (ASTLocate ast (BinaryOperator' ast))) => Pretty (BinaryOperator ast)

instance
  ( Pretty (CleanupLocated (ASTLocate' ast (Select "SymOp" ast))),
    (Pretty (Select "Infixed" ast))
  ) =>
  Pretty (BinaryOperator' ast)
  where
  pretty (SymOp op) = pretty op
  pretty (Infixed op) = "`" <> pretty op <> "`"

deriving newtype instance (Pretty (ASTLocate ast (Type' ast))) => Pretty (Type ast)

instance
  ( Pretty (ASTLocate ast (Declaration' ast))
  ) =>
  Pretty (Declaration ast)
  where
  pretty (Declaration ldb) = pretty ldb

data UnknownPretty = forall a. (Pretty a) => UnknownPretty a

instance Pretty UnknownPretty where
  pretty (UnknownPretty a) = pretty a

instance
  ( Pretty (Expr ast),
    Pretty (CleanupLocated (ASTLocate' ast (Select "TypeVar" ast))),
    Pretty (CleanupLocated (ASTLocate' ast (Select "DeclarationName" ast))),
    Pretty (CleanupLocated (ASTLocate' ast (TypeDeclaration ast))),
    Pretty valueType,
    ToMaybe (Select "ValueType" ast) (Maybe valueType),
    valueType ~ UnwrapMaybe (Select "ValueType" ast),
    Pretty exprType,
    exprType ~ UnwrapMaybe (Select "ExprType" ast),
    (ToMaybe (Select "ExprType" ast) (Maybe exprType)),
    RUnlocate (ast :: b)
  ) =>
  Pretty (Declaration' ast)
  where
  pretty (Declaration' _ n b) =
    let val = b ^. _Unwrapped
        y = rUnlocate @b @ast @(DeclarationBody' ast) val
     in prettyDB n y
    where
      -- The type of a 'Value' can appear in 2 places: Either as a field in 'Value''s constructor, or as the second field of the 'Expr' tuple
      -- We know that only one will ever exist at a time (in theory, this isn't a formal invariant) so need to find a way of handling both cases
      -- The fields have different types, but both are required to have a Pretty instance (see constraints above).
      -- 'prettyValueDeclaration' takes a 'Pretty a3 => Maybe a3' as its third argument, representing the type of the value.
      -- To make the two compatible, we create an existential wrapper 'UnknownPretty' which has a 'Pretty' instance, and use that as the type of the third argument.
      -- The converting of values to a 'Maybe' is handled by the 'ToMaybe' class.
      prettyDB n (Value e@(Expr (_, t)) _ t') =
        let typeOfE =
              UnknownPretty <$> (toMaybe t :: Maybe exprType) -- Prioritise the type in the expression
                <|> UnknownPretty <$> (toMaybe t' :: Maybe valueType) -- Otherwise, use the type in the declaration
         in prettyValueDeclaration n e typeOfE
      prettyDB n (TypeDeclaration vars t) = prettyTypeDeclaration n vars t

instance Pretty (TypeDeclaration ast) where
  pretty _ = "TODO"

instance
  ( exprType ~ UnwrapMaybe (Select "ExprType" ast), -- This constraint fixes ambiguity errors
    letPatterns ~ UnwrapList (Select "LetPattern" ast),
    lambdaPatterns ~ UnwrapList (Select "LambdaPattern" ast),
    Pretty (ASTLocate ast (Select "ConRef" ast)),
    Pretty (ASTLocate ast (Select "VarRef" ast)),
    (Pretty (Select "InParens" ast)),
    (Pretty (ASTLocate ast (Select "LetParamName" ast))),
    Pretty letPatterns,
    letPatterns ~ UnwrapList (Select "LetPattern" ast),
    (ToList (Select "LetPattern" ast) [letPatterns]),
    Pretty lambdaPatterns,
    lambdaPatterns ~ UnwrapList (Select "LambdaPattern" ast),
    (ToList (ASTLocate ast (Select "LambdaPattern" ast)) [lambdaPatterns]),
    (Pretty (ASTLocate ast (BinaryOperator' ast))),
    (ToMaybe (Select "ExprType" ast) (Maybe (UnwrapMaybe (Select "ExprType" ast)))),
    (ToMaybe (Select "PatternType" ast) (Maybe (UnwrapMaybe (Select "PatternType" ast)))),
    (Pretty (UnwrapMaybe (Select "ExprType" ast))),
    (Pretty (UnwrapMaybe (Select "PatternType" ast))),
    (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast)))),
    (StripLocation (ASTLocate ast (Expr' ast)) (Expr' ast)),
    (Pretty (Select "ExprType" ast)),
    (DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast))
  ) =>
  Pretty (Expr ast)
  where
  pretty =
    let ?withType = True
        ?contextFree = True
     in prettyExpr @ast @exprType @letPatterns @lambdaPatterns

prettyExpr ::
  forall ast exprType letPatterns lambdaPatterns.
  ( exprType ~ UnwrapMaybe (Select "ExprType" ast), -- This constraint fixes ambiguity errors
    letPatterns ~ UnwrapList (Select "LetPattern" ast),
    lambdaPatterns ~ UnwrapList (Select "LambdaPattern" ast),
    Pretty (ASTLocate ast (Select "ConRef" ast)),
    Pretty (ASTLocate ast (Select "VarRef" ast)),
    (Pretty (Select "InParens" ast)),
    (Pretty (ASTLocate ast (Select "LetParamName" ast))),
    Pretty letPatterns,
    (ToList (Select "LetPattern" ast) [letPatterns]),
    Pretty lambdaPatterns,
    (ToList (ASTLocate ast (Select "LambdaPattern" ast)) [lambdaPatterns]),
    (Pretty (ASTLocate ast (BinaryOperator' ast))),
    (ToMaybe (Select "ExprType" ast) (Maybe (UnwrapMaybe (Select "ExprType" ast)))),
    (ToMaybe (Select "PatternType" ast) (Maybe (UnwrapMaybe (Select "PatternType" ast)))),
    (Pretty (UnwrapMaybe (Select "ExprType" ast))),
    (Pretty (UnwrapMaybe (Select "PatternType" ast))),
    (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast)))),
    (StripLocation (ASTLocate ast (Expr' ast)) (Expr' ast)),
    (Pretty (Select "ExprType" ast)),
    (DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast)),
    (?contextFree :: Bool, ?withType :: Bool)
  ) =>
  Expr ast ->
  Doc AnsiStyle
prettyExpr (Expr (e, t)) = group (flatAlt long short)
  where
    te = if ?withType then (":" <+>) . pretty <$> (toMaybe t :: Maybe exprType) else Nothing
    pe = prettyExpr' (stripLocation @(ASTLocate ast (Expr' ast)) @(Expr' ast) e)
    long = pe <+> pretty te
    short = align (pretty pe <+> pretty te)

instance
  ( Pretty (ASTLocate ast (Select "ConRef" ast)),
    Pretty (ASTLocate ast (Select "VarRef" ast)),
    (Pretty (Select "InParens" ast)),
    (Pretty (ASTLocate ast (Select "LetParamName" ast))),
    Pretty letPatterns,
    letPatterns ~ UnwrapList (Select "LetPattern" ast),
    (ToList (Select "LetPattern" ast) [letPatterns]),
    Pretty lambdaPatterns,
    lambdaPatterns ~ UnwrapList (Select "LambdaPattern" ast),
    (ToList (ASTLocate ast (Select "LambdaPattern" ast)) [lambdaPatterns]),
    (Pretty (ASTLocate ast (BinaryOperator' ast))),
    (ToMaybe (Select "ExprType" ast) (Maybe (UnwrapMaybe (Select "ExprType" ast)))),
    (ToMaybe (Select "PatternType" ast) (Maybe (UnwrapMaybe (Select "PatternType" ast)))),
    (Pretty (UnwrapMaybe (Select "ExprType" ast))),
    (Pretty (UnwrapMaybe (Select "PatternType" ast))),
    (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast)))),
    (StripLocation (CleanupLocated (ASTLocate' ast (Expr' ast))) (Expr' ast)),
    (Pretty (Select "ExprType" ast)),
    (DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast))
  ) =>
  Pretty (Expr' ast)
  where
  pretty e =
    let ?contextFree = True
        ?withType = True
     in prettyExpr' @ast @letPatterns @lambdaPatterns e

prettyExpr' ::
  forall ast letPatterns lambdaPatterns.
  ( lambdaPatterns ~ UnwrapList (Select "LambdaPattern" ast),
    letPatterns ~ UnwrapList (Select "LetPattern" ast),
    ?contextFree :: Bool,
    ?withType :: Bool,
    Pretty (ASTLocate ast (Select "ConRef" ast)),
    Pretty (ASTLocate ast (Select "VarRef" ast)),
    (Pretty (Select "InParens" ast)),
    (Pretty (ASTLocate ast (Select "LetParamName" ast))),
    Pretty letPatterns,
    (ToList (Select "LetPattern" ast) [letPatterns]),
    Pretty lambdaPatterns,
    (ToList (ASTLocate ast (Select "LambdaPattern" ast)) [lambdaPatterns]),
    (Pretty (ASTLocate ast (BinaryOperator' ast))),
    (ToMaybe (Select "ExprType" ast) (Maybe (UnwrapMaybe (Select "ExprType" ast)))),
    (ToMaybe (Select "PatternType" ast) (Maybe (UnwrapMaybe (Select "PatternType" ast)))),
    (Pretty (UnwrapMaybe (Select "ExprType" ast))),
    (Pretty (UnwrapMaybe (Select "PatternType" ast))),
    (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast)))),
    (StripLocation (ASTLocate ast (Expr' ast)) (Expr' ast)),
    (Pretty (Select "ExprType" ast)),
    (DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast))
  ) =>
  Expr' ast ->
  Doc AnsiStyle
prettyExpr' (Int i) = pretty i
prettyExpr' (Float f) = pretty f
prettyExpr' (String s) = pretty '\"' <> pretty s <> pretty '\"'
prettyExpr' (Char c) = "'" <> escapeChar c <> "'"
prettyExpr' Unit = "()"
prettyExpr' (Var v) = pretty v
prettyExpr' (Constructor c) = pretty c
prettyExpr' (Lambda ps e) = prettyLambdaExpr (fieldToList @(ASTLocate ast (Select "LambdaPattern" ast)) ps :: [lambdaPatterns]) (prettyExpr e)
prettyExpr' (FunctionCall e1 e2) = prettyFunctionCallExpr (prettyExpr e1) (prettyExpr e2)
prettyExpr' (TypeApplication e1 e2) = prettyFunctionCallExpr (prettyExpr e1) ("@" <> pretty e2)
prettyExpr' (If e1 e2 e3) = prettyIfExpr (prettyExpr e1) (prettyExpr e2) (prettyExpr e3)
prettyExpr' (List l) = prettyList (prettyExpr <$> l)
prettyExpr' (Match e m) = prettyMatchExpr (prettyExpr e) (prettyMatchBranch . second prettyExpr <$> m)
prettyExpr' (LetIn v p e1 e2) = prettyLetInExpr v (fieldToList @(Select "LetPattern" ast) p :: [letPatterns]) (prettyExpr e1) (prettyExpr e2)
prettyExpr' (Let v p e) = prettyLetExpr v (fieldToList @(Select "LetPattern" ast) p :: [letPatterns]) (prettyExpr e)
prettyExpr' (Block b) = prettyBlockExpr (prettyExpr <$> b)
prettyExpr' (InParens e) = parens (pretty e)
prettyExpr' (Tuple t) = prettyTupleExpr (prettyExpr <$> t)
prettyExpr' (BinaryOperator b) =
  let (op, e1, e2) = dataConAs @(Select "BinaryOperator" ast) @(BinaryOperator ast, Expr ast, Expr ast) b
   in prettyBinaryOperatorExpr (prettyExpr e1) op (prettyExpr e2)

instance
  ( Pretty a1,
    ToMaybe (Select "PatternType" ast) (Maybe a1),
    a1 ~ UnwrapMaybe (Select "PatternType" ast),
    (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
  ) =>
  Pretty (Pattern ast)
  where
  pretty (Pattern (p, t)) = group (flatAlt long short)
    where
      te = (":" <+>) . pretty <$> (toMaybe t :: Maybe a1)
      long = pretty p <+> pretty te
      short = align (pretty p <+> pretty te)

instance
  ( Pretty (CleanupLocated (ASTLocate' ast (Select "VarPat" ast))),
    Pretty (CleanupLocated (ASTLocate' ast (Select "ConPat" ast))),
    (ToMaybe (Select "PatternType" ast) (Maybe (UnwrapMaybe (Select "PatternType" ast)))),
    (Pretty (UnwrapMaybe (Select "PatternType" ast))),
    (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
  ) =>
  Pretty (Pattern' ast)
  where
  pretty = let ?contextFree = True in prettyPattern

prettyPattern ::
  forall ast.
  (_) =>
  (?contextFree :: Bool) =>
  Pattern' ast ->
  Doc AnsiStyle
prettyPattern (VarPattern v) = pretty v
prettyPattern (ConstructorPattern c ps) = prettyConstructorPattern c ps
prettyPattern (ListPattern l) = prettyList l
prettyPattern (ConsPattern p1 p2) = prettyConsPattern p1 p2
prettyPattern WildcardPattern = "_"
prettyPattern (IntegerPattern i) = pretty i
prettyPattern (FloatPattern f) = pretty f
prettyPattern (StringPattern s) = pretty '\"' <> pretty s <> pretty '\"'
prettyPattern (CharPattern c) = "'" <> escapeChar c <> "'"
prettyPattern UnitPattern = "()"

instance
  ( Pretty (ASTLocate ast (Type' ast)),
    Pretty (ASTLocate ast LowerAlphaName),
    Pretty (ASTLocate ast (Select "TypeVar" ast)),
    Pretty (ASTLocate ast (Select "UserDefinedType" ast))
  ) =>
  Pretty (Type' ast)
  where
  pretty = \case
    TypeVar name -> pretty name
    FunctionType a b -> parens (pretty a <+> "->" <+> pretty b)
    UnitType -> "()"
    TypeConstructorApplication a b -> pretty a <+> pretty b
    UserDefinedType name -> pretty name
    RecordType fields -> "{" <+> prettyFields fields <+> "}"
    TupleType fields -> tupled (map pretty (toList fields))
    ListType a -> "[" <+> pretty a <+> "]"
    where
      prettyFields = hsep . punctuate "," . map (\(name, value) -> pretty name <+> ":" <+> pretty value) . toList

instance
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( ASTLocate' ast1 ~ Located,
    ASTLocate' ast2 ~ Unlocated,
    (StripLocation (Select "LambdaPattern" ast1) (Select "LambdaPattern" ast2)),
    (StripLocation (Select "LetPattern" ast1) (Select "LetPattern" ast2)),
    (ApplyAsFunctorish (Select "ExprType" ast1) (Select "ExprType" ast2) (Type ast1) (Type ast2)),
    (ApplyAsFunctorish (Select "PatternType" ast1) (Select "PatternType" ast2) (Type ast1) (Type ast2)),
    ( StripLocation
        (Select "Infixed" ast1)
        (Select "Infixed" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "SymOp" ast1)))
        (Select "SymOp" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "TypeVar" ast1)))
        (Select "TypeVar" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "VarRef" ast1)))
        (Select "VarRef" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "VarPat" ast1)))
        (Select "VarPat" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "ConPat" ast1)))
        (Select "ConPat" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "ConRef" ast1)))
        (Select "ConRef" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "LambdaPattern" ast1)))
        (Select "LambdaPattern" ast1)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "LetParamName" ast1)))
        (Select "LetParamName" ast2)
    ),
    ( DataConAs
        (Select "BinaryOperator" ast1)
        (BinaryOperator ast1, Expr ast1, Expr ast1)
    ),
    ( DataConAs
        (Select "BinaryOperator" ast2)
        (BinaryOperator ast2, Expr ast2, Expr ast2)
    ),
    (DataConAs (Select "InParens" ast1) (Expr ast1)),
    (DataConAs (Select "InParens" ast2) (Expr ast2))
  ) =>
  StripLocation (Expr ast1) (Expr ast2)
  where
  stripLocation = stripExprLocation @ast1 @ast2

stripExprLocation ::
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( ASTLocate' ast1 ~ Located,
    ASTLocate' ast2 ~ Unlocated,
    StripLocation (Select "LambdaPattern" ast1) (Select "LambdaPattern" ast2),
    StripLocation (Select "LetPattern" ast1) (Select "LetPattern" ast2),
    ApplyAsFunctorish (Select "ExprType" ast1) (Select "ExprType" ast2) (Type ast1) (Type ast2),
    ( DataConAs
        (Select "BinaryOperator" ast1)
        (BinaryOperator ast1, Expr ast1, Expr ast1)
    ),
    _
  ) =>
  Expr ast1 ->
  Expr ast2
stripExprLocation (Expr (e :: ASTLocate ast1 (Expr' ast1), t)) =
  let e' = fmapUnlocated @LocatedAST @ast1 stripExprLocation' e
   in Expr
        ( stripLocation e',
          applyAsFunctorish @(Select "ExprType" ast1) @(Select "ExprType" ast2) @(Type ast1) @(Type ast2)
            stripTypeLocation
            t
        )
  where
    stripExprLocation' :: Expr' ast1 -> Expr' ast2
    stripExprLocation' (Int i) = Int i
    stripExprLocation' (Float f) = Float f
    stripExprLocation' (String s) = String s
    stripExprLocation' (Char c) = Char c
    stripExprLocation' Unit = Unit
    stripExprLocation' (Var v) = Var (stripLocation v)
    stripExprLocation' (Constructor c) = Constructor (stripLocation c)
    stripExprLocation' (Lambda ps e) =
      let ps' = stripLocation ps
          ps'' =
            stripLocation @(Select "LambdaPattern" ast1) @(Select "LambdaPattern" ast2)
              ps'
       in Lambda ps'' (stripExprLocation e)
    stripExprLocation' (FunctionCall e1 e2) = FunctionCall (stripExprLocation e1) (stripExprLocation e2)
    stripExprLocation' (TypeApplication e1 e2) =
      TypeApplication
        (stripExprLocation e1)
        (applyAsFunctorish @(Select "ExprType" ast1) @(Select "ExprType" ast2) @(Type ast1) @(Type ast2) stripTypeLocation e2)
    stripExprLocation' (If e1 e2 e3) = If (stripExprLocation e1) (stripExprLocation e2) (stripExprLocation e3)
    stripExprLocation' (BinaryOperator b) =
      let (op, e1, e2) = dataConAs @(Select "BinaryOperator" ast1) @(BinaryOperator ast1, Expr ast1, Expr ast1) b
       in BinaryOperator $ asDataCon (stripBinaryOperatorLocation @ast1 @ast2 op, stripExprLocation e1, stripExprLocation e2)
    stripExprLocation' (List l) = List (stripExprLocation <$> l)
    stripExprLocation' (Match e m) = Match (stripExprLocation e) (bimapF stripPatternLocation stripExprLocation m)
    stripExprLocation' (LetIn v p e1 e2) =
      let p' = stripLocation @(Select "LetPattern" ast1) @(Select "LetPattern" ast2) p
       in LetIn
            (stripLocation v)
            p'
            (stripExprLocation e1)
            (stripExprLocation e2)
    stripExprLocation' (Let v p e) =
      let p' = stripLocation @(Select "LetPattern" ast1) @(Select "LetPattern" ast2) p
       in Let (stripLocation v) p' (stripExprLocation e)
    stripExprLocation' (Block b) = Block (stripExprLocation <$> b)
    stripExprLocation' (InParens e) =
      let e' = dataConAs @(Select "InParens" ast1) @(Expr ast1) e
       in InParens $ asDataCon (stripExprLocation e')
    stripExprLocation' (Tuple t) = Tuple (stripExprLocation <$> t)

instance
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( ASTLocate' ast1 ~ Located,
    ASTLocate' ast2 ~ Unlocated,
    ( ApplyAsFunctorish
        (Select "PatternType" ast1)
        (Select "PatternType" ast2)
        (Type ast1)
        (Type ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "TypeVar" ast1)))
        (Select "TypeVar" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "VarPat" ast1)))
        (Select "VarPat" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "ConPat" ast1)))
        (Select "ConPat" ast2)
    )
  ) =>
  StripLocation (Pattern ast1) (Pattern ast2)
  where
  stripLocation = stripPatternLocation @ast1 @ast2

stripPatternLocation ::
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( ASTLocate' ast1 ~ Located,
    ASTLocate' ast2 ~ Unlocated,
    _
  ) =>
  Pattern ast1 ->
  Pattern ast2
stripPatternLocation (Pattern (p :: ASTLocate ast1 (Pattern' ast1), t)) =
  let p' = fmapUnlocated @LocatedAST @ast1 stripPatternLocation' p
   in Pattern
        ( stripLocation p',
          applyAsFunctorish @(Select "PatternType" ast1) @(Select "PatternType" ast2) @(Type ast1) @(Type ast2) stripTypeLocation t
        )
  where
    stripPatternLocation' :: Pattern' ast1 -> Pattern' ast2
    stripPatternLocation' (VarPattern v) = VarPattern (stripLocation v)
    stripPatternLocation' (ConstructorPattern c ps) = ConstructorPattern (stripLocation c) (stripPatternLocation <$> ps)
    stripPatternLocation' (ListPattern l) = ListPattern (stripPatternLocation <$> l)
    stripPatternLocation' (ConsPattern p1 p2) = ConsPattern (stripPatternLocation p1) (stripPatternLocation p2)
    stripPatternLocation' WildcardPattern = WildcardPattern
    stripPatternLocation' (IntegerPattern i) = IntegerPattern i
    stripPatternLocation' (FloatPattern f) = FloatPattern f
    stripPatternLocation' (StringPattern s) = StringPattern s
    stripPatternLocation' (CharPattern c) = CharPattern c
    stripPatternLocation' UnitPattern = UnitPattern

instance
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( ASTLocate' ast1 ~ Located,
    ASTLocate' ast2 ~ Unlocated,
    ( StripLocation
        (Select "Infixed" ast1)
        (Select "Infixed" ast2)
    ),
    ( StripLocation
        (CleanupLocated (Located (Select "SymOp" ast1)))
        (Select "SymOp" ast2)
    )
  ) =>
  StripLocation (BinaryOperator ast1) (BinaryOperator ast2)
  where
  stripLocation = stripBinaryOperatorLocation @ast1 @ast2

stripBinaryOperatorLocation ::
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( (ASTLocate' ast1 ~ Located),
    ASTLocate' ast2 ~ Unlocated,
    StripLocation (ASTLocate ast1 (BinaryOperator' ast2)) (BinaryOperator' ast2),
    _
  ) =>
  BinaryOperator ast1 ->
  BinaryOperator ast2
stripBinaryOperatorLocation (MkBinaryOperator (op :: ASTLocate ast1 (BinaryOperator' ast1))) =
  let op' = fmapUnlocated @LocatedAST @ast1 stripBinaryOperatorLocation' op
   in MkBinaryOperator (stripLocation op' :: BinaryOperator' ast2)
  where
    stripBinaryOperatorLocation' :: BinaryOperator' ast1 -> BinaryOperator' ast2
    stripBinaryOperatorLocation' (SymOp name) = SymOp (stripLocation name)
    stripBinaryOperatorLocation' (Infixed name) = Infixed (stripLocation name)

stripTypeLocation ::
  forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
  ( (ASTLocate' ast1 ~ Located),
    ASTLocate' ast2 ~ Unlocated,
    _
  ) =>
  Type ast1 ->
  Type ast2
stripTypeLocation (Type (t :: ASTLocate ast1 (Type' ast1))) =
  let t' = fmapUnlocated @LocatedAST @ast1 stripTypeLocation' t
   in Type (stripLocation @(ASTLocate ast1 (Type' ast2)) @(Type' ast2) t')
  where
    stripTypeLocation' :: Type' ast1 -> Type' ast2
    stripTypeLocation' (TypeVar name) = TypeVar (stripLocation name)
    stripTypeLocation' (FunctionType a b) = FunctionType (stripTypeLocation a) (stripTypeLocation b)
    stripTypeLocation' UnitType = UnitType
    stripTypeLocation' (TypeConstructorApplication a b) = TypeConstructorApplication (stripTypeLocation a) (stripTypeLocation b)

-- stripTypeLocation' (UserDefinedType name) = UserDefinedType (_ name)

{-  =====================
    Messy deriving stuff
    ====================
-}

type ForAllExpr :: (Kind.Type -> Kind.Constraint) -> ast -> Kind.Constraint
type ForAllExpr c ast =
  ( c (Expr ast),
    c (ASTLocate ast (Select "VarRef" ast)),
    c (ASTLocate ast (Select "LambdaPattern" ast)),
    c (ASTLocate ast (Select "ConRef" ast)),
    c (ASTLocate ast (Select "LetParamName" ast))
  )

-- Eq instances

deriving instance
  ( (Eq (Select "LetPattern" ast)),
    ForAllExpr Eq ast,
    (Eq (ASTLocate ast (BinaryOperator' ast))),
    (Eq (Select "InParens" ast)),
    (Eq (Select "ExprType" ast)),
    (Eq (Select "PatternType" ast)),
    (Eq (Select "BinaryOperator" ast)),
    Eq (ASTLocate ast (Expr' ast)),
    Eq (ASTLocate ast (Pattern' ast))
  ) =>
  Eq (Expr' ast)

deriving instance (Eq (ASTLocate ast (Expr' ast)), Eq (Select "ExprType" ast)) => Eq (Expr ast)

deriving instance
  ( Eq (ASTLocate ast (Select "VarPat" ast)),
    Eq (ASTLocate ast (Select "ConPat" ast)),
    (Eq (Select "PatternType" ast)),
    Eq (ASTLocate ast (Pattern' ast))
  ) =>
  Eq (Pattern' ast)

deriving instance (Eq (ASTLocate ast (Pattern' ast)), Eq (Select "PatternType" ast)) => Eq (Pattern ast)

deriving instance
  ( Eq (ASTLocate ast (Select "TypeVar" ast)),
    Eq (ASTLocate ast (Select "UserDefinedType" ast)),
    Eq (ASTLocate ast (Type' ast)),
    Eq (ASTLocate ast LowerAlphaName),
    Eq (Type ast)
  ) =>
  Eq (Type' ast)

deriving instance (Eq (ASTLocate ast (Type' ast))) => Eq (Type ast)

deriving instance
  ( Eq (ASTLocate ast (Select "SymOp" ast)),
    Eq (Select "Infixed" ast)
  ) =>
  Eq (BinaryOperator' ast)

deriving instance (Eq (ASTLocate ast (BinaryOperator' ast))) => Eq (BinaryOperator ast)

-- Show instances

deriving instance
  ( (Show (Select "LetPattern" ast)),
    (Show (ASTLocate ast (Select "VarRef" ast))),
    (Show (ASTLocate ast (Select "LambdaPattern" ast))),
    (Show (ASTLocate ast (Select "ConRef" ast))),
    (Show (ASTLocate ast (Select "LetParamName" ast))),
    (Show (ASTLocate ast (BinaryOperator' ast))),
    (Show (Select "InParens" ast)),
    (Show (Select "ExprType" ast)),
    (Show (Select "PatternType" ast)),
    Show (Select "BinaryOperator" ast),
    Show (ASTLocate ast (Expr' ast)),
    Show (ASTLocate ast (Pattern' ast))
  ) =>
  Show (Expr' ast)

deriving instance (Show (ASTLocate ast (Expr' ast)), Show (Select "ExprType" ast)) => Show (Expr ast)

deriving instance
  ( Show (ASTLocate ast (Select "VarPat" ast)),
    Show (ASTLocate ast (Select "ConPat" ast)),
    (Show (Select "PatternType" ast)),
    Show (ASTLocate ast (Pattern' ast))
  ) =>
  Show (Pattern' ast)

deriving instance (Show (ASTLocate ast (Pattern' ast)), Show (Select "PatternType" ast)) => Show (Pattern ast)

deriving instance
  ( Show (ASTLocate ast (Select "TypeVar" ast)),
    Show (ASTLocate ast (Select "UserDefinedType" ast)),
    Show (ASTLocate ast (Type' ast)),
    Show (ASTLocate ast LowerAlphaName),
    Show (Type ast)
  ) =>
  Show (Type' ast)

deriving instance (Show (ASTLocate ast (Type' ast))) => Show (Type ast)

deriving instance
  ( Show (ASTLocate ast (Select "SymOp" ast)),
    Show (Select "Infixed" ast)
  ) =>
  Show (BinaryOperator' ast)

deriving instance (Show (ASTLocate ast (BinaryOperator' ast))) => Show (BinaryOperator ast)

deriving instance
  ( Show (DeclarationBody ast),
    Show (ASTLocate ast (Select "DeclarationName" ast)),
    Show (ASTLocate ast ModuleName)
  ) =>
  Show (Declaration' ast)

deriving instance (Show (ASTLocate ast (Declaration' ast))) => Show (Declaration ast)

deriving instance
  ( (Show (Select "ValueTypeDef" ast)),
    (Show (Select "ValuePatterns" ast)),
    (Show (Select "ValueType" ast)),
    Show (Select "ExprType" ast),
    Show (ASTLocate ast (Select "TypeVar" ast)),
    Show (ASTLocate ast (Expr' ast)),
    Show (ASTLocate ast (TypeDeclaration ast))
  ) =>
  Show (DeclarationBody' ast)

deriving instance (Show (ASTLocate ast (DeclarationBody' ast))) => Show (DeclarationBody ast)

deriving instance
  ( Show (ASTLocate ast (Select "ConstructorName" ast)),
    Show (Type ast)
  ) =>
  Show (TypeDeclaration ast)

-- Ord instances

deriving newtype instance (Ord (ASTLocate ast (BinaryOperator' ast))) => Ord (BinaryOperator ast)

deriving instance
  ( Ord (ASTLocate ast (Select "SymOp" ast)),
    Ord (Select "Infixed" ast)
  ) =>
  Ord (BinaryOperator' ast)

-- Data instances

deriving instance
  forall a (ast :: a).
  ( Data (ASTLocate ast (Pattern' ast)),
    Data (Select "PatternType" ast),
    Typeable ast,
    Typeable a
  ) =>
  Data (Pattern ast)

deriving instance
  forall a (ast :: a).
  ( Typeable a,
    Typeable ast,
    (Data (Pattern ast)),
    (Data (ASTLocate ast (Select "VarPat" ast))),
    (Data (ASTLocate ast (Select "ConPat" ast)))
  ) =>
  Data (Pattern' ast)

deriving instance
  forall a (ast :: a).
  ( Data (ASTLocate ast (Expr' ast)),
    Data (Select "ExprType" ast),
    Typeable ast,
    Typeable a
  ) =>
  Data (Expr ast)

deriving instance
  forall a (ast :: a).
  ( Data (ASTLocate ast (Expr' ast)),
    Data ((Select "LetPattern" ast)),
    Data ((Select "InParens" ast)),
    Data ((Select "PatternType" ast)),
    Data ((Select "BinaryOperator" ast)),
    (Data (Select "ExprType" ast)),
    Data (ASTLocate ast (Select "VarRef" ast)),
    Data (ASTLocate ast (Select "ConRef" ast)),
    Data (ASTLocate ast (Select "LetParamName" ast)),
    Data (ASTLocate ast (Select "LambdaPattern" ast)),
    Data (ASTLocate ast (Pattern' ast)),
    Typeable ast,
    Typeable a
  ) =>
  Data (Expr' ast)

deriving instance
  forall a (ast :: a).
  ( Data (ASTLocate ast (Type' ast)),
    Data (Select "TypeVar" ast),
    Data (Select "UserDefinedType" ast),
    Typeable ast,
    Typeable a
  ) =>
  Data (Type ast)

deriving instance
  forall a (ast :: a).
  ( Data (ASTLocate ast (Type' ast)),
    Data (ASTLocate ast (Select "TypeVar" ast)),
    Data ((Select "TypeVar" ast)),
    Data (ASTLocate ast (Select "UserDefinedType" ast)),
    Data (ASTLocate ast LowerAlphaName),
    Data ((Select "UserDefinedType" ast)),
    Typeable ast,
    Typeable a
  ) =>
  Data (Type' ast)

-- Some of these 'Plated' instances could be derived with 'template', but I feel like it's more efficient to write them by hand

instance
  ( RUnlocate ast
  ) =>
  Plated (Pattern' ast)
  where
  plate f = \case
    p@(VarPattern _) -> pure p
    ConstructorPattern a b -> ConstructorPattern a <$> traverseOf (each . _Unwrapped . _1 . traverseUnlocated @_ @ast) f b
    ListPattern a -> ListPattern <$> traverseOf (each . _Unwrapped . _1 . traverseUnlocated @_ @ast) f a
    ConsPattern a b -> ConsPattern <$> traverseOf (_Unwrapped . _1 . traverseUnlocated @_ @ast) f a <*> traverseOf (_Unwrapped . _1 . traverseUnlocated @_ @ast) f b
    WildcardPattern -> pure WildcardPattern
    IntegerPattern a -> pure (IntegerPattern a)
    FloatPattern a -> pure (FloatPattern a)
    StringPattern a -> pure (StringPattern a)
    CharPattern a -> pure (CharPattern a)
    UnitPattern -> pure UnitPattern

instance
  forall a (ast :: a).
  ( (Data (Select "PatternType" ast)),
    (Data ((ASTLocate ast (Pattern' ast)))),
    Typeable ast,
    Typeable a
  ) =>
  Plated (Pattern ast)
  where
  plate = template

instance
  ( RUnlocate ast,
    (DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast)),
    (DataConAs (Select "InParens" ast) (Expr ast))
  ) =>
  Plated (Expr' ast)
  where
  plate f =
    let traverseExpr = (_Unwrapped . _1 . traverseUnlocated @_ @ast)
     in \case
          Int i -> pure (Int i)
          Float f -> pure (Float f)
          String s -> pure (String s)
          Char c -> pure (Char c)
          Unit -> pure Unit
          Var v -> pure (Var v)
          Constructor c -> pure (Constructor c)
          Lambda ps e -> (Lambda ps <$> traverseOf traverseExpr f e)
          FunctionCall e1 e2 -> FunctionCall <$> traverseOf traverseExpr f e1 <*> traverseOf traverseExpr f e2
          TypeApplication e1 e2 -> TypeApplication <$> traverseOf traverseExpr f e1 <*> pure e2
          If e1 e2 e3 -> If <$> traverseOf traverseExpr f e1 <*> traverseOf traverseExpr f e2 <*> traverseOf traverseExpr f e3
          List l -> List <$> traverseOf (each . traverseExpr) f l
          Match e m -> Match <$> traverseOf traverseExpr f e <*> traverseOf (each . _2 . traverseExpr) (f) m
          LetIn v p e1 e2 -> (LetIn v p <$> traverseOf traverseExpr f e1) <*> traverseOf traverseExpr f e2
          Let v p e -> (Let v p <$> traverseOf traverseExpr f e)
          Block b -> Block <$> traverseOf (each . traverseExpr) f b
          InParens e ->
            let e' = dataConAs @(Select "InParens" ast) @(Expr ast) e
             in InParens . asDataCon <$> traverseOf traverseExpr f e'
          Tuple t -> Tuple <$> traverseOf (each . traverseExpr) f t
          BinaryOperator b ->
            let (op, e1, e2) = dataConAs @(Select "BinaryOperator" ast) @(BinaryOperator ast, Expr ast, Expr ast) b
             in BinaryOperator . asDataCon <$> (((,,) op <$> traverseOf traverseExpr f e1) <*> traverseOf traverseExpr f e2)

instance
  forall a (ast :: a).
  ( (Data (Select "ExprType" ast)),
    (Data ((ASTLocate ast (Expr' ast)))),
    Typeable ast,
    Typeable a
  ) =>
  Plated (Expr ast)
  where
  plate = template

instance
  forall a (ast :: a).
  ( (Data (Type ast))
  ) =>
  Plated (Type ast)
  where
  plate = template

instance
  forall a (ast :: a).
  ( Data (ASTLocate ast (Type' ast)),
    Data (ASTLocate ast (Select "TypeVar" ast)),
    Data ((Select "TypeVar" ast)),
    Data (ASTLocate ast (Select "UserDefinedType" ast)),
    Data (ASTLocate ast LowerAlphaName),
    Data ((Select "UserDefinedType" ast)),
    Typeable ast,
    Typeable a
  ) =>
  Plated (Type' ast)
