{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Elara.AST.Generic where

-- import Elara.AST.Frontend qualified as Frontend

import Control.Lens (to, view, (^.))
import Data.Data (Data)
import Data.Generics.Wrapped
import Data.Kind qualified as Kind
import Elara.AST.Name (ModuleName, Name, Qualified, VarName (..))
import Elara.AST.Pretty
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select (AST (..), LocatedAST, UnlocatedAST)
import Elara.AST.StripLocation (StripLocation (..))
import Elara.Data.Pretty
import GHC.Generics (
    C,
    D,
    Generic (Rep),
    M1,
    Meta (MetaCons),
    type (:+:),
 )
import GHC.TypeLits
import TODO (todo)
import Prelude hiding (group)

data DataConCantHappen deriving (Generic, Data)

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

data NoFieldValue = NoFieldValue
    deriving (Generic, Data)

instance Pretty NoFieldValue where
    pretty :: HasCallStack => NoFieldValue -> Doc AnsiStyle
    pretty _ = error "This instance should never be used"

{- | Used to select a field type for a given AST.

Conventions for usage:
If a selection is likely to be one of the "principal" newtypes ('Expr', 'Pattern', etc), it should not be wrapped in 'ASTLocate',
as this increases friction and creates redundant 'Located' wrappers.
This means that implementations should manually wrap in 'Locate' if not using one of the principle newtypes
-}
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
    | If (Expr ast) (Expr ast) (Expr ast)
    | BinaryOperator (BinaryOperator ast) (Expr ast) (Expr ast)
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
    deriving (Generic)

data Pattern' (ast :: a)
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

newtype Pattern (ast :: a) = Pattern (ASTLocate ast (Pattern' ast), Select "PatternType" ast)
    deriving (Generic)

data BinaryOperator' (ast :: a)
    = SymOp (ASTLocate ast (Select "SymOp" ast))
    | Infixed (ASTLocate ast (Select "Infixed" ast))
    deriving (Generic)

newtype BinaryOperator ast = MkBinaryOperator (ASTLocate ast (BinaryOperator' ast))
    deriving (Generic)

newtype Declaration ast = Declaration (ASTLocate ast (Declaration' ast))
    deriving (Generic)

data Declaration' (ast :: a) = Declaration'
    { moduleName :: ASTLocate ast ModuleName
    , name :: ASTLocate ast (Select "DeclarationName" ast)
    , body :: DeclarationBody ast
    }
    deriving (Generic)

newtype DeclarationBody (ast :: a) = DeclarationBody (ASTLocate ast (DeclarationBody' ast))
    deriving (Generic)

data DeclarationBody' (ast :: a)
    = -- | let <p> = <e>
      Value
        { _expression :: Expr ast
        , _patterns :: Select "ValuePatterns" ast
        , _valueType :: Select "ValueType" ast
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
    | RecordType (NonEmpty (ASTLocate ast VarName, Type ast))
    | TupleType (NonEmpty (Type ast))
    | ListType (Type ast)
    deriving (Generic)

-- Ttg stuff

type RUnlocate :: ast -> Kind.Constraint
class RUnlocate ast where
    rUnlocate ::
        forall a.
        CleanupLocated (Located a) ~ Located a =>
        ASTLocate ast a ->
        a

    fmapUnlocated ::
        forall a b.
        (CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
        (a -> b) ->
        ASTLocate ast a ->
        ASTLocate ast b

instance (ASTLocate' ast ~ Located) => RUnlocate (ast :: LocatedAST) where
    rUnlocate = view unlocated
    fmapUnlocated = fmap

instance (ASTLocate' ast ~ Unlocated) => RUnlocate (ast :: UnlocatedAST) where
    rUnlocate = identity
    fmapUnlocated f = f

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

coerceTypeDeclaration :: _ => TypeDeclaration ast1 -> TypeDeclaration ast2
coerceTypeDeclaration (Alias a) = Alias (coerceType a)
coerceTypeDeclaration (ADT a) = ADT (fmap coerceType <<$>> a)

coerceType :: _ => Type ast1 -> Type ast2
coerceType (Type a) = Type (coerceType' <$> a)

coerceType' :: _ => Type' ast1 -> Type' ast2
coerceType' (TypeVar a) = TypeVar a
coerceType' (FunctionType a b) = FunctionType (coerceType a) (coerceType b)
coerceType' UnitType = UnitType
coerceType' (TypeConstructorApplication a b) = TypeConstructorApplication (coerceType a) (coerceType b)
coerceType' (UserDefinedType a) = UserDefinedType a
coerceType' (RecordType a) = RecordType (fmap coerceType <$> a)
coerceType' (TupleType a) = TupleType (coerceType <$> a)
coerceType' (ListType a) = ListType (coerceType a)

-- Pretty printing

deriving newtype instance Pretty (ASTLocate ast (BinaryOperator' ast)) => Pretty (BinaryOperator ast)

deriving newtype instance Pretty (ASTLocate ast (Type' ast)) => Pretty (Type ast)
instance
    ( Pretty (ASTLocate ast (Declaration' ast))
    ) =>
    Pretty (Declaration ast)
    where
    pretty (Declaration ldb) = pretty ldb

data UnknownPretty = forall a. Pretty a => UnknownPretty a

instance Pretty UnknownPretty where
    pretty (UnknownPretty a) = pretty a

instance
    ( Pretty (Expr ast)
    , Pretty (CleanupLocated (ASTLocate' ast (Select "TypeVar" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "DeclarationName" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (TypeDeclaration ast)))
    , Pretty valueType
    , ToMaybe (Select "ValueType" ast) (Maybe valueType)
    , valueType ~ UnwrapMaybe (Select "ValueType" ast)
    , Pretty exprType
    , exprType ~ UnwrapMaybe (Select "ExprType" ast)
    , (ToMaybe (Select "ExprType" ast) (Maybe exprType))
    , RUnlocate (ast :: b)
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
                    (UnknownPretty <$> (toMaybe t :: Maybe exprType)) -- Prioritise the type in the expression
                        <|> (UnknownPretty <$> (toMaybe t' :: Maybe valueType)) -- Otherwise, use the type in the declaration
             in prettyValueDeclaration n e typeOfE
        prettyDB n (TypeDeclaration vars t) = prettyTypeDeclaration n vars t

instance Pretty (TypeDeclaration ast) where
    pretty _ = "TODO"

{- | When fields may be optional, we need a way of representing that generally. This class does that.
 In short, it converts a type to a 'Maybe'. If the type is already a 'Maybe', it is left alone.
 If it is not, it is wrapped in a 'Just'. If it is 'NoFieldValue', it is converted to 'Nothing'.
-}
class ToMaybe i o where
    toMaybe :: i -> o

instance {-# OVERLAPPING #-} ToMaybe NoFieldValue (Maybe a) where
    toMaybe _ = Nothing

instance ToMaybe (Maybe a) (Maybe a) where
    toMaybe = identity

instance {-# INCOHERENT #-} ToMaybe a (Maybe a) where
    toMaybe = Just

-- | Unwraps 1 level of 'Maybe' from a type. Useful when a type family returns Maybe
type family UnwrapMaybe (a :: Kind.Type) = (k :: Kind.Type) where
    UnwrapMaybe (Maybe a) = a
    UnwrapMaybe a = a

instance
    ( Pretty (CleanupLocated (ASTLocate' ast (Expr' ast)))
    , Pretty a1
    , ToMaybe (Select "ExprType" ast) (Maybe a1)
    , a1 ~ UnwrapMaybe (Select "ExprType" ast)
    ) =>
    Pretty (Expr ast)
    where
    pretty (Expr (e, t)) = group (flatAlt long short)
      where
        te = (":" <+>) . pretty <$> (toMaybe t :: Maybe a1)
        long = pretty e <+> pretty te
        short = align (pretty e <+> pretty te)

instance
    ( Pretty (Expr ast)
    , Pretty (CleanupLocated (ASTLocate' ast (Select "LambdaPattern" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "ConRef" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "VarRef" ast)))
    , Pretty (Pattern ast)
    , (Pretty (Select "InParens" ast))
    , (Pretty (CleanupLocated (ASTLocate' ast (Select "LetParamName" ast))))
    , Pretty a2
    , a2 ~ UnwrapMaybe (Select "LetPattern" ast)
    , (ToMaybe (Select "LetPattern" ast) (Maybe a2))
    ) =>
    Pretty (Expr' ast)
    where
    pretty (Int i) = pretty i
    pretty (Float f) = pretty f
    pretty (String s) = pretty '\"' <> pretty s <> pretty '\"'
    pretty (Char c) = "'" <> escapeChar c <> "'"
    pretty Unit = "()"
    pretty (Var v) = pretty v
    pretty (Constructor c) = pretty c
    pretty (Lambda ps e) = prettyLambdaExpr [ps] e
    pretty (FunctionCall e1 e2) = prettyFunctionCallExpr e1 e2
    pretty (If e1 e2 e3) = prettyIfExpr e1 e2 e3
    pretty (List l) = prettyListExpr l
    pretty (Match e m) = prettyMatchExpr e (prettyMatchBranch <$> m)
    pretty (LetIn v p e1 e2) = prettyLetInExpr v (maybeToList $ toMaybe p :: [a2]) e1 (Just e2)
    pretty (Let v p e) = prettyLetExpr v (maybeToList $ toMaybe p :: [a2]) e
    pretty (Block b) = prettyBlockExpr b
    pretty (InParens e) = parens (pretty e)

instance
    ( Pretty a1
    , ToMaybe (Select "PatternType" ast) (Maybe a1)
    , a1 ~ UnwrapMaybe (Select "PatternType" ast)
    , (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
    ) =>
    Pretty (Pattern ast)
    where
    pretty (Pattern (p, t)) = group (flatAlt long short)
      where
        te = (":" <+>) . pretty <$> (toMaybe t :: Maybe a1)
        long = pretty p <+> pretty te
        short = align (pretty p <+> pretty te)

instance
    ( Pretty (CleanupLocated (ASTLocate' ast (Select "VarPat" ast)))
    , Pretty (Pattern ast)
    , Pretty (CleanupLocated (ASTLocate' ast (Select "ConPat" ast)))
    ) =>
    Pretty (Pattern' ast)
    where
    pretty (VarPattern v) = pretty v
    pretty (ConstructorPattern c ps) = prettyConstructorPattern c ps
    pretty (ListPattern l) = prettyListPattern l
    pretty (ConsPattern p1 p2) = prettyConsPattern p1 p2
    pretty other = error "aaaaaaa"

instance
    ( Pretty (ASTLocate ast (Type' ast))
    , Pretty (ASTLocate ast VarName)
    , Pretty (ASTLocate ast (Select "TypeVar" ast))
    , Pretty (ASTLocate ast (Select "UserDefinedType" ast))
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

stripExprLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ((ASTLocate' ast1 ~ Located), ASTLocate' ast2 ~ Unlocated) =>
    Expr ast1 ->
    Expr ast2
stripExprLocation (Expr (e :: ASTLocate ast1 (Expr' ast1), t)) =
    let e' = fmapUnlocated @LocatedAST @ast1 stripExprLocation' e
     in -- t' = fmapUnlocated @LocatedAST @ast1 _ t :: ASTLocate ast1 (Select "ExprType" ast2)
        Expr (stripLocation e', todo t)
  where
    stripExprLocation' :: forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST). Expr' ast1 -> Expr' ast2
    stripExprLocation' (Int i) = Int i
    stripExprLocation' (Float f) = Float f
    stripExprLocation' (String s) = String s
    stripExprLocation' (Char c) = Char c
    stripExprLocation' Unit = Unit
    stripExprLocation' (Var v) = Var (todo)

-- Messy deriving stuff

deriving instance
    ( (Eq (Select "LetPattern" ast))
    , (Eq (ASTLocate ast (Select "VarRef" ast)))
    , (Eq (ASTLocate ast (Select "LambdaPattern" ast)))
    , (Eq (ASTLocate ast (Select "ConRef" ast)))
    , (Eq (ASTLocate ast (Select "LetParamName" ast)))
    , (Eq (ASTLocate ast (BinaryOperator' ast)))
    , (Eq (Select "InParens" ast))
    , (Eq (Select "ExprType" ast))
    , (Eq (Select "PatternType" ast))
    , Eq (ASTLocate ast (Expr' ast))
    , Eq (ASTLocate ast (Pattern' ast))
    ) =>
    Eq (Expr' ast)

deriving instance (Eq (ASTLocate ast (Expr' ast)), Eq (Select "ExprType" ast)) => Eq (Expr ast)

deriving instance
    
    ( Eq (ASTLocate ast (Select "VarPat" ast))
    , Eq (ASTLocate ast (Select "ConPat" ast))
    , (Eq (Select "PatternType" ast))
    , Eq (ASTLocate ast (Pattern' ast))
    ) =>
    Eq (Pattern' ast)

deriving instance (Eq (ASTLocate ast (Pattern' ast)), Eq (Select "PatternType" ast)) => Eq (Pattern ast)

deriving instance
    ( Eq (ASTLocate ast (Select "TypeVar" ast))
    , Eq (ASTLocate ast (Select "UserDefinedType" ast))
    , Eq (ASTLocate ast (Type' ast))
    , Eq (ASTLocate ast VarName)
    , Eq (Type ast)
    ) =>
    Eq (Type' ast)

deriving instance (Eq (ASTLocate ast (Type' ast))) => Eq (Type ast)


deriving instance
    ( (Show (Select "LetPattern" ast))
    , (Show (ASTLocate ast (Select "VarRef" ast)))
    , (Show (ASTLocate ast (Select "LambdaPattern" ast)))
    , (Show (ASTLocate ast (Select "ConRef" ast)))
    , (Show (ASTLocate ast (Select "LetParamName" ast)))
    , (Show (ASTLocate ast (BinaryOperator' ast)))
    , (Show (Select "InParens" ast))
    , (Show (Select "ExprType" ast))
    , (Show (Select "PatternType" ast))
    , Show (ASTLocate ast (Expr' ast))
    , Show (ASTLocate ast (Pattern' ast))
    ) =>
    Show (Expr' ast)

deriving instance (Show (ASTLocate ast (Expr' ast)), Show (Select "ExprType" ast)) => Show (Expr ast)

deriving instance
    
    ( Show (ASTLocate ast (Select "VarPat" ast))
    , Show (ASTLocate ast (Select "ConPat" ast))
    , (Show (Select "PatternType" ast))
    , Show (ASTLocate ast (Pattern' ast))
    ) =>
    Show (Pattern' ast)

deriving instance (Show (ASTLocate ast (Pattern' ast)), Show (Select "PatternType" ast)) => Show (Pattern ast)

deriving instance
    ( Show (ASTLocate ast (Select "TypeVar" ast))
    , Show (ASTLocate ast (Select "UserDefinedType" ast))
    , Show (ASTLocate ast (Type' ast))
    , Show (ASTLocate ast VarName)
    , Show (Type ast)
    ) =>
    Show (Type' ast)

deriving instance (Show (ASTLocate ast (Type' ast))) => Show (Type ast)

deriving newtype instance Eq (ASTLocate ast (BinaryOperator' ast)) => Eq (BinaryOperator ast)
deriving newtype instance Ord (ASTLocate ast (BinaryOperator' ast)) => Ord (BinaryOperator ast)
deriving newtype instance Show (ASTLocate ast (BinaryOperator' ast)) => Show (BinaryOperator ast)

deriving instance
    ( Eq (ASTLocate ast (Select "SymOp" ast))
    , Eq (ASTLocate ast (Select "Infixed" ast))
    ) =>
    Eq (BinaryOperator' ast)

deriving instance
    ( Ord (ASTLocate ast (Select "SymOp" ast))
    , Ord (ASTLocate ast (Select "Infixed" ast))
    ) =>
    Ord (BinaryOperator' ast)

deriving instance
    ( Show (ASTLocate ast (Select "SymOp" ast))
    , Show (ASTLocate ast (Select "Infixed" ast))
    ) =>
    Show (BinaryOperator' ast)


