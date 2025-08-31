module Elara.TypeInfer.Convert where

import Elara.AST.Generic.Types (freeTypeVars)
import Elara.AST.Generic.Types qualified as Generic
import Elara.AST.Kinded
import Elara.AST.Name
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Shunted (ShuntedType, ShuntedType')
import Elara.Data.Kind
import Elara.Data.Pretty
import Elara.Error (ReportableError (..))
import Elara.Prim (boolName, charName, intName, mkPrimQual, stringName)
import Elara.TypeInfer.Type
import Polysemy
import Polysemy.Error

astTypeToGeneralisedInferType :: Member (Error TypeConvertError) r => KindedType -> Sem r (Type SourceRegion)
astTypeToGeneralisedInferType t@(Generic.Type (Located loc t', kind)) = do
    let ftvs = freeTypeVars t
    let skolems = fmap convertTyVar ftvs
    asInferType <- astTypeToInferType' loc t'

    case skolems of
        [] -> pure $ Lifted asInferType
        _ -> pure $ Polytype (Forall skolems EmptyConstraint asInferType)

astTypeToInferType :: Member (Error TypeConvertError) r => KindedType -> Sem r (Monotype SourceRegion)
astTypeToInferType t@(Generic.Type (Located loc t', kind)) = do
    asInferType <- astTypeToInferType' loc t'
    pure asInferType

astTypeToInferTypeWithKind :: Member (Error TypeConvertError) r => KindedType -> Sem r (Monotype SourceRegion, ElaraKind)
astTypeToInferTypeWithKind t@(Generic.Type (Located loc t', kind)) = do
    asInferType <- astTypeToInferType' loc t'
    pure (asInferType, kind)

convertTyVar name = fmap (Just . nameText) (name ^. unlocated)

astTypeToInferType' :: Member (Error TypeConvertError) r => SourceRegion -> KindedType' -> Sem r (Monotype SourceRegion)
astTypeToInferType' loc (Generic.TypeVar name) = do
    pure $ TypeVar $ UnificationVar $ convertTyVar name -- idk if this should ever be a skolem variable? i dont think so
astTypeToInferType' loc (Generic.FunctionType i o) = do
    i' <- astTypeToInferType i
    o' <- astTypeToInferType o
    pure $ Function i' o'
astTypeToInferType' loc (Generic.UnitType) = do
    pure $ Scalar ScalarUnit
astTypeToInferType' loc (Generic.TupleType ts) = do
    ts' <- traverse (astTypeToInferType) ts
    throw $ NotSupported "Tuple types are not supported yet"
astTypeToInferType' loc (Generic.ListType t) = do
    t' <- astTypeToInferType t
    throw $ NotSupported "List types are not supported yet"
astTypeToInferType' loc (Generic.RecordType fields) = do
    throw $ NotSupported "Record types are not supported yet"
astTypeToInferType' loc (Generic.TypeConstructorApplication ctor arg) = do
    ctor' <- astTypeToInferType ctor
    arg' <- astTypeToInferType arg
    case ctor' of
        TypeConstructor name args -> do
            pure $ TypeConstructor name (args ++ [arg'])
        other -> throw $ NotSupported "Type constructor application is only supported for type constructors"
-- primitive types
-- this will be removed soon as we remove primitives from the typechecker
astTypeToInferType' loc (Generic.UserDefinedType (Located _ name)) | name == mkPrimQual stringName = do
    pure $ Scalar ScalarString
astTypeToInferType' loc (Generic.UserDefinedType (Located _ name)) | name == mkPrimQual intName = do
    pure $ Scalar ScalarInt
astTypeToInferType' loc (Generic.UserDefinedType (Located _ name)) | name == mkPrimQual charName = do
    pure $ Scalar ScalarChar

-- custom types
astTypeToInferType' loc (Generic.UserDefinedType name) = do
    pure $ TypeConstructor (name ^. unlocated) []

assertMonotype :: Member (Error TypeConvertError) r => Type SourceRegion -> Sem r (Monotype SourceRegion)
assertMonotype (Lifted t) = pure t
assertMonotype (Polytype t) = throw (HigherRankTypesNotSupported t)

data TypeConvertError
    = HigherRankTypesNotSupported (Polytype SourceRegion)
    | NotSupported Text
    deriving (Show, Eq, Generic)

instance Pretty TypeConvertError

instance ReportableError TypeConvertError
