module Elara.TypeInfer.Convert where

import Effectful (Eff, (:>))
import Effectful.Error.Static
import Elara.AST.Generic.Types (freeTypeVars)
import Elara.AST.Generic.Types qualified as Generic
import Elara.AST.Kinded
import Elara.AST.Name
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Shunted ()
import Elara.Data.Kind
import Elara.Data.Pretty
import Elara.Error (ReportableError (..))
import Elara.Prim (charName, intName, mkPrimQual, stringName)
import Elara.TypeInfer.Type

astTypeToGeneralisedInferType :: Error TypeConvertError :> r => KindedType -> Eff r (Type SourceRegion)
astTypeToGeneralisedInferType t@(Generic.Type (Located loc t', kind)) = do
    let ftvs = freeTypeVars t
    let skolems = fmap convertTyVar ftvs
    asInferType <- astTypeToInferType' loc t'

    case skolems of
        [] -> pure $ Lifted asInferType
        _ -> pure $ Polytype (Forall skolems EmptyConstraint asInferType)

astTypeToInferType :: Error TypeConvertError :> r => KindedType -> Eff r (Monotype SourceRegion)
astTypeToInferType t@(Generic.Type (Located loc t', kind)) = do
    astTypeToInferType' loc t'

astTypeToInferTypeWithKind :: Error TypeConvertError :> r => KindedType -> Eff r (Monotype SourceRegion, ElaraKind)
astTypeToInferTypeWithKind t@(Generic.Type (Located loc t', kind)) = do
    asInferType <- astTypeToInferType' loc t'
    pure (asInferType, kind)

convertTyVar name = fmap (Just . nameText) (name ^. unlocated)

astTypeToInferType' :: Error TypeConvertError :> r => SourceRegion -> KindedType' -> Eff r (Monotype SourceRegion)
astTypeToInferType' _ (Generic.TypeVar name) = do
    pure $ TypeVar $ UnificationVar $ convertTyVar name -- idk if this should ever be a skolem variable? i dont think so
astTypeToInferType' _ (Generic.FunctionType i o) = do
    i' <- astTypeToInferType i
    o' <- astTypeToInferType o
    pure $ Function i' o'
astTypeToInferType' _ Generic.UnitType = do
    pure $ Scalar ScalarUnit
astTypeToInferType' _ (Generic.TupleType ts) = do
    ts' <- traverse astTypeToInferType ts
    throwError $ NotSupported "Tuple types are not supported yet"
astTypeToInferType' _ (Generic.ListType t) = do
    t' <- astTypeToInferType t
    throwError $ NotSupported "List types are not supported yet"
astTypeToInferType' _ (Generic.RecordType fields) = do
    throwError $ NotSupported "Record types are not supported yet"
astTypeToInferType' _ (Generic.TypeConstructorApplication ctor arg) = do
    ctor' <- astTypeToInferType ctor
    arg' <- astTypeToInferType arg
    case ctor' of
        TypeConstructor name args -> do
            pure $ TypeConstructor name (args ++ [arg'])
        other -> throwError $ NotSupported "Type constructor application is only supported for type constructors"
-- primitive types
-- this will be removed soon as we remove primitives from the typechecker
astTypeToInferType' _ (Generic.UserDefinedType (Located _ name)) | name == mkPrimQual stringName = do
    pure $ Scalar ScalarString
astTypeToInferType' _ (Generic.UserDefinedType (Located _ name)) | name == mkPrimQual intName = do
    pure $ Scalar ScalarInt
astTypeToInferType' _ (Generic.UserDefinedType (Located _ name)) | name == mkPrimQual charName = do
    pure $ Scalar ScalarChar

-- custom types
astTypeToInferType' _ (Generic.UserDefinedType name) = do
    pure $ TypeConstructor (name ^. unlocated) []

assertMonotype :: Error TypeConvertError :> r => Type SourceRegion -> Eff r (Monotype SourceRegion)
assertMonotype (Lifted t) = pure t
assertMonotype (Polytype t) = throwError (HigherRankTypesNotSupported t)

data TypeConvertError
    = HigherRankTypesNotSupported (Polytype SourceRegion)
    | NotSupported Text
    deriving (Show, Eq, Generic)

instance Pretty TypeConvertError

instance ReportableError TypeConvertError
