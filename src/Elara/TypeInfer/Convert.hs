module Elara.TypeInfer.Convert where

import Elara.AST.Generic.Types qualified as Generic
import Elara.AST.Name
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Shunted (ShuntedType, ShuntedType')
import Elara.TypeInfer.Type
import Polysemy
import Polysemy.Error
import Elara.Error (ReportableError (..))

astTypeToInferType :: Member (Error TypeConvertError) r => ShuntedType -> Sem r (Type SourceRegion)
astTypeToInferType (Generic.Type (Located loc t, kind)) =
    astTypeToInferType' loc t

astTypeToInferType' :: Member (Error TypeConvertError) r => SourceRegion -> ShuntedType' -> Sem r (Type SourceRegion)
astTypeToInferType' loc (Generic.TypeVar name) = do
    let tv = fmap (Just . nameText) (name ^. unlocated)
    pure $ Lifted $ TypeVar (UnificationVar $ tv)
astTypeToInferType' loc (Generic.FunctionType i o) = do
    i' <- astTypeToInferType i >>= assertMonotype
    o' <- astTypeToInferType o >>= assertMonotype
    pure $ Lifted $ Function i' o'
astTypeToInferType' loc (Generic.UnitType) = do
    pure $ Lifted $ Scalar ScalarUnit
astTypeToInferType' loc (Generic.TupleType ts) = do
    ts' <- traverse (astTypeToInferType >=> assertMonotype) ts
    throw $ NotSupported "Tuple types are not supported yet"
astTypeToInferType' loc (Generic.ListType t) = do
    t' <- astTypeToInferType t >>= assertMonotype
    throw $ NotSupported "List types are not supported yet"
astTypeToInferType' loc (Generic.RecordType fields) = do
    throw $ NotSupported "Record types are not supported yet"
astTypeToInferType' loc (Generic.TypeConstructorApplication ctor arg) = do
    ctor' <- astTypeToInferType ctor >>= assertMonotype
    arg' <- astTypeToInferType arg >>= assertMonotype
    case ctor' of
        TypeConstructor name args -> do
            pure $ Lifted $ TypeConstructor name (args ++ [arg'])
        other -> throw $ NotSupported "Type constructor application is only supported for type constructors"
astTypeToInferType' loc (Generic.UserDefinedType name) = do
    pure $ Lifted $ TypeConstructor (name ^. unlocated) []

assertMonotype :: Member (Error TypeConvertError) r => Type SourceRegion -> Sem r (Monotype SourceRegion)
assertMonotype (Lifted t) = pure t
assertMonotype (Polytype _) = throw HigherRankTypesNotSupported

data TypeConvertError
    = HigherRankTypesNotSupported
    | NotSupported Text
    deriving (Show, Eq)

instance ReportableError TypeConvertError
