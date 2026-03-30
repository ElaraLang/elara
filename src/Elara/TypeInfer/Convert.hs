module Elara.TypeInfer.Convert where

import Effectful (Eff, (:>))
import Effectful.Error.Static
import Elara.AST.Name
import Elara.AST.Phases.Kinded (KindedType, KindedType')
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Types qualified as New
import Elara.Data.Kind
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Error (ReportableError (..))
import Elara.Prim (mkPrimQual)
import Elara.TypeInfer.Type

-- | Collect free type variables from a type
freeTypeVars :: KindedType -> [Located (Unique LowerAlphaName)]
freeTypeVars (New.Type _ _ t') = freeTypeVars' t'
  where
    freeTypeVars' :: KindedType' -> [Located (Unique LowerAlphaName)]
    freeTypeVars' (New.TVar v) = [v]
    freeTypeVars' (New.TFun t1 t2) = freeTypeVars t1 <> freeTypeVars t2
    freeTypeVars' New.TUnit = []
    freeTypeVars' (New.TApp t1 t2) = freeTypeVars t1 <> freeTypeVars t2
    freeTypeVars' (New.TUserDefined _) = []
    freeTypeVars' (New.TRecord fields) = concatMap (freeTypeVars . snd) fields
    freeTypeVars' (New.TList t) = freeTypeVars t
    freeTypeVars' (New.TExtension v) = absurd v

astTypeToGeneralisedInferType :: Error TypeConvertError :> r => KindedType -> Eff r (Type SourceRegion)
astTypeToGeneralisedInferType t@(New.Type loc _kind t') = do
    let ftvs = ordNub $ freeTypeVars t
    let skolems = fmap (view unlocated . convertTyVar) ftvs
    asInferType <- astTypeToInferType' loc t'

    case skolems of
        [] -> pure $ Lifted asInferType
        _ -> pure $ Polytype (Forall loc skolems (EmptyConstraint loc) asInferType)

astTypeToInferType :: Error TypeConvertError :> r => KindedType -> Eff r (Monotype SourceRegion)
astTypeToInferType (New.Type loc _kind t') = do
    astTypeToInferType' loc t'

astTypeToInferTypeWithKind :: Error TypeConvertError :> r => KindedType -> Eff r (Monotype SourceRegion, ElaraKind)
astTypeToInferTypeWithKind (New.Type loc kind t') = do
    asInferType <- astTypeToInferType' loc t'
    pure (asInferType, kind)

convertTyVar :: Located (Unique LowerAlphaName) -> Located UniqueTyVar
convertTyVar = fmap (fmap (Just . nameText))

astTypeToInferType' :: Error TypeConvertError :> r => SourceRegion -> KindedType' -> Eff r (Monotype SourceRegion)
astTypeToInferType' loc (New.TVar name) = do
    pure $ TypeVar loc $ UnificationVar $ view unlocated $ convertTyVar name
astTypeToInferType' loc (New.TFun i o) = do
    i' <- astTypeToInferType i
    o' <- astTypeToInferType o
    pure $ Function loc i' o'
astTypeToInferType' loc New.TUnit = do
    pure $ TypeConstructor loc (mkPrimQual "()") []
astTypeToInferType' _ (New.TList _t) = do
    throwError $ NotSupported "List types are not supported yet"
astTypeToInferType' _ (New.TRecord _fields) = do
    throwError $ NotSupported "Record types are not supported yet"
astTypeToInferType' loc (New.TApp ctor arg) = do
    ctor' <- astTypeToInferType ctor
    arg' <- astTypeToInferType arg
    case ctor' of
        TypeConstructor _ name args -> do
            pure $ TypeConstructor loc name (args ++ [arg'])
        _other -> throwError $ NotSupported "Type constructor application is only supported for type constructors"
astTypeToInferType' loc (New.TUserDefined name) = do
    pure $ TypeConstructor loc (name ^. unlocated) []
astTypeToInferType' _ (New.TExtension v) = absurd v

assertMonotype :: Error TypeConvertError :> r => Type SourceRegion -> Eff r (Monotype SourceRegion)
assertMonotype (Lifted t) = pure t
assertMonotype (Polytype t) = throwError (HigherRankTypesNotSupported t)

data TypeConvertError
    = HigherRankTypesNotSupported (Polytype SourceRegion)
    | NotSupported Text
    deriving (Show, Eq, Generic)

instance Pretty TypeConvertError

instance ReportableError TypeConvertError
