{-# LANGUAGE RecordWildCards #-}

module Elara.TypeInfer.Kind where

import Elara.AST.Generic.Types
import Elara.AST.Generic.Types qualified as Generic
import Elara.AST.Kinded
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.VarRef (mkGlobal')
import Elara.Data.Kind
import Elara.Data.Unique
import Elara.Prim (fullListName)
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError (..))
import Elara.TypeInfer.Infer (Status)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type qualified as Infer
import Polysemy
import Polysemy.Error
import Polysemy.State
import Print (showPretty)

universallyQuantify :: [Located (Unique LowerAlphaName)] -> Infer.Type SourceRegion -> Infer.Type SourceRegion
universallyQuantify [] x = x
universallyQuantify (Located sr u : us) t =
    Infer.Forall sr sr (fmap (Just . nameText) u) Domain.Type (universallyQuantify us t)

-- | Like 'astTypeToInferType' but universally quantifies over the free type variables
astTypeToInferPolyType :: (HasCallStack, Member (State Status) r, Member (Error TypeInferenceError) r) => KindedType -> Sem r (Infer.Type SourceRegion, ElaraKind)
astTypeToInferPolyType l = first (universallyQuantify (freeTypeVars l)) <$> astTypeToInferType l

astTypeToInferType :: forall r. HasCallStack => (Member (State Status) r, Member (Error TypeInferenceError) r) => KindedType -> Sem r (Infer.Type SourceRegion, ElaraKind)
astTypeToInferType lt@(Generic.Type (Located sr ut, kind)) = astTypeToInferType' ut
  where
    astTypeToInferType' :: KindedType' -> Sem r (Infer.Type SourceRegion, ElaraKind)
    astTypeToInferType' (TypeVar l) = pure (Infer.VariableType sr (l ^. unlocated % to (fmap (Just . nameText))), kind)
    astTypeToInferType' UnitType = pure (Infer.Scalar sr Mono.Unit, kind)
    astTypeToInferType' (UserDefinedType n) = do
        ctx <- Infer.get
        case Context.lookup (mkGlobal' n) ctx of
            Just ty -> pure (ty, kind)
            Nothing -> throw (UserDefinedTypeNotInContext sr lt ctx)
    astTypeToInferType' (FunctionType a b) = do
        (a', _) <- astTypeToInferType a
        (b', _) <- astTypeToInferType b
        pure (Infer.Function sr a' b', kind)
    astTypeToInferType' (ListType ts) = do
        (ts', _) <- astTypeToInferType ts
        pure (Infer.Custom sr fullListName [ts'], kind)
    astTypeToInferType' (TypeConstructorApplication ctor arg) = do
        (ctor', _) <- astTypeToInferType ctor
        (arg', _) <- astTypeToInferType arg

        -- apply the type argument to the constructor
        -- this will discharge one single forall and one single function arrow
        let applyTypeArg ctor'' arg'' = base
              where
                base = case ctor'' of
                    Infer.Custom{conName = ctorName, ..} -> Infer.Custom sr ctorName (typeArguments ++ [arg''])
                    Infer.Function{output} -> output
                    Infer.Forall{..} ->
                        -- apply a substitution to the forall
                        Infer.substituteType name arg'' (rec' type_)
                    other -> error (showPretty (other, arg''))
                rec' ctor'' = case ctor'' of
                    Infer.Function{output} -> output
                    Infer.Forall{type_, ..} -> Infer.Forall{type_ = rec' type_, ..}
                    other -> other

        pure (applyTypeArg ctor' arg', kind)
    astTypeToInferType' other = error (showPretty other)