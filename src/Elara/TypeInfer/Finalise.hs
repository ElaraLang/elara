{- | Once we have a substituted 'Elara.AST.Typed.Expr PartialType' we want to turn it into a 'Elara.AST.Typed.Expr Type'.
| This is just done by replacing any leftover IDs with fresh type variables.
-}
module Elara.TypeInfer.Finalise where

import Control.Lens
import Data.IntMap qualified as IM
import Elara.AST.Module (Module, traverseModule)
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.Typed
import Elara.Data.Unique
import Elara.TypeInfer.TypeVariables
import Polysemy
import Polysemy.State

type TVMap = IntMap TypeVar

getTV :: (Member (State TVMap) r, Member UniqueGen r) => UniqueId -> Sem r TypeVar
getTV id = do
  s <- get
  let key = uniqueIdVal id
  case IM.lookup key s of
    Just tv -> pure tv
    Nothing -> do
      tv <- newTypeVar
      put (IM.insert key tv s)
      pure tv

runFinalise :: Sem (State TVMap ': r) a -> Sem r a
runFinalise = evalState IM.empty

finaliseExpr :: forall r. (Member (State TVMap) r, Member UniqueGen r) => Expr PartialType -> Sem r (Expr Type)
finaliseExpr = traverseOf _Expr finaliseExpr'
 where
  finaliseExpr' :: (Located (Expr' PartialType), PartialType) -> Sem r (Located (Expr' Type), Type)
  finaliseExpr' (e, t) = do
    t' <- finaliseType t
    e' <- traverse (traverse finaliseType) e
    pure (e', t')

finaliseType :: forall r. (Member (State TVMap) r, Member UniqueGen r) => PartialType -> Sem r Type
finaliseType (Final t) = pure t
finaliseType (Id id) = Type . TypeVar <$> getTV id
finaliseType (Partial t) = Type <$> finaliseType' t
 where
  finaliseType' :: Type' PartialType -> Sem r (Type' Type)
  finaliseType' = \case
    UnitType -> pure UnitType
    TypeVar v -> pure (TypeVar v)
    UserDefinedType lqtn -> pure (UserDefinedType lqtn)
    FunctionType t1 t2 -> FunctionType <$> finaliseType t1 <*> finaliseType t2
    TypeConstructorApplication t1 t2 -> TypeConstructorApplication <$> finaliseType t1 <*> finaliseType t2
    RecordType fields -> RecordType <$> traverse (traverse finaliseType) fields

finaliseModule ::
  forall r.
  (Member (State TVMap) r, Member UniqueGen r) =>
  Module PartialTyped ->
  Sem r (Module Typed)
finaliseModule = traverseModule finaliseDeclaration
 where
  finaliseDeclaration :: Declaration PartialType -> Sem r (Declaration Type)
  finaliseDeclaration = traverseOf (_Declaration . unlocated) finaliseDecl

  finaliseDecl :: Declaration' PartialType -> Sem r (Declaration' Type)
  finaliseDecl (decl' :: Declaration' PartialType) = do
    body' <- finaliseDeclarationBody (decl' ^. declaration'Body)
    pure (Declaration' (decl' ^. moduleName) (decl' ^. name) body')

  finaliseDeclarationBody :: DeclarationBody PartialType -> Sem r (DeclarationBody Type)
  finaliseDeclarationBody =
    traverseOf
      (_DeclarationBody . unlocated)
      ( \case
          Value e _ -> Value <$> finaliseExpr e <*> pure Nothing
          TypeAlias t -> TypeAlias <$> traverse finaliseType t
      )