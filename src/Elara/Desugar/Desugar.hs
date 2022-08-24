{-# LANGUAGE RecordWildCards #-}

module Elara.Desugar.Desugar where

import Control.Lens (set, view, (^.))
import Control.Monad.ListM
import Data.List (foldl1)
import Data.Map qualified as M
import Data.Multimap qualified as Mu
import Data.Text (pack)
import Elara.AST.Canonical qualified as Canonical
import Elara.AST.Frontend qualified as Frontend
import Elara.Data.Located (IsLocated, Located (..))
import Elara.Data.Located qualified as Located
import Elara.Data.Module
import Elara.Data.Module.Inspection
import Elara.Data.Name as Name (ModuleName, Name (Name), NameLike (moduleName), QualifiedName (QualifiedName))
import Elara.Data.Project (Project (..))
import Elara.Data.Qualifications (MaybeQualified, Qualified)
import Elara.Data.Type
import Elara.Data.TypeAnnotation (TypeAnnotation (..))
import Elara.Data.Uniqueness
import Elara.Error (DesugarError (CantDesugar, EmptyBlock, LetEndsBlock, MultipleDeclarations))
import Print (prettyShow)
import Relude
import Text.Pretty.Simple (pShow)
import Utils qualified

type DesugarResult = ReaderT DesugarState (Either DesugarError)

data DesugarState = DesugarState
  { allModules :: M.Map ModuleName SourceModule,
    thisModule :: SourceModule
  }

type SourceModule = Module Frontend.LocatedExpr Frontend.Pattern TypeAnnotation MaybeQualified Many

type CanonicalModule = Module Canonical.LocatedExpr Canonical.Pattern (ConcreteType Qualified) Qualified Unique

type SourceDeclaration = Declaration Frontend.LocatedExpr Frontend.Pattern TypeAnnotation MaybeQualified

type CanonicalDeclaration = Declaration Canonical.LocatedExpr Canonical.Pattern (ConcreteType Qualified) Qualified

type CanonicalDeclarationBody = DeclarationBody Canonical.LocatedExpr Canonical.Pattern (ConcreteType Qualified) Qualified

desugarProject :: Project Frontend.ProjectFields -> Either DesugarError (Project Canonical.ProjectFields)
desugarProject project@Project {..} = do
  let allModules = fields.modules
  modules' <- traverse (desugarModule allModules) (M.elems allModules)
  let newFields = Canonical.ProjectFields (Utils.associateWithKey (view name) modules')
  return project {fields = newFields}

desugarModule :: M.Map ModuleName SourceModule -> SourceModule -> Either DesugarError CanonicalModule
desugarModule modules module'@Module {..} = do
  let state = DesugarState modules module'
  let declarations' = module' ^. declarations
  merged <- usingReaderT state $ do
    declarations' <- traverse desugarDeclaration declarations'
    mergeDeclarations declarations'
  return module' {_moduleDeclarations = merged}

mergeDeclarations :: Mu.ListMultimap Name CanonicalDeclaration -> DesugarResult (M.Map Name CanonicalDeclaration)
mergeDeclarations multimap = do
  let asMap = Mu.toMap multimap
  traverse (\declarations -> foldM1 mergeDeclaration declarations) asMap

mergeDeclaration :: CanonicalDeclaration -> CanonicalDeclaration -> DesugarResult CanonicalDeclaration
mergeDeclaration a b = do
  body <- mergeBodies (a ^. body) (b ^. body)
  return $ Declaration (a ^. module_) (a ^. name) body
  where
    throwDuplicateError :: DesugarResult x
    throwDuplicateError = do
      thisModuleName <- (^. name) . thisModule <$> ask
      lift (Left $ MultipleDeclarations (a ^. name) thisModuleName)

    mergeBodies :: CanonicalDeclarationBody -> CanonicalDeclarationBody -> DesugarResult CanonicalDeclarationBody
    mergeBodies a' b' | a' == b' = throwDuplicateError
    mergeBodies (Value {}) (Value {}) = throwDuplicateError
    mergeBodies (ValueTypeDef annotation) v@(Value _ _ annotation2) = do
      if isJust annotation2
        then throwDuplicateError
        else return $ v {_declarationBodyTypeAnnotation = Just annotation}
    mergeBodies v@(Value _ _ annotation2) (ValueTypeDef annotation) = do
      if isJust annotation2
        then throwDuplicateError
        else return $ v {_declarationBodyTypeAnnotation = Just annotation}
    mergeBodies a' b' = error ("mergeBodies: " <> prettyShow a' <> " and " <> prettyShow b')

desugarDeclaration :: SourceDeclaration -> DesugarResult CanonicalDeclaration
desugarDeclaration dec@Declaration {..} = do
  case view body dec of
    ValueTypeDef annotation -> Declaration _declarationModule_ _declarationName . ValueTypeDef <$> desugarAnnotation annotation
    Value expr patterns ty -> do
      expr' <- desugarExpr (view name dec) expr
      patterns' <- traverse desugarPattern patterns
      ty' <- traverse desugarAnnotation ty

      return $ Declaration _declarationModule_ _declarationName $ Value expr' patterns' ty'
    _ -> lift . Left . CantDesugar $ "desugarDeclaration: " <> show dec

desugarExpr :: Name -> Frontend.LocatedExpr -> DesugarResult Canonical.LocatedExpr
desugarExpr name (Located _ (Frontend.Lambda args body)) = do
  body' <- desugarExpr name body
  curryLambda args body'
desugarExpr name (Located _ (Frontend.Block exprs)) = desugarBlock name exprs
desugarExpr name e = traverse desugarExpr' e
  where
    desugarExpr' :: Frontend.Expr IsLocated -> DesugarResult Canonical.Expr
    desugarExpr' expr = case expr of
      Frontend.Int i -> return $ Canonical.Int i
      Frontend.Float f -> return $ Canonical.Float f
      Frontend.String s -> return $ Canonical.String s
      Frontend.Bool b -> return $ Canonical.Bool b
      Frontend.Unit -> return Canonical.Unit
      Frontend.Argument name -> return $ Canonical.Argument name
      Frontend.Var name -> do
        name' <- qualifyName name
        return (Canonical.Var name')
      Frontend.BinaryOperator op left right -> do
        op' <- desugarExpr name op
        left' <- desugarExpr name left
        right' <- desugarExpr name right
        return $ Canonical.BinaryOperator op' left' right'
      Frontend.FunctionCall f arg -> do
        f' <- desugarExpr name f
        arg' <- desugarExpr name arg
        return $ Canonical.FunctionCall f' arg'
      Frontend.If cond then_ else_ -> do
        cond' <- desugarExpr name cond
        then_' <- desugarExpr name then_
        else_' <- desugarExpr name else_
        return $ Canonical.If cond' then_' else_'
      Frontend.List exprs -> traverse (desugarExpr name) exprs <&> Canonical.List
      l@(Frontend.Let name pats body) -> do
        error $ "standalone let " <> show l
      Frontend.LetIn name pats val body -> do
        val' <- desugarExpr name val
        curriedVal <- curryLambda pats val'
        body' <- desugarExpr name body
        return $ Canonical.LetIn name curriedVal body'
      other -> lift . Left . CantDesugar $ "desugarExpr: " <> show other

desugarLet :: Name -> [Frontend.Pattern] -> Frontend.LocatedExpr -> DesugarResult (Name, Canonical.LocatedExpr)
desugarLet name pats val = do
  error "what"

desugarBlock :: Name -> [Frontend.LocatedExpr] -> DesugarResult Canonical.LocatedExpr
desugarBlock varName [] = ask >>= (lift . Left . EmptyBlock varName) . (^. name) . thisModule
desugarBlock varName exprs = do
  let spanningRegion = Located.spanningRegion (Located.getRegion <$> fromList exprs)
  block <- desugarBlock' exprs []
  return $ Located spanningRegion block
  where
    desugarBlock' :: [Frontend.LocatedExpr] -> [Canonical.LocatedExpr] -> DesugarResult Canonical.Expr
    desugarBlock' [] acc = return (Canonical.Block acc)
    desugarBlock' [l@(Located _ (Frontend.Let {}))] acc = do
      this <- (^. name) . thisModule <$> ask
      lift . Left $ LetEndsBlock varName this l
    desugarBlock' (l@(Located loc (Frontend.Let name pats body)) : others) acc = do
      pats' <- traverse desugarPattern pats
      body' <- desugarBlock name others
      in' <- desugarBlock name others
      return $! Canonical.Block (acc ++ [Located loc $ Canonical.LetIn name body' in'])
    desugarBlock' (e : xs) acc = do
      e' <- desugarExpr varName e
      desugarBlock' xs (e' : acc)

qualifyName :: Name -> DesugarResult QualifiedName
qualifyName name = do
  DesugarState {..} <- ask
  mod <- lift $ findModuleOfVar allModules thisModule name
  return (QualifiedName mod name)

-- turns \x y z -> ... into \x -> \y -> \z -> ...
curryLambda :: [Frontend.Pattern] -> Canonical.LocatedExpr -> DesugarResult Canonical.LocatedExpr
curryLambda pats lambdaBody = do
  args' <- traverse desugarPattern pats
  return $ foldr (\arg body'' -> Canonical.Lambda arg body'' <$ lambdaBody) lambdaBody args'

desugarPattern :: Frontend.Pattern -> DesugarResult Canonical.Pattern
desugarPattern pat = case pat of
  Frontend.WildPattern -> return Canonical.WildPattern
  Frontend.NamedPattern patName -> return $ Canonical.NamedPattern patName

desugarAnnotation :: TypeAnnotation -> DesugarResult (ConcreteType Qualified)
desugarAnnotation TypeAnnotation {..} = desugarType _type

desugarType :: ConcreteType MaybeQualified -> DesugarResult (ConcreteType Qualified)
desugarType (Concrete (UserDefinedType qual typeName) _) = do
  DesugarState {..} <- ask
  qual' <- lift $ findModuleOfVar allModules thisModule typeName
  let resultType = UserDefinedType qual' typeName
  return (Concrete resultType qual')
desugarType (Concrete Unit q) = desugarType (Concrete (UserDefinedType Nothing (Name "()")) q)
desugarType (Concrete t q) = do
  newType' <- newType
  let qual = q <|> typeQual newType'
  return $ Concrete newType' $ fromMaybe (error $ "cannot resolve " <> toStrict (pShow newType')) qual
  where
    newType = case t of
      (TypeVar n) -> return (TypeVar n)
      Function from to -> do
        from' <- desugarType from
        to' <- desugarType to
        return (Function from' to')
      TypeConstructorApplication con arg -> do
        con' <- desugarType con
        arg' <- desugarType arg
        return (TypeConstructorApplication con' arg')
      _ -> lift . Left . CantDesugar $ "desugarType: " <> show t