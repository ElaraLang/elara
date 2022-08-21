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
import Elara.Data.Module
import Elara.Data.Module.Inspection
import Elara.Data.Name as Name (ModuleName, Name (Name), NameLike (moduleName), QualifiedName (QualifiedName))
import Elara.Data.Project (Project (..))
import Elara.Data.Qualifications (MaybeQualified, Qualified)
import Elara.Data.Type
import Elara.Data.TypeAnnotation (TypeAnnotation (..))
import Elara.Data.Uniqueness
import Elara.Error (DesugarError (CantDesugar, MultipleDeclarations))
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
      expr' <- desugarExpr expr
      patterns' <- traverse desugarPattern patterns
      ty' <- traverse desugarAnnotation ty

      return $ Declaration _declarationModule_ _declarationName $ Value expr' patterns' ty'
    _ -> lift . Left . CantDesugar $ "desugarDeclaration: " <> show dec

desugarExpr :: Frontend.LocatedExpr -> DesugarResult Canonical.LocatedExpr
desugarExpr (Located _ (Frontend.Lambda args body)) = curryLambda args body
desugarExpr e = traverse desugarExpr' e
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
        op' <- desugarExpr op
        left' <- desugarExpr left
        right' <- desugarExpr right
        return $ Canonical.BinaryOperator op' left' right'
      Frontend.FunctionCall f arg -> do
        f' <- desugarExpr f
        arg' <- desugarExpr arg
        return $ Canonical.FunctionCall f' arg'
      Frontend.If cond then_ else_ -> do
        cond' <- desugarExpr cond
        then_' <- desugarExpr then_
        else_' <- desugarExpr else_
        return $ Canonical.If cond' then_' else_'
      Frontend.Block exprs -> do
        exprs' <- traverse desugarExpr exprs
        return $ Canonical.Block exprs'
      Frontend.List exprs -> traverse desugarExpr exprs <&> Canonical.List
      Frontend.Let name pats body -> do
        body <- curryLambda pats body
        name' <- qualifyName name
        return $ Canonical.Let name' body
      Frontend.LetIn name pats val body -> do
        val' <- curryLambda pats val
        body' <- curryLambda pats body
        name' <- qualifyName name
        return $ Canonical.LetIn name' val' body'
      other -> lift . Left . CantDesugar $ "desugarExpr: " <> show other

qualifyName :: Name -> DesugarResult QualifiedName
qualifyName name = do
  DesugarState {..} <- ask
  mod <- lift $ findModuleOfVar allModules thisModule name
  return (QualifiedName mod name)

-- turns \x y z -> ... into \x -> \y -> \z -> ...
curryLambda :: [Frontend.Pattern] -> Frontend.LocatedExpr -> DesugarResult Canonical.LocatedExpr
curryLambda pats body = do
  args' <- traverse desugarPattern pats
  body' <- desugarExpr body
  return $ foldr (\arg body'' -> Canonical.Lambda arg body'' <$ body) body' args'

desugarPattern :: Frontend.Pattern -> DesugarResult Canonical.Pattern
desugarPattern pat = case pat of
  Frontend.WildPattern -> return Canonical.WildPattern
  Frontend.NamedPattern name -> return $ Canonical.NamedPattern name

desugarAnnotation :: TypeAnnotation -> DesugarResult (ConcreteType Qualified)
desugarAnnotation TypeAnnotation {..} = desugarType _type

desugarType :: ConcreteType MaybeQualified -> DesugarResult (ConcreteType Qualified)
desugarType (Concrete (UserDefinedType qual typeName) q) = do
  let qual' = q <|> qual <|> Name.moduleName typeName
  DesugarState {..} <- ask
  qual <- lift $ findModuleOfVar allModules thisModule typeName
  let resultType = UserDefinedType qual typeName
  return (Concrete resultType qual)
desugarType (Concrete Unit q) = desugarType (Concrete (UserDefinedType Nothing (Name "()")) Nothing)
desugarType (Concrete t q) = do
  newType <- newType
  state <- ask
  let qual = q <|> typeQual newType
  return $ Concrete newType $ fromMaybe (error $ "cannot resolve " <> toStrict (pShow newType)) qual
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