{-# LANGUAGE TemplateHaskell #-}

module Elara.Rename where

import Control.Lens (makeLenses, over, traverseOf, use, (%~), (^.))
import Data.Map qualified as Map
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module (Exposing (ExposingAll, ExposingSome), Exposition (..), HasAs (as), HasDeclarations (declarations), HasExposing (exposing), HasImporting (importing), HasImports (imports), HasQualified (qualified), Import, Import' (..), Module, Module' (Module'), _Import, _Module)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (NOpName, NTypeName, NVarName), Qualified (Qualified), TypeName, VarName)
import Elara.AST.Region (Located (Located), sourceRegion, unlocated)
import Elara.AST.Renamed (VarRef (..))
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select (Frontend, HasName (..), Renamed)
import Elara.Data.Unique (Unique, UniqueGen, makeUnique)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.MTL ()
import Polysemy.Reader hiding (Local)
import Polysemy.State

data RenameError
    = UnknownModule ModuleName
    | QualifiedInWrongModule ModuleName ModuleName
    | UnknownName Name

data RenameState = RenameState
    { _varNames :: Map VarName (VarRef VarName)
    , _typeNames :: Map TypeName (VarRef TypeName)
    }

makeLenses ''RenameState

type ModulePath = Map ModuleName (Module Frontend)

type Renamer a = Sem '[State RenameState, UniqueGen, Error RenameError, Reader ModulePath] a

-- qualifyIn :: ModuleName -> MaybeQualified name -> Renamer (Qualified name)
-- qualifyIn mn (MaybeQualified n (Just m)) = do
--     when (m /= mn) $ throw $ QualifiedInWrongModule m mn
--     pure $ Qualified n m
-- qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

-- lookupVarName :: Located (MaybeQualified VarName) -> Renamer (Located (VarRef VarName))
-- lookupVarName (Located sr (MaybeQualified n (Just m))) = do
--     ensureExistsAndExposed m (NVarName n)
--     pure $ Located sr $ Global (Located sr (Qualified n m))
-- lookupVarName (Located sr (MaybeQualified n Nothing)) = do
--     varNames' <- use varNames
--     case Map.lookup n varNames' of
--         Just v -> pure $ Located sr v
--         Nothing -> throw $ UnknownName (NVarName n)

-- qualifyTypeName :: Located (MaybeQualified TypeName) -> Renamer (Located (Qualified TypeName))
-- qualifyTypeName (Located sr (MaybeQualified n (Just m))) = do
--     ensureExistsAndExposed m (NTypeName n)
--     pure $ Located sr (Qualified n m)
-- qualifyTypeName (Located sr (MaybeQualified n Nothing)) = do
--     typeNames' <- use typeNames
--     case Map.lookup n typeNames' of
--         Just (Global (Located sr' (Qualified n' m))) -> pure $ Located sr' (Qualified n' m)
--         Just (Local (Located sr' n')) -> error "can't have local type names"
--         Nothing -> throw $ UnknownName (NTypeName n)

-- lookupTypeName :: Located (MaybeQualified TypeName) -> Renamer (Located (VarRef TypeName))
-- lookupTypeName (Located sr (MaybeQualified n (Just m))) = do
--     ensureExistsAndExposed m (NTypeName n)
--     pure $ Located sr $ Global (Located sr (Qualified n m))
-- lookupTypeName (Located sr (MaybeQualified n Nothing)) = do
--     typeNames' <- use typeNames
--     case Map.lookup n typeNames' of
--         Just v -> pure $ Located sr v
--         Nothing -> throw $ UnknownName (NTypeName n)

-- inModifiedState :: (RenameState -> RenameState) -> Renamer a -> Renamer a
-- inModifiedState f m = do
--     s <- get
--     put $ f s
--     a <- m
--     put s
--     pure a

-- uniquify :: Located name -> Renamer (Located (Unique name))
-- uniquify (Located sr n) = Located sr <$> makeUnique n

-- rename :: Module Frontend -> Renamer (Module Renamed)
-- rename =
--     traverseOf
--         (_Module @Frontend @Renamed . unlocated)
--         ( \m' -> do
--             exposing' <- renameExposing (m' ^. name . unlocated) (m' ^. exposing)
--             imports' <- traverse renameImport (m' ^. imports)
--             declarations' <- traverse renameDeclaration (m' ^. declarations)
--             pure (Module' (m' ^. name) exposing' imports' declarations')
--         )
--   where
--     renameExposing :: ModuleName -> Exposing Frontend -> Renamer (Exposing Renamed)
--     renameExposing _ ExposingAll = pure ExposingAll
--     renameExposing mn (ExposingSome es) = ExposingSome <$> traverse (renameExposition mn) es

--     renameExposition :: ModuleName -> Exposition Frontend -> Renamer (Exposition Renamed)
--     renameExposition mn (ExposedValue vn) = ExposedValue <$> traverse (qualifyIn mn) vn
--     renameExposition mn (ExposedOp opn) = ExposedOp <$> traverse (qualifyIn mn) opn
--     renameExposition mn (ExposedType tn) = ExposedType <$> traverse (qualifyIn mn) tn
--     renameExposition mn (ExposedTypeAndAllConstructors tn) = ExposedTypeAndAllConstructors <$> traverse (qualifyIn mn) tn

--     renameImport :: Import Frontend -> Renamer (Import Renamed)
--     renameImport = traverseOf (_Import @Frontend @Renamed . unlocated) renameImport'

--     renameImport' :: Import' Frontend -> Renamer (Import' Renamed)
--     renameImport' imp = do
--         exposing' <- renameExposing (imp ^. importing . unlocated) (imp ^. exposing)
--         pure $ Import' (imp ^. importing) (imp ^. as) (imp ^. qualified) exposing'

-- ensureExistsAndExposed :: ModuleName -> Name -> Renamer ()
-- ensureExistsAndExposed mn n = do
--     modules <- ask
--     case Map.lookup mn modules of
--         Nothing -> throw $ UnknownModule mn
--         Just m -> unless (isExposing m n) $ throw $ UnknownName n
--   where
--     isExposing :: Module Frontend -> Name -> Bool
--     isExposing m n = case m ^. exposing of
--         ExposingAll -> True
--         ExposingSome es -> any (isExposition n) es

--     isExposition :: Name -> Exposition Frontend -> Bool
--     isExposition (NVarName vn) (ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
--     isExposition (NOpName opn) (ExposedOp opn') = MaybeQualified opn (Just mn) == opn' ^. unlocated
--     isExposition (NTypeName tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
--     isExposition (NTypeName tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
--     isExposition _ _ = False

-- renameDeclaration :: Frontend.Declaration -> Renamer Renamed.Declaration
-- renameDeclaration (Frontend.Declaration ld) = Renamed.Declaration <$> traverseOf unlocated renameDeclaration' ld
--   where
--     renameDeclaration' :: Frontend.Declaration' -> Renamer Renamed.Declaration'
--     renameDeclaration' fd = do
--         let name' = sequenceA (traverseOf unlocated (`Qualified` (fd ^. Frontend.declaration'Module' . unlocated)) (fd ^. name))
--         body' <- renameDeclarationBody (fd ^. Frontend.declaration'Body)
--         pure $ Renamed.Declaration' (fd ^. Frontend.declaration'Module') name' body'

--     renameDeclarationBody :: Frontend.DeclarationBody -> Renamer Renamed.DeclarationBody
--     renameDeclarationBody (Frontend.DeclarationBody ldb) = Renamed.DeclarationBody <$> traverseOf unlocated renameDeclarationBody' ldb

--     renameDeclarationBody' :: Frontend.DeclarationBody' -> Renamer Renamed.DeclarationBody'
--     renameDeclarationBody' (Frontend.Value val pats) = do
--         undefined
--     renameDeclarationBody' (Frontend.TypeAlias pats) = do
--         undefined

-- renameExpr :: Frontend.Expr -> Renamer Renamed.Expr
-- renameExpr (Frontend.Expr le) = Renamed.Expr <$> traverseOf unlocated renameExpr' le
--   where
--     renameExpr' :: Frontend.Expr' -> Renamer Renamed.Expr'
--     renameExpr' (Frontend.Int i) = pure $ Renamed.Int i
--     renameExpr' (Frontend.Float i) = pure $ Renamed.Float i
--     renameExpr' (Frontend.String i) = pure $ Renamed.String i
--     renameExpr' (Frontend.Char i) = pure $ Renamed.Char i
--     renameExpr' Frontend.Unit = pure Renamed.Unit
--     renameExpr' (Frontend.Var i) = Renamed.Var <$> lookupVarName i
--     renameExpr' (Frontend.Lambda pats e) = do
--         pats' <- traverse renamePattern pats
--         exp' <- renameExpr e
--         pure $ foldLambda pats' exp' ^. Renamed._Expr . unlocated
--     renameExpr' (Frontend.FunctionCall e1 e2) = do
--         e1' <- renameExpr e1
--         e2' <- renameExpr e2
--         pure $ Renamed.FunctionCall e1' e2'
--     renameExpr' (Frontend.If e1 e2 e3) = do
--         e1' <- renameExpr e1
--         e2' <- renameExpr e2
--         e3' <- renameExpr e3
--         pure $ Renamed.If e1' e2' e3'
--     renameExpr' (Frontend.List es) = Renamed.List <$> traverse renameExpr es
--     renameExpr' (Frontend.Let vn pats e) = do
--         vn' <- uniquify vn
--         modify (varNames %~ Map.insert (vn ^. unlocated) (Local vn'))
--         pats' <- traverse renamePattern pats
--         exp' <- renameExpr e
--         pure $ Renamed.Let vn' (foldLambda pats' exp')
--     renameExpr' (Frontend.LetIn vn pats e body) = do
--         vn' <- uniquify vn
--         inModifiedState (varNames %~ Map.insert (vn ^. unlocated) (Local vn')) $ do
--             pats' <- traverse renamePattern pats
--             exp' <- renameExpr e
--             body' <- renameExpr body
--             pure $ Renamed.LetIn vn' (foldLambda pats' exp') body'
--     renameExpr' (Frontend.Match e cases) = do
--         e' <- renameExpr e
--         cases' <- traverse (bitraverse renamePattern renameExpr) cases
--         pure $ Renamed.Match e' cases'
--     renameExpr' (Frontend.Block es) = Renamed.Block <$> traverse renameExpr es
--     renameExpr' (Frontend.InParens es) = Renamed.InParens <$> renameExpr es
--     renameExpr' _ = undefined

-- renamePattern :: Frontend.Pattern -> Renamer Renamed.Pattern
-- renamePattern (Frontend.Pattern fp) = Renamed.Pattern <$> traverseOf unlocated renamePattern' fp
--   where
--     renamePattern' :: Frontend.Pattern' -> Renamer Renamed.Pattern'

--     renamePattern' (Frontend.IntegerPattern i) = pure $ Renamed.IntegerPattern i
--     renamePattern' (Frontend.FloatPattern i) = pure $ Renamed.FloatPattern i
--     renamePattern' (Frontend.StringPattern i) = pure $ Renamed.StringPattern i
--     renamePattern' (Frontend.CharPattern i) = pure $ Renamed.CharPattern i
--     renamePattern' Frontend.WildcardPattern = pure Renamed.WildcardPattern
--     renamePattern' (Frontend.ListPattern ps) = Renamed.ListPattern <$> traverse renamePattern ps
--     renamePattern' (Frontend.VarPattern vn) = do
--         vn' <- uniquify vn
--         modify (varNames %~ Map.insert (vn ^. unlocated) (Local vn'))
--         pure $ Renamed.VarPattern (Located (vn ^. sourceRegion) (Local vn'))
--     renamePattern' (Frontend.ConstructorPattern cn ps) = do
--         cn' <- qualifyTypeName cn
--         ps' <- traverse renamePattern ps
--         pure $ Renamed.ConstructorPattern cn' ps'

