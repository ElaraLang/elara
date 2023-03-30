{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Rename where

import Control.Lens (Each (each), Getter, filteredBy, folded, makeLenses, over, to, traverseOf, traverseOf_, use, (%~), (^.), (^..), _2)
import Data.Map qualified as Map
import Elara.AST.Desugared qualified as Desugared
import Elara.AST.Module (
    Exposing (ExposingAll, ExposingSome),
    Exposition (..),
    HasAs (as),
    HasDeclarations (declarations),
    HasExposing (exposing),
    HasImporting (importing),
    HasImports (imports),
    HasQualified (qualified),
    Import,
    Import' (..),
    Module,
    Module' (Module'),
    _Import,
    _Module,
 )
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (NOpName, NTypeName, NVarName), NameLike (nameText), Qualified (Qualified), ToName (toName), TypeName, VarName (OperatorVarName))
import Elara.AST.Region (Located (Located), sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Renamed (VarRef (..))
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select (Desugared, HasModuleName (..), HasName (..), Renamed)
import Elara.Data.Unique (Unique, UniqueGen, makeUnique, uniqueGenToIO)
import Elara.Error (ReportableError (report), writeReport)
import Elara.Error.Codes qualified as Codes (nonExistentModuleDeclaration, unknownModule)
import Error.Diagnose (Marker (This), Report (Err))
import Polysemy (Sem)
import Polysemy.Embed
import Polysemy.Error (Error, note, runError, throw)
import Polysemy.MTL ()
import Polysemy.Reader hiding (Local)
import Polysemy.State

data RenameError
    = UnknownModule ModuleName
    | QualifiedInWrongModule ModuleName ModuleName
    | NonExistentModuleDeclaration ModuleName (Located Name)
    | UnknownName (Located Name)
    | NativeDefUnsupported (Located Desugared.Declaration')

instance ReportableError RenameError where
    report (UnknownModule mn) =
        writeReport $
            Err
                (Just Codes.unknownModule)
                ("Unknown module: " <> show mn)
                []
                []
    report (QualifiedInWrongModule m1 m2) =
        writeReport $
            Err
                Nothing
                ("Qualified name in wrong module: " <> show m1 <> " in " <> show m2)
                []
                []
    report (NonExistentModuleDeclaration m n) =
        let nPos = sourceRegionToDiagnosePosition (n ^. sourceRegion)
         in writeReport $
                Err
                    (Just Codes.nonExistentModuleDeclaration)
                    ("Element " <> (n ^. unlocated . to nameText) <> " does not exist in in module " <> nameText m)
                    [(nPos, This "referenced here")]
                    []
    report (UnknownName n) =
        writeReport $
            Err
                Nothing
                ("Unknown name: " <> show n)
                []
                []
    report (NativeDefUnsupported _) =
        writeReport $
            Err
                Nothing
                "Native definitions are not supported"
                []
                []

data RenameState = RenameState
    { _varNames :: Map VarName (VarRef VarName)
    , _typeNames :: Map TypeName (VarRef TypeName)
    }
    deriving (Show)

makeLenses ''RenameState

type ModulePath = Map ModuleName (Module Desugared)

type Renamer a = Sem '[State RenameState, Error RenameError, Reader ModulePath, UniqueGen] a

runRenamer :: i -> Sem (State RenameState : Error e : Reader i : UniqueGen : r) a -> Sem (Embed IO : r) (Either e a)
runRenamer mp = uniqueGenToIO . runReader mp . runError . evalState (RenameState Map.empty Map.empty)

qualifyIn :: ModuleName -> MaybeQualified name -> Renamer (Qualified name)
qualifyIn mn (MaybeQualified n (Just m)) = do
    when (m /= mn) $ throw $ QualifiedInWrongModule m mn
    pure $ Qualified n m
qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

qualifyTypeName :: Located (MaybeQualified TypeName) -> Renamer (Located (Qualified TypeName))
qualifyTypeName (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed m (Located sr (NTypeName n))
    pure $ Located sr (Qualified n m)
qualifyTypeName (Located sr (MaybeQualified n Nothing)) = do
    typeNames' <- use typeNames
    case Map.lookup n typeNames' of
        Just (Global (Located sr' (Qualified n' m))) -> pure $ Located sr' (Qualified n' m)
        Just (Local _) -> error "can't have local type names"
        Nothing -> throw $ UnknownName (Located sr (NTypeName n))

lookupGenericName ::
    (Ord name, ToName name) =>
    Getter RenameState (Map name (VarRef name)) ->
    Located (MaybeQualified name) ->
    Renamer (Located (VarRef name))
lookupGenericName _ (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed m (Located sr (toName n))
    pure $ Located sr $ Global (Located sr (Qualified n m))
lookupGenericName lens (Located sr (MaybeQualified n Nothing)) = do
    names' <- use lens
    case Map.lookup n names' of
        Just v -> pure $ Located sr v
        Nothing -> throw $ UnknownName (Located sr $ toName n)

lookupVarName :: Located (MaybeQualified VarName) -> Renamer (Located (VarRef VarName))
lookupVarName = lookupGenericName varNames

lookupTypeName :: Located (MaybeQualified TypeName) -> Renamer (Located (VarRef TypeName))
lookupTypeName = lookupGenericName typeNames

inModifiedState :: (RenameState -> RenameState) -> Renamer a -> Renamer a
inModifiedState f m = do
    s <- get
    put $ f s
    a <- m
    put s
    pure a

uniquify :: Located name -> Renamer (Located (Unique name))
uniquify (Located sr n) = Located sr <$> makeUnique n

rename :: Module Desugared -> Renamer (Module Renamed)
rename =
    traverseOf
        (_Module @Desugared @Renamed . unlocated)
        ( \m' -> do
            addImportsToContext (m' ^. imports)
            traverseOf_ (declarations . each) (addDeclarationToContext False) m' -- add our own declarations to the context
            exposing' <- renameExposing (m' ^. name . unlocated) (m' ^. exposing)
            imports' <- traverse renameImport (m' ^. imports)
            declarations' <- traverse renameDeclaration (m' ^. declarations)
            pure (Module' (m' ^. name) exposing' imports' declarations')
        )
  where
    renameExposing :: ModuleName -> Exposing Desugared -> Renamer (Exposing Renamed)
    renameExposing _ ExposingAll = pure ExposingAll
    renameExposing mn (ExposingSome es) = ExposingSome <$> traverse (renameExposition mn) es

    renameExposition :: ModuleName -> Exposition Desugared -> Renamer (Exposition Renamed)
    renameExposition mn (ExposedValue vn) = ExposedValue <$> traverse (qualifyIn mn) vn
    renameExposition mn (ExposedOp opn) = ExposedOp <$> traverse (qualifyIn mn) opn
    renameExposition mn (ExposedType tn) = ExposedType <$> traverse (qualifyIn mn) tn
    renameExposition mn (ExposedTypeAndAllConstructors tn) = ExposedTypeAndAllConstructors <$> traverse (qualifyIn mn) tn

    renameImport :: Import Desugared -> Renamer (Import Renamed)
    renameImport = traverseOf (_Import @Desugared @Renamed . unlocated) renameImport'

    renameImport' :: Import' Desugared -> Renamer (Import' Renamed)
    renameImport' imp = do
        exposing' <- renameExposing (imp ^. importing . unlocated) (imp ^. exposing)
        pure $ Import' (imp ^. importing) (imp ^. as) (imp ^. qualified) exposing'

addImportsToContext :: [Import Desugared] -> Renamer ()
addImportsToContext = traverse_ addImportToContext
  where
    addImportToContext :: Import Desugared -> Renamer ()
    addImportToContext imp = do
        modules <- ask
        imported <- note (UnknownModule (imp ^. importing . unlocated)) $ Map.lookup (imp ^. importing . unlocated) modules
        let isExposingL = Desugared._Declaration . unlocated . Desugared.declaration'Name . unlocated . to (isExposingAndExists imported)
        let exposed = case imported ^. exposing of
                ExposingAll -> imported ^. declarations
                ExposingSome _ -> imported ^.. declarations . folded . filteredBy isExposingL
        traverse_ (addDeclarationToContext (imp ^. qualified)) exposed

addDeclarationToContext :: Bool -> Desugared.Declaration -> Renamer ()
addDeclarationToContext _ decl = do
    let global :: name -> VarRef name
        global vn = Global (Qualified vn (decl ^. moduleName . unlocated) <$ decl ^. Desugared._Declaration)
    case decl ^. name . unlocated of
        NVarName vn -> modify $ over varNames $ Map.insert vn (global vn)
        NTypeName vn -> modify $ over typeNames $ Map.insert vn (global vn)
        NOpName vn -> modify $ over varNames $ Map.insert (OperatorVarName vn) (global (OperatorVarName vn))

ensureExistsAndExposed :: ModuleName -> Located Name -> Renamer ()
ensureExistsAndExposed mn n = do
    modules <- ask
    case Map.lookup mn modules of
        Nothing -> throw $ UnknownModule mn
        Just m -> do
            unless (elementExistsInModule m (n ^. unlocated)) $ throw $ NonExistentModuleDeclaration mn n
            unless (isExposingAndExists m (n ^. unlocated)) $ throw $ UnknownName n

elementExistsInModule :: Module Desugared -> Name -> Bool
elementExistsInModule m' n' = any (\d -> d ^. name . unlocated == n') (m' ^. declarations)

isExposingAndExists :: Module Desugared -> Name -> Bool
isExposingAndExists m n =
    let mn = m ^. name . unlocated
     in case m ^. exposing of
            ExposingAll -> elementExistsInModule m n
            ExposingSome es -> elementExistsInModule m n && any (isExposition mn n) es
  where
    isExposition :: ModuleName -> Name -> Exposition Desugared -> Bool
    isExposition mn (NVarName vn) (ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
    isExposition mn (NOpName opn) (ExposedOp opn') = MaybeQualified opn (Just mn) == opn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False

renameDeclaration :: Desugared.Declaration -> Renamer Renamed.Declaration
renameDeclaration (Desugared.Declaration ld) = Renamed.Declaration <$> traverseOf unlocated renameDeclaration' ld
  where
    renameDeclaration' :: Desugared.Declaration' -> Renamer Renamed.Declaration'
    renameDeclaration' fd = do
        let name' = sequenceA (traverseOf unlocated (`Qualified` (fd ^. Desugared.declaration'Module' . unlocated)) (fd ^. name))
        body' <- renameDeclarationBody (fd ^. Desugared.declaration'Body)
        pure $ Renamed.Declaration' (fd ^. Desugared.declaration'Module') name' body'

    renameDeclarationBody :: Desugared.DeclarationBody -> Renamer Renamed.DeclarationBody
    renameDeclarationBody (Desugared.DeclarationBody ldb) = Renamed.DeclarationBody <$> traverseOf unlocated renameDeclarationBody' ldb

    renameDeclarationBody' :: Desugared.DeclarationBody' -> Renamer Renamed.DeclarationBody'
    renameDeclarationBody' (Desugared.Value val ty) = do
        val' <- renameExpr val
        let declModuleName = ld ^. unlocated . (unlocatedModuleName @Desugared.Declaration' @Desugared)
        ty' <- traverse (traverse (renameTypeAnnotation declModuleName)) ty
        pure $ Renamed.Value val' ty'
    renameDeclarationBody' (Desugared.TypeAlias ty) = do
        ty' <- traverse renameType ty
        pure $ Renamed.TypeAlias ty'
    renameDeclarationBody' (Desugared.NativeDef _) = throw (NativeDefUnsupported ld)

renameTypeAnnotation :: ModuleName -> Desugared.TypeAnnotation -> Renamer Renamed.TypeAnnotation
renameTypeAnnotation thisMod (Desugared.TypeAnnotation ln ty) = Renamed.TypeAnnotation ((`Qualified` thisMod) <$> ln) <$> renameType ty

renameType :: Desugared.Type -> Renamer Renamed.Type
renameType (Desugared.TypeVar n) = Renamed.TypeVar <$> makeUnique n
renameType (Desugared.FunctionType t1 t2) = Renamed.FunctionType <$> renameType t1 <*> renameType t2
renameType Desugared.UnitType = pure Renamed.UnitType
renameType (Desugared.TypeConstructorApplication t1 t2) = Renamed.TypeConstructorApplication <$> renameType t1 <*> renameType t2
renameType (Desugared.UserDefinedType ln) = Renamed.UserDefinedType <$> qualifyTypeName ln
renameType (Desugared.RecordType ln) = Renamed.RecordType <$> traverse (traverseOf _2 renameType) ln

renameExpr :: Desugared.Expr -> Renamer Renamed.Expr
renameExpr (Desugared.Expr le) = Renamed.Expr <$> traverseOf unlocated renameExpr' le
  where
    renameExpr' :: Desugared.Expr' -> Renamer Renamed.Expr'
    renameExpr' (Desugared.Int i) = pure $ Renamed.Int i
    renameExpr' (Desugared.Float i) = pure $ Renamed.Float i
    renameExpr' (Desugared.String i) = pure $ Renamed.String i
    renameExpr' (Desugared.Char i) = pure $ Renamed.Char i
    renameExpr' Desugared.Unit = pure Renamed.Unit
    renameExpr' (Desugared.Var i) = Renamed.Var <$> lookupVarName i
    renameExpr' (Desugared.Constructor i) = Renamed.Constructor <$> lookupTypeName i
    renameExpr' (Desugared.Lambda pat e) = do
        pat' <- renamePattern pat
        exp' <- renameExpr e
        pure $ Renamed.Lambda pat' exp'
    renameExpr' (Desugared.FunctionCall e1 e2) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        pure $ Renamed.FunctionCall e1' e2'
    renameExpr' (Desugared.If e1 e2 e3) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        e3' <- renameExpr e3
        pure $ Renamed.If e1' e2' e3'
    renameExpr' (Desugared.BinaryOperator op left right) = do
        op' <- renameBinaryOperator op
        left' <- renameExpr left
        right' <- renameExpr right
        pure $ Renamed.BinaryOperator op' left' right'
    renameExpr' (Desugared.List es) = Renamed.List <$> traverse renameExpr es
    renameExpr' (Desugared.Let vn e) = do
        vn' <- uniquify vn
        modify (varNames %~ Map.insert (vn ^. unlocated) (Local vn'))
        exp' <- renameExpr e
        pure $ Renamed.Let vn' exp'
    renameExpr' (Desugared.LetIn vn e body) = do
        vn' <- uniquify vn
        inModifiedState (varNames %~ Map.insert (vn ^. unlocated) (Local vn')) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure $ Renamed.LetIn vn' exp' body'
    renameExpr' (Desugared.Match e cases) = do
        e' <- renameExpr e
        cases' <- traverse (bitraverse renamePattern renameExpr) cases
        pure $ Renamed.Match e' cases'
    renameExpr' (Desugared.Block es) = Renamed.Block <$> traverse renameExpr es
    renameExpr' (Desugared.InParens es) = Renamed.InParens <$> renameExpr es

renameBinaryOperator :: Desugared.BinaryOperator -> Renamer Renamed.BinaryOperator
renameBinaryOperator (Desugared.MkBinaryOperator op) = Renamed.MkBinaryOperator <$> traverseOf unlocated renameBinaryOperator' op
  where
    renameBinaryOperator' :: Desugared.BinaryOperator' -> Renamer Renamed.BinaryOperator'
    renameBinaryOperator' (Desugared.Op o) = do
        op' <- lookupVarName (OperatorVarName <<$>> o)
        let onlyOpName (OperatorVarName o') = o'
            onlyOpName _ = error "renameBinaryOperator': I really don't like this"
        let op'' = onlyOpName <<$>> op'
        pure $ Renamed.Op op''
    renameBinaryOperator' (Desugared.Infixed o) = do
        op' <- lookupVarName o
        pure $ Renamed.Infixed op'

renamePattern :: Desugared.Pattern -> Renamer Renamed.Pattern
renamePattern (Desugared.Pattern fp) = Renamed.Pattern <$> traverseOf unlocated renamePattern' fp
  where
    renamePattern' :: Desugared.Pattern' -> Renamer Renamed.Pattern'

    renamePattern' (Desugared.IntegerPattern i) = pure $ Renamed.IntegerPattern i
    renamePattern' (Desugared.FloatPattern i) = pure $ Renamed.FloatPattern i
    renamePattern' (Desugared.StringPattern i) = pure $ Renamed.StringPattern i
    renamePattern' (Desugared.CharPattern i) = pure $ Renamed.CharPattern i
    renamePattern' Desugared.WildcardPattern = pure Renamed.WildcardPattern
    renamePattern' (Desugared.ListPattern ps) = Renamed.ListPattern <$> traverse renamePattern ps
    renamePattern' (Desugared.VarPattern vn) = do
        vn' <- uniquify vn
        modify (varNames %~ Map.insert (vn ^. unlocated) (Local vn'))
        pure $ Renamed.VarPattern (Located (vn ^. sourceRegion) (Local vn'))
    renamePattern' (Desugared.ConstructorPattern cn ps) = do
        cn' <- qualifyTypeName cn
        ps' <- traverse renamePattern ps
        pure $ Renamed.ConstructorPattern cn' ps'
