{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Rename where

import Control.Lens (Each (each), Getter, filteredBy, folded, makeLenses, over, to, traverseOf, traverseOf_, (%~), (^.), (^..), _1, _2)
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
import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (MaybeQualified), ModuleName, Name (NOpName, NTypeName, NVarName), Qualified (Qualified), ToName (toName), TypeName, VarName (NormalVarName, OperatorVarName))
import Elara.AST.Region (Located (Located), enclosingRegion', sourceRegion, sourceRegionToDiagnosePosition, unlocated, withLocationOf)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select (Desugared, HasModuleName (..), HasName (..), Renamed)
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local))
import Elara.Data.Pretty
import Elara.Data.TopologicalGraph
import Elara.Data.Unique (Unique, UniqueGen, makeUnique, uniqueGenToIO)
import Elara.Error (ReportableError (report), writeReport)
import Elara.Error.Codes qualified as Codes (nonExistentModuleDeclaration, unknownModule)
import Error.Diagnose (Marker (This), Report (Err))
import Polysemy (Members, Sem)
import Polysemy.Embed
import Polysemy.Error (Error, note, runError, throw)
import Polysemy.Reader hiding (Local)
import Polysemy.State
import Polysemy.State.Extra
import Polysemy.Utils (withModified)

data RenameError
    = UnknownModule ModuleName
    | QualifiedInWrongModule ModuleName ModuleName
    | NonExistentModuleDeclaration ModuleName (Located Name)
    | UnknownTypeVariable LowerAlphaName
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
                ("Qualified name in wrong module:" <+> show m1 <+> "in" <+> show m2)
                []
                []
    report (NonExistentModuleDeclaration m n) =
        let nPos = sourceRegionToDiagnosePosition (n ^. sourceRegion)
         in writeReport $
                Err
                    (Just Codes.nonExistentModuleDeclaration)
                    ("Element" <+> (n ^. unlocated . to pretty) <+> "does not exist in in module" <+> pretty m)
                    [(nPos, This "referenced here")]
                    []
    report (UnknownTypeVariable n) =
        writeReport $
            Err
                Nothing
                ("Unknown type variable: " <> show n)
                []
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
    , _typeVars :: Map LowerAlphaName (Unique LowerAlphaName)
    -- ^ All the type variables in scope
    }
    deriving (Show)

makeLenses ''RenameState

instance Semigroup RenameState where
    RenameState v1 t1 tv1 <> RenameState v2 t2 tv2 = RenameState (v1 <> v2) (t1 <> t2) (tv1 <> tv2)

instance Monoid RenameState where
    mempty = RenameState mempty mempty mempty

type Rename r = Members '[State RenameState, Error RenameError, Reader (TopologicalGraph (Module Desugared)), UniqueGen] r

runRenamer :: RenameState -> i -> Sem (State RenameState : Error e : Reader i : UniqueGen : r) a -> Sem (Embed IO : r) (Either e a)
runRenamer st mp = uniqueGenToIO . runReader mp . runError . evalState st

qualifyIn :: Rename r => ModuleName -> MaybeQualified name -> Sem r (Qualified name)
qualifyIn mn (MaybeQualified n (Just m)) = do
    when (m /= mn) $ throw $ QualifiedInWrongModule m mn
    pure $ Qualified n m
qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

qualifyTypeName :: Rename r => Located (MaybeQualified TypeName) -> Sem r (Located (Qualified TypeName))
qualifyTypeName (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed m (Located sr (NTypeName n))
    pure $ Located sr (Qualified n m)
qualifyTypeName (Located sr (MaybeQualified n Nothing)) = do
    typeNames' <- use' typeNames
    case Map.lookup n typeNames' of
        Just (Global (Located sr' (Qualified n' m))) -> pure $ Located sr' (Qualified n' m)
        Just (Local _) -> error "can't have local type names"
        Nothing -> throw $ UnknownName (Located sr (NTypeName n))

lookupGenericName ::
    Rename r =>
    (Ord name, ToName name) =>
    Getter RenameState (Map name (VarRef name)) ->
    Located (MaybeQualified name) ->
    Sem r (Located (VarRef name))
lookupGenericName _ (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed m (Located sr (toName n))
    pure $ Located sr $ Global (Located sr (Qualified n m))
lookupGenericName lens (Located sr (MaybeQualified n Nothing)) = do
    names' <- use' lens
    case Map.lookup n names' of
        Just v -> pure $ Located sr v
        Nothing -> throw $ UnknownName (Located sr $ toName n)

lookupVarName :: Rename r => Located (MaybeQualified VarName) -> Sem r (Located (VarRef VarName))
lookupVarName = lookupGenericName varNames

lookupTypeName :: Rename r => Located (MaybeQualified TypeName) -> Sem r (Located (Qualified TypeName))
lookupTypeName n =
    lookupGenericName typeNames n <<&>> \case
        Local _ -> error "can't have local type names"
        Global v -> v ^. unlocated

lookupTypeVar :: Rename r => LowerAlphaName -> Sem r (Maybe (Unique LowerAlphaName))
lookupTypeVar n = do
    typeVars' <- use' typeVars
    pure $ Map.lookup n typeVars'

uniquify :: Rename r => Located name -> Sem r (Located (Unique name))
uniquify (Located sr n) = Located sr <$> makeUnique n

-- | Performs a topological sort of the declarations, so as many
sortDeclarations :: [Renamed.Declaration] -> Sem r [Renamed.Declaration]
sortDeclarations = pure

rename :: Rename r => Module Desugared -> Sem r (Module Renamed)
rename =
    traverseOf
        (_Module @Desugared @Renamed . unlocated)
        ( \m' -> do
            addImportsToContext (m' ^. imports)
            traverseOf_ (declarations . each) (addDeclarationToContext False) m' -- add our own declarations to the context
            exposing' <- renameExposing (m' ^. name . unlocated) (m' ^. exposing)
            imports' <- traverse renameImport (m' ^. imports)
            declarations' <- traverse renameDeclaration (m' ^. declarations)
            sorted <- sortDeclarations declarations'
            pure (Module' (m' ^. name) exposing' imports' sorted)
        )
  where
    renameExposing :: Rename r => ModuleName -> Exposing Desugared -> Sem r (Exposing Renamed)
    renameExposing _ ExposingAll = pure ExposingAll
    renameExposing mn (ExposingSome es) = ExposingSome <$> traverse (renameExposition mn) es

    renameExposition :: Rename r => ModuleName -> Exposition Desugared -> Sem r (Exposition Renamed)
    renameExposition mn (ExposedValue vn) = ExposedValue <$> traverse (qualifyIn mn) vn
    renameExposition mn (ExposedOp opn) = ExposedOp <$> traverse (qualifyIn mn) opn
    renameExposition mn (ExposedType tn) = ExposedType <$> traverse (qualifyIn mn) tn
    renameExposition mn (ExposedTypeAndAllConstructors tn) = ExposedTypeAndAllConstructors <$> traverse (qualifyIn mn) tn

    renameImport :: Rename r => Import Desugared -> Sem r (Import Renamed)
    renameImport = traverseOf (_Import @Desugared @Renamed . unlocated) renameImport'

    renameImport' :: Rename r => Import' Desugared -> Sem r (Import' Renamed)
    renameImport' imp = do
        exposing' <- renameExposing (imp ^. importing . unlocated) (imp ^. exposing)
        pure $ Import' (imp ^. importing) (imp ^. as) (imp ^. qualified) exposing'

addImportsToContext :: Rename r => [Import Desugared] -> Sem r ()
addImportsToContext = traverse_ addImportToContext
  where
    addImportToContext :: Rename r => Import Desugared -> Sem r ()
    addImportToContext imp = do
        modules <- ask
        imported <- note (UnknownModule (imp ^. importing . unlocated)) $ moduleFromName (imp ^. importing . unlocated) modules
        let isExposingL = Desugared._Declaration . unlocated . name . unlocated . to (isExposingAndExists imported)
        let exposed = case imported ^. exposing of
                ExposingAll -> imported ^. declarations
                ExposingSome _ -> imported ^.. declarations . folded . filteredBy isExposingL
        traverse_ (addDeclarationToContext (imp ^. qualified)) exposed

addDeclarationToContext :: Rename r => Bool -> Desugared.Declaration -> Sem r ()
addDeclarationToContext _ decl = do
    let global :: name -> VarRef name
        global vn = Global (Qualified vn (decl ^. moduleName . unlocated) <$ decl ^. Desugared._Declaration)
    case decl ^. name . unlocated of
        NVarName vn -> modify $ over varNames $ Map.insert vn (global vn)
        NTypeName vn -> modify $ over typeNames $ Map.insert vn (global vn)
        NOpName vn -> modify $ over varNames $ Map.insert (OperatorVarName vn) (global (OperatorVarName vn))

    case decl ^. Desugared._Declaration . unlocated . Desugared.declaration'Body . Desugared._DeclarationBody . unlocated of
        -- Add all the constructor names to the context
        Desugared.TypeDeclaration _ (Located _ (Desugared.ADT constructors)) ->
            traverseOf_ (each . _1 . unlocated) (\tn -> modify $ over typeNames $ Map.insert tn (global tn)) constructors
        _ -> pass

ensureExistsAndExposed :: Rename r => ModuleName -> Located Name -> Sem r ()
ensureExistsAndExposed mn n = do
    modules <- ask
    case moduleFromName mn modules of
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

renameDeclaration :: Rename r => Desugared.Declaration -> Sem r Renamed.Declaration
renameDeclaration (Desugared.Declaration ld) = Renamed.Declaration <$> traverseOf unlocated renameDeclaration' ld
  where
    renameDeclaration' :: Rename r => Desugared.Declaration' -> Sem r Renamed.Declaration'
    renameDeclaration' fd = do
        let name' = sequenceA (traverseOf unlocated (`Qualified` (fd ^. Desugared.declaration'Module' . unlocated)) (fd ^. name))
        body' <- renameDeclarationBody (fd ^. Desugared.declaration'Body)
        pure $ Renamed.Declaration' (fd ^. Desugared.declaration'Module') name' body'

    renameDeclarationBody :: Rename r => Desugared.DeclarationBody -> Sem r Renamed.DeclarationBody
    renameDeclarationBody (Desugared.DeclarationBody ldb) = Renamed.DeclarationBody <$> traverseOf unlocated renameDeclarationBody' ldb

    renameDeclarationBody' :: Rename r => Desugared.DeclarationBody' -> Sem r Renamed.DeclarationBody'
    renameDeclarationBody' (Desugared.Value val ty) = do
        val' <- renameExpr val
        ty' <- traverse (traverse (renameType True)) ty
        pure $ Renamed.Value val' ty'
    renameDeclarationBody' (Desugared.TypeDeclaration vars ty) = do
        vars' <- traverse uniquify vars
        let varAliases = zip vars vars' :: [(Located LowerAlphaName, Located (Unique LowerAlphaName))]
        let addAllVarAliases s =
                foldl'
                    (\s' (vn, uniqueVn) -> typeVars %~ Map.insert (vn ^. unlocated) (uniqueVn ^. unlocated) $ s')
                    s
                    varAliases
        let declModuleName = ld ^. unlocated . unlocatedModuleName
        withModified addAllVarAliases $ do
            ty' <- traverseOf unlocated (renameTypeDeclaration declModuleName) ty
            pure $ Renamed.TypeDeclaration vars' ty'
    renameDeclarationBody' (Desugared.NativeDef _) = throw (NativeDefUnsupported ld)

renameTypeDeclaration :: Rename r => ModuleName -> Desugared.TypeDeclaration -> Sem r Renamed.TypeDeclaration
renameTypeDeclaration _ (Desugared.Alias t) = do
    t' <- traverseOf unlocated (renameType False) t
    pure $ Renamed.Alias t'
renameTypeDeclaration thisMod (Desugared.ADT constructors) = do
    constructors' <-
        traverse
            (\(n, y) -> (over unlocated (`Qualified` thisMod) n,) <$> traverseOf (each . unlocated) (renameType False) y)
            constructors
    pure $ Renamed.ADT constructors'

-- | Renames a type, qualifying type constructors and type variables where necessary
renameType ::
    Rename r =>
    -- | If new type variables are allowed - if False, this will throw an error if a type variable is not in scope
    -- This is useful for type declarations, where something like @type Invalid a = b@ would clearly be invalid
    -- But for local type annotations, we want to allow this, as it may be valid
    Bool ->
    Desugared.Type ->
    Sem r Renamed.Type
renameType allowNewTypeVars (Desugared.TypeVar n) = do
    inCtx <- lookupTypeVar n -- find the type variable in the context, if it exists
    case inCtx of
        Just inCtx' -> pure $ Renamed.TypeVar inCtx' -- if it exists, use the unique name
        Nothing
            | allowNewTypeVars ->
                -- if it doesn't exist, and we're allowed to make new type variables
                Renamed.TypeVar <$> makeUnique n
            | otherwise -> throw $ UnknownTypeVariable n
renameType antv (Desugared.FunctionType t1 t2) = Renamed.FunctionType <$> traverseOf unlocated (renameType antv) t1 <*> traverseOf unlocated (renameType antv) t2
renameType _ Desugared.UnitType = pure Renamed.UnitType
renameType antv (Desugared.TypeConstructorApplication t1 t2) = Renamed.TypeConstructorApplication <$> traverseOf unlocated (renameType antv) t1 <*> traverseOf unlocated (renameType antv) t2
renameType _ (Desugared.UserDefinedType ln) = Renamed.UserDefinedType <$> qualifyTypeName ln
renameType antv (Desugared.RecordType ln) = Renamed.RecordType <$> traverse (traverseOf (_2 . unlocated) (renameType antv)) ln
renameType antv (Desugared.TupleType ts) = Renamed.TupleType <$> traverse (traverseOf unlocated (renameType antv)) ts

renameExpr :: Rename r => Desugared.Expr -> Sem r Renamed.Expr
renameExpr (Desugared.Expr le) = Renamed.Expr <$> traverseOf unlocated renameExpr' le
  where
    renameExpr' :: Rename r => Desugared.Expr' -> Sem r Renamed.Expr'
    renameExpr' (Desugared.Int i) = pure $ Renamed.Int i
    renameExpr' (Desugared.Float i) = pure $ Renamed.Float i
    renameExpr' (Desugared.String i) = pure $ Renamed.String i
    renameExpr' (Desugared.Char i) = pure $ Renamed.Char i
    renameExpr' Desugared.Unit = pure Renamed.Unit
    renameExpr' (Desugared.Var i) = Renamed.Var <$> lookupVarName i
    renameExpr' (Desugared.Constructor i) = Renamed.Constructor <$> lookupTypeName i
    renameExpr' (Desugared.Lambda pat e) = do
        renameLambda pat e
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
        withModified (varNames %~ Map.insert (vn ^. unlocated) (Local vn')) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure $ Renamed.LetIn vn' exp' body'
    renameExpr' (Desugared.Match e cases) = do
        e' <- renameExpr e
        cases' <- traverse (bitraverse renamePattern renameExpr) cases
        pure $ Renamed.Match e' cases'
    renameExpr' (Desugared.Block es) = Renamed.Block <$> traverse renameExpr es
    renameExpr' (Desugared.InParens es) = Renamed.InParens <$> renameExpr es
    renameExpr' (Desugared.Tuple es) = Renamed.Tuple <$> traverse renameExpr es

renameBinaryOperator :: Rename r => Desugared.BinaryOperator -> Sem r Renamed.BinaryOperator
renameBinaryOperator (Desugared.MkBinaryOperator op) = Renamed.MkBinaryOperator <$> traverseOf unlocated renameBinaryOperator' op
  where
    renameBinaryOperator' :: Rename r => Desugared.BinaryOperator' -> Sem r Renamed.BinaryOperator'
    renameBinaryOperator' (Desugared.Op o) = do
        op' <- lookupVarName (OperatorVarName <<$>> o)
        let onlyOpName (OperatorVarName o') = o'
            onlyOpName _ = error "renameBinaryOperator': I really don't like this"
        let op'' = onlyOpName <<$>> op'
        pure $ Renamed.Op op''
    renameBinaryOperator' (Desugared.Infixed o) = do
        op' <- lookupVarName o
        pure $ Renamed.Infixed op'

renamePattern :: Rename r => Desugared.Pattern -> Sem r Renamed.Pattern
renamePattern (Desugared.Pattern fp) = Renamed.Pattern <$> traverseOf unlocated renamePattern' fp
  where
    renamePattern' :: Rename r => Desugared.Pattern' -> Sem r Renamed.Pattern'
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

{- | Estimates a var name from a pattern
This isn't really necessary as names will be uniquified anyway, but it could make dumped code more readable
-}
patternToVarName :: Desugared.Pattern -> VarName
patternToVarName (Desugared.Pattern (Located _ p)) =
    let mn = NormalVarName . LowerAlphaName
     in case p of
            Desugared.WildcardPattern -> mn "wildcard"
            Desugared.ListPattern _ -> mn "list"
            Desugared.VarPattern vn -> vn ^. unlocated
            Desugared.IntegerPattern _ -> mn "int"
            Desugared.FloatPattern _ -> mn "float"
            Desugared.StringPattern _ -> mn "string"
            Desugared.CharPattern _ -> mn "char"
            Desugared.ConstructorPattern _ _ -> mn "constructor"

patternToMatch :: Rename r => Desugared.Pattern -> Desugared.Expr -> Sem r (Located (Unique VarName), Renamed.Expr)
-- Special case, no match needed
-- We can just turn \x -> x into \x -> x
patternToMatch (Desugared.Pattern (Located _ (Desugared.VarPattern vn))) body = do
    uniqueVn <- uniquify vn
    body' <- withModified (varNames %~ Map.insert (vn ^. unlocated) (Local uniqueVn)) $ renameExpr body
    pure (uniqueVn, body')
patternToMatch pat body = do
    let vn = patternToVarName pat
    let patLocation = pat ^. Desugared._Pattern . sourceRegion
    let bodyLocation = body ^. Desugared._Expr . sourceRegion
    uniqueVn <- uniquify (Located patLocation vn)
    let varRef = Local uniqueVn `withLocationOf` uniqueVn
    pat' <- renamePattern pat
    body' <- renameExpr body
    let match =
            Renamed.Match
                (Renamed.Expr (Renamed.Var varRef `withLocationOf` uniqueVn))
                [(pat', body')]

    pure (uniqueVn, Renamed.Expr $ Located (enclosingRegion' patLocation bodyLocation) match)

{- | Rename a lambda expression
This is a little bit special because patterns have to be convered to match expressions
For example, @\(a, b) -> a@  becomes @\ab_ -> match ab_ with (a, b) -> a@
-}
renameLambda :: Rename r => Desugared.Pattern -> Desugared.Expr -> Sem r Renamed.Expr'
renameLambda p e = do
    (arg, match) <- patternToMatch p e
    pure (Renamed.Lambda arg match)
