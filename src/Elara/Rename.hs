{-# LANGUAGE PartialTypeSignatures #-}

module Elara.Rename where

import Control.Lens (Each (each), Getter, filteredBy, folded, makeLenses, over, to, traverseOf, traverseOf_, (%~), (^.), (^..), _1, _2)
import Data.Generics.Product
import Data.Generics.Wrapped
import Data.Map qualified as Map
import Elara.AST.Desugared
import Elara.AST.Generic
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (MaybeQualified), ModuleName, Name (NOpName, NTypeName, NVarName), Qualified (Qualified), ToName (toName), TypeName, VarName (NormalVarName, OperatorVarName))
import Elara.AST.Region (Located (Located), enclosingRegion', sourceRegion, sourceRegionToDiagnosePosition, unlocated, withLocationOf)
import Elara.AST.Renamed
import Elara.AST.Select (LocatedAST (Desugared, Renamed))
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
    | NativeDefUnsupported (Located DesugaredDeclaration')
    deriving (Show)

instance Exception RenameError
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
                    ("Element" <+> n ^. unlocated . to pretty <+> "does not exist in in module" <+> pretty m)
                    [(nPos, This "referenced here")]
                    []
    report (UnknownTypeVariable n) =
        writeReport $
            Err
                Nothing
                ("Unknown type variable: " <> pretty n)
                []
                []
    report (UnknownName n) =
        writeReport $
            Err
                Nothing
                ("Unknown name: " <> pretty n)
                [(n ^. sourceRegion . to sourceRegionToDiagnosePosition, This "referenced here")]
                []
    report (NativeDefUnsupported _) =
        writeReport $
            Err
                Nothing
                "Native definitions are not supported"
                []
                []

data RenameState = RenameState
    { varNames :: Map VarName (VarRef VarName)
    , typeNames :: Map TypeName (VarRef TypeName)
    , typeVars :: Map LowerAlphaName (Unique LowerAlphaName)
    -- ^ All field' type variables in scope
    }
    deriving (Show, Generic)

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
    typeNames' <- use' (field' @"typeNames")
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
lookupVarName = lookupGenericName (field' @"varNames")

lookupTypeName :: Rename r => Located (MaybeQualified TypeName) -> Sem r (Located (Qualified TypeName))
lookupTypeName n =
    lookupGenericName (field' @"typeNames") n <<&>> \case
        Local _ -> error "can't have local type names"
        Global v -> v ^. unlocated

lookupTypeVar :: Rename r => LowerAlphaName -> Sem r (Maybe (Unique LowerAlphaName))
lookupTypeVar n = do
    typeVars' <- use' (field' @"typeVars")
    pure $ Map.lookup n typeVars'

uniquify :: Rename r => Located name -> Sem r (Located (Unique name))
uniquify (Located sr n) = Located sr <$> makeUnique n

-- | Performs a topological sort of field' declarations, so as many
sortDeclarations :: [RenamedDeclaration] -> Sem r [RenamedDeclaration]
sortDeclarations = pure

rename :: Rename r => Module 'Desugared -> Sem r (Module 'Renamed)
rename =
    traverseOf
        (_Unwrapped . unlocated)
        ( \m' -> do
            addImportsToContext (m' ^. field' @"imports")
            traverseOf_ (field' @"declarations" . each) (addDeclarationToContext False) m' -- add our own declarations to field' context
            exposing' <- renameExposing (m' ^. field' @"name" . unlocated) (m' ^. field' @"exposing")
            imports' <- traverse renameImport (m' ^. field' @"imports")
            declarations' <- traverse renameDeclaration (m' ^. field' @"declarations")
            sorted <- sortDeclarations declarations'
            pure (Module' (m' ^. field' @"name") exposing' imports' sorted)
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
    renameImport = traverseOf (_Unwrapped . unlocated) renameImport'

    renameImport' :: Rename r => Import' Desugared -> Sem r (Import' Renamed)
    renameImport' imp = do
        exposing' <- renameExposing (imp ^. field' @"importing" . unlocated) (imp ^. field' @"exposing")
        pure $ Import' (imp ^. field' @"importing") (imp ^. field' @"as") (imp ^. field' @"qualified") exposing'

addImportsToContext :: Rename r => [Import Desugared] -> Sem r ()
addImportsToContext = traverse_ addImportToContext
  where
    addImportToContext :: Rename r => Import Desugared -> Sem r ()
    addImportToContext imp = do
        modules <- ask
        imported <- note (UnknownModule (imp ^. _Unwrapped . unlocated . field' @"importing" . unlocated)) $ moduleFromName (imp ^. _Unwrapped . unlocated . field' @"importing" . unlocated) modules
        let isExposingL = _Unwrapped . unlocated . field' @"name" . unlocated . to (isExposingAndExists imported)
        let exposed = case imported ^. _Unwrapped . unlocated . field' @"exposing" of
                ExposingAll -> imported ^. _Unwrapped . unlocated . field' @"declarations"
                ExposingSome _ -> imported ^.. _Unwrapped . unlocated . field' @"declarations" . folded . filteredBy isExposingL
        traverse_ (addDeclarationToContext (imp ^. _Unwrapped . unlocated . field' @"qualified")) exposed

addDeclarationToContext :: Rename r => Bool -> DesugaredDeclaration -> Sem r ()
addDeclarationToContext _ decl = do
    let global :: name -> VarRef name
        global vn = Global (Qualified vn (decl ^. _Unwrapped . unlocated . field' @"moduleName" . unlocated) <$ decl ^. _Unwrapped)
    case decl ^. _Unwrapped . unlocated . field' @"name" . unlocated of
        NVarName vn -> modify $ over (the @"varNames") $ Map.insert vn (global vn)
        NTypeName vn -> modify $ over (the @"typeNames") $ Map.insert vn (global vn)
        NOpName vn -> modify $ over (the @"varNames") $ Map.insert (OperatorVarName vn) (global (OperatorVarName vn))

    case decl ^. _Unwrapped . unlocated . field @"body" . _Unwrapped . unlocated of
        -- Add all the constructor names to field' context
        -- TypeDeclaration _ (Located _ (ADT constructors)) ->
        --     traverseOf_ (each . _1 . unlocated) (\tn -> modify $ over (the @"typeNames") $ Map.insert tn (global tn)) constructors
        _ -> pass

ensureExistsAndExposed :: Rename r => ModuleName -> Located Name -> Sem r ()
ensureExistsAndExposed mn n = do
    modules <- ask
    case moduleFromName mn modules of
        Nothing -> throw $ UnknownModule mn
        Just m -> do
            unless (elementExistsInModule m (n ^. unlocated)) $ throw $ NonExistentModuleDeclaration mn n
            unless (isExposingAndExists m (n ^. unlocated)) $ throw $ UnknownName n

elementExistsInModule :: Module 'Desugared -> Name -> Bool
elementExistsInModule m' n' = any (\d -> d ^. _Unwrapped . unlocated . field' @"name" . unlocated == n') (m' ^. _Unwrapped . unlocated . field' @"declarations")

isExposingAndExists :: Module 'Desugared -> Name -> Bool
isExposingAndExists m n =
    let mn = m ^. _Unwrapped . unlocated . field' @"name" . unlocated
     in case m ^. _Unwrapped . unlocated . field' @"exposing" of
            ExposingAll -> elementExistsInModule m n
            ExposingSome es -> elementExistsInModule m n && any (isExposition mn n) es
  where
    isExposition :: ModuleName -> Name -> Exposition 'Desugared -> Bool
    isExposition mn (NVarName vn) (ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
    isExposition mn (NOpName opn) (ExposedOp opn') = MaybeQualified opn (Just mn) == opn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False

renameDeclaration :: Rename r => DesugaredDeclaration -> Sem r RenamedDeclaration
renameDeclaration (Declaration ld) = Declaration <$> traverseOf unlocated renameDeclaration' ld
  where
    renameDeclaration' :: Rename r => DesugaredDeclaration' -> Sem r RenamedDeclaration'
    renameDeclaration' fd = do
        let name' = sequenceA (traverseOf unlocated (`Qualified` (fd ^. field' @"moduleName" . unlocated)) (fd ^. field' @"name"))
        body' <- renameDeclarationBody (fd ^. field' @"body")

        pure $ Declaration' (fd ^. field' @"moduleName") name' body'

    renameDeclarationBody :: Rename r => DesugaredDeclarationBody -> Sem r RenamedDeclarationBody
    renameDeclarationBody (DeclarationBody ldb) = DeclarationBody <$> traverseOf unlocated renameDeclarationBody' ldb

    renameDeclarationBody' :: Rename r => DesugaredDeclarationBody' -> Sem r RenamedDeclarationBody'
    renameDeclarationBody' (Value val _ ty) = do
        val' <- renameExpr val
        ty' <- traverse (traverseOf (_Unwrapped . unlocated) (renameType True)) ty
        pure $ Value val' NoFieldValue ty'
    renameDeclarationBody' (TypeDeclaration vars ty) = do
        vars' <- traverse uniquify vars
        let varAliases = zip vars vars' :: [(Located LowerAlphaName, Located (Unique LowerAlphaName))]
        let addAllVarAliases s =
                foldl'
                    (\s' (vn, uniqueVn) -> the @"typeVars" %~ Map.insert (vn ^. unlocated) (uniqueVn ^. unlocated) $ s')
                    s
                    varAliases
        let declModuleName = ld ^. unlocated . field' @"moduleName" . unlocated
        withModified addAllVarAliases $ do
            ty' <- traverseOf unlocated (renameTypeDeclaration declModuleName) ty
            pure $ TypeDeclaration vars' ty'

renameTypeDeclaration :: Rename r => ModuleName -> DesugaredTypeDeclaration -> Sem r RenamedTypeDeclaration
renameTypeDeclaration _ (Alias t) = do
    t' <- traverseOf (_Unwrapped . unlocated) (renameType False) t
    pure $ Alias t'
renameTypeDeclaration thisMod (ADT constructors) = do
    constructors' <-
        traverse
            (\(n, y) -> (over unlocated (`Qualified` thisMod) n,) <$> traverseOf (each . _Unwrapped . unlocated) (renameType False) y)
            constructors
    pure $ ADT constructors'

-- | Renames a type, qualifying type constructors and type variables where necessary
renameType ::
    Rename r =>
    -- | If new type variables are allowed - if False, this will throw an error if a type variable is not in scope
    -- This is useful for type declarations, where something like @type Invalid a = b@ would clearly be invalid
    -- But for local type annotations, we want to allow this, as it may be valid
    Bool ->
    DesugaredType' ->
    Sem r RenamedType'
renameType allowNewTypeVars (TypeVar (Located sr n)) = do
    inCtx <- lookupTypeVar n -- find field' type variable in the context, if it exists
    case inCtx of
        Just inCtx' -> pure $ TypeVar (Located sr inCtx') -- if it exists, use the unique name
        Nothing
            | allowNewTypeVars -> do
                -- if it doesn't exist, and we're allowed to make new type variables
                uniqueN <- makeUnique n -- make a new unique name
                modify $ over (the @"typeVars") $ Map.insert n uniqueN -- add it to the context
                pure (TypeVar $ Located sr uniqueN)
            | otherwise -> throw $ UnknownTypeVariable n
renameType antv (FunctionType t1 t2) = FunctionType <$> traverseOf (_Unwrapped . unlocated) (renameType antv) t1 <*> traverseOf (_Unwrapped . unlocated) (renameType antv) t2
renameType _ UnitType = pure UnitType
renameType antv (TypeConstructorApplication t1 t2) = TypeConstructorApplication <$> traverseOf (_Unwrapped . unlocated) (renameType antv) t1 <*> traverseOf (_Unwrapped . unlocated) (renameType antv) t2
renameType _ (UserDefinedType ln) = UserDefinedType <$> qualifyTypeName ln
renameType antv (RecordType ln) = RecordType <$> traverse (traverseOf (_2 . (_Unwrapped . unlocated)) (renameType antv)) ln
renameType antv (TupleType ts) = TupleType <$> traverse (traverseOf (_Unwrapped . unlocated) (renameType antv)) ts
renameType antv (ListType t) = ListType <$> traverseOf (_Unwrapped . unlocated) (renameType antv) t

renameExpr :: Rename r => DesugaredExpr -> Sem r RenamedExpr
renameExpr (Expr le) =
    Expr
        <$> bitraverse
            (traverseOf unlocated renameExpr')
            (traverse (traverseOf (_Unwrapped . unlocated) (renameType False)))
            le
  where
    renameExpr' :: Rename r => DesugaredExpr' -> Sem r RenamedExpr'
    renameExpr' (Int i) = pure $ Int i
    renameExpr' (Float i) = pure $ Float i
    renameExpr' (String i) = pure $ String i
    renameExpr' (Char i) = pure $ Char i
    renameExpr' Unit = pure Unit
    renameExpr' (Var i) = Var <$> lookupVarName i
    renameExpr' (Constructor i) = Constructor <$> lookupTypeName i
    renameExpr' (Lambda pat e) = renameLambda (pat) e
    renameExpr' (FunctionCall e1 e2) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        pure $ FunctionCall e1' e2'
    renameExpr' (If e1 e2 e3) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        e3' <- renameExpr e3
        pure $ If e1' e2' e3'
    renameExpr' (BinaryOperator op left right) = do
        op' <- renameBinaryOperator op
        left' <- renameExpr left
        right' <- renameExpr right
        pure $ BinaryOperator op' left' right'
    renameExpr' (List es) = List <$> traverse renameExpr es
    renameExpr' (Let vn _ e) = do
        vn' <- uniquify vn
        modify (the @"varNames" %~ Map.insert (vn ^. unlocated) (Local vn'))
        exp' <- renameExpr e
        pure $ Let vn' NoFieldValue exp'
    renameExpr' (LetIn vn _ e body) = do
        vn' <- uniquify vn
        withModified (the @"varNames" %~ Map.insert (vn ^. unlocated) (Local vn')) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure $ LetIn vn' NoFieldValue exp' body'
    renameExpr' (Match e cases) = do
        e' <- renameExpr e
        cases' <- traverse (bitraverse renamePattern renameExpr) cases
        pure $ Match e' cases'
    renameExpr' (Block es) = Block <$> traverse renameExpr es
    renameExpr' (InParens es) = InParens <$> renameExpr es
    renameExpr' (Tuple es) = Tuple <$> traverse renameExpr es

renameBinaryOperator :: Rename r => DesugaredBinaryOperator -> Sem r RenamedBinaryOperator
renameBinaryOperator (MkBinaryOperator op) = MkBinaryOperator <$> traverseOf unlocated renameBinaryOperator' op
  where
    renameBinaryOperator' :: Rename r => DesugaredBinaryOperator' -> Sem r RenamedBinaryOperator'
    renameBinaryOperator' (SymOp o) = do
        op' <- lookupVarName (OperatorVarName <<$>> o)
        let onlyOpName (OperatorVarName o') = o'
            onlyOpName _ = error "renameBinaryOperator': I really don't like this"
        let op'' = onlyOpName <<$>> op'
        pure $ SymOp op''
    renameBinaryOperator' (Infixed o) = do
        op' <- lookupVarName o
        pure $ Infixed op'

renamePattern :: Rename r => DesugaredPattern -> Sem r RenamedPattern
renamePattern (Pattern fp) = (\x -> Pattern (x, _)) <$> traverseOf unlocated renamePattern' (fp ^. _1)
  where
    renamePattern' :: Rename r => DesugaredPattern' -> Sem r RenamedPattern'
    renamePattern' (IntegerPattern i) = pure $ IntegerPattern i
    renamePattern' (FloatPattern i) = pure $ FloatPattern i
    renamePattern' (StringPattern i) = pure $ StringPattern i
    renamePattern' (CharPattern i) = pure $ CharPattern i
    renamePattern' WildcardPattern = pure WildcardPattern
    renamePattern' (ListPattern ps) = ListPattern <$> traverse renamePattern ps
    renamePattern' (ConsPattern p1 p2) = ConsPattern <$> renamePattern p1 <*> renamePattern p2
    renamePattern' (VarPattern vn) = do
        vn' <- uniquify vn
        modify (the @"varNames" %~ Map.insert (vn ^. unlocated) (Local vn'))
        pure $ VarPattern vn'
    renamePattern' (ConstructorPattern cn ps) = do
        cn' <- qualifyTypeName cn
        ps' <- traverse renamePattern ps
        pure $ ConstructorPattern cn' ps'

{- | Estimates a var name from a pattern
 This isn't really necessary as names will be uniquified anyway, but it could make dumped code more readable
-}
patternToVarName :: DesugaredPattern -> VarName
patternToVarName (Pattern (Located _ p, _)) =
    let mn = NormalVarName . LowerAlphaName
     in case p of
            WildcardPattern -> mn "wildcard"
            ListPattern _ -> mn "list"
            VarPattern vn -> vn ^. unlocated
            IntegerPattern _ -> mn "int"
            FloatPattern _ -> mn "float"
            StringPattern _ -> mn "string"
            CharPattern _ -> mn "char"
            ConstructorPattern _ _ -> mn "constructor"
            ConsPattern _ _ -> mn "cons"
            UnitPattern -> "unit"

patternToMatch :: Rename r => DesugaredPattern -> DesugaredExpr -> Sem r (Located (Unique VarName), RenamedExpr)
-- Special case, no match needed
-- We can just turn \x -> x into \x -> x
patternToMatch (Pattern (Located _ (VarPattern vn), _)) body = do
    uniqueVn <- uniquify vn
    body' <- withModified (the @"varNames" %~ Map.insert (vn ^. unlocated) (Local uniqueVn)) $ renameExpr body
    pure (uniqueVn, body')
patternToMatch pat body = do
    let vn = patternToVarName pat
    let patLocation = pat ^. _Unwrapped . _1 . sourceRegion
    let bodyLocation = body ^. _Unwrapped . _1 . sourceRegion
    uniqueVn <- uniquify (Located patLocation vn)
    let varRef = Local uniqueVn `withLocationOf` uniqueVn
    pat' <- renamePattern pat
    body' <- renameExpr body
    let match =
            Match
                (Expr (Var varRef `withLocationOf` uniqueVn, _))
                [(pat', body')]

    pure (uniqueVn, Expr (Located (enclosingRegion' patLocation bodyLocation) match, _))

{- | Rename a lambda expression
 This is a little bit special because patterns have to be converted to match expressions

 For example,
    @\(a, b) -> a@  becomes @\ab_ -> match ab_ with (a, b) -> a@
-}
renameLambda :: Rename r => DesugaredPattern -> DesugaredExpr -> Sem r RenamedExpr'
renameLambda p e = do
    (arg, match) <- patternToMatch p e
    pure (Lambda arg match)
