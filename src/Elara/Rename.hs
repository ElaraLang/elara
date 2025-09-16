{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}

{- | Renaming stage of compilation
This stage handles:
1. Renaming all variables, types, and type variables, adding module qualification or unique suffixes to avoid name clashes
2. Desugaring any "first-class" pattern matches into normal match expressions (eg '\[] -> 1' to '\x -> match x with [] -> 1')
3. Desugaring blocks into let-in chains (and monad operations soon), eg 'let y = 1; y + 1' to 'let y = 1 in y + 1'
   Note that until the monad operations are implemented, we can't fully remove blocks, as we have nothing to translate 'f x; g x' into
-}
module Elara.Rename where

import Data.Generics.Product hiding (list)
import Data.Generics.Wrapped
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Effectful.Reader.Static qualified as Eff
import Effectful.State.Extra
import Effectful.State.Static.Local qualified as Eff
import Elara.AST.Desugared
import Elara.AST.Generic hiding (moduleName)
import Elara.AST.Generic.Common
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (MaybeQualified), ModuleName, Name (NTypeName, NVarName), Qualified (Qualified), ToName (toName), TypeName (..), VarName (NormalVarName, OperatorVarName), VarOrConName (..))
import Elara.AST.Region (Located (Located), enclosingRegion', sourceRegion, spanningRegion', unlocated, withLocationOf)
import Elara.AST.Renamed
import Elara.AST.Select (LocatedAST (Desugared, Renamed))
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local))
import Elara.Data.TopologicalGraph
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.Desugar.Error (DesugarError)
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug)
import Elara.Prim.Core (consCtorName, emptyListCtorName, tuple2CtorName)
import Elara.Query qualified
import Elara.Query.Effects
import Elara.Rename.Error
import Elara.Rename.Imports (isImportedBy)
import Optics (anyOf, filteredBy, traverseOf_)
import Polysemy.Error (Error)
import Polysemy.Reader hiding (Local)
import Polysemy.State
import Rock qualified

type RenamePipelineEffects =
    '[ State RenameState
     , Error RenameError
     , Reader (TopologicalGraph (Module 'Desugared))
     , UniqueGen
     , StructuredDebug
     ]

type Rename r =
    ( Eff.State RenameState :> r
    , Eff.Error RenameError :> r
    , UniqueGen :> r
    , QueryEffects r
    , StructuredDebug :> r
    , Rock.Rock Elara.Query.Query :> r
    )

type InnerRename r =
    ( Eff.State RenameState :> r
    , Eff.Error RenameError :> r
    , UniqueGen :> r
    , QueryEffects r
    , StructuredDebug :> r
    , Eff.Reader (Maybe (Module 'Desugared)) :> r -- the module we're renaming
    )

getRenamedModule ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Eff.Error RenameError, Eff.State RenameState, Rock.Rock Elara.Query.Query])
        (Module 'Renamed)
getRenamedModule mn = do
    m <- runErrorOrReport @DesugarError $ Rock.fetch $ Elara.Query.DesugaredModule mn
    rename m

-- runRenamePipeline ::
--     IsPipeline r =>
--     TopologicalGraph (Module 'Desugared) ->
--     RenameState ->
--     Eff (EffectsAsPrefixOf RenamePipelineEffects r) a ->
--     Eff r a
-- runRenamePipeline graph st =
--     subsume
--         . uniqueGenToIO
--         . runReader graph
--         . runErrorOrReport @RenameError
--         . evalState st

qualifyIn :: Rename r => ModuleName -> MaybeQualified name -> Eff r (Qualified name)
qualifyIn mn (MaybeQualified n (Just m)) = do
    when (m /= mn) $ throwError $ QualifiedInWrongModule m mn
    pure $ Qualified n m
qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

qualifyTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => Located (MaybeQualified TypeName) -> Eff r (Located (Qualified TypeName))
qualifyTypeName (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed m (Located sr (NTypeName n))
    pure $ Located sr (Qualified n m)
qualifyTypeName (Located sr (MaybeQualified n Nothing)) = do
    typeNames' <- use' (field' @"typeNames")
    m <- Eff.ask
    case Map.lookup n typeNames' of
        Nothing -> throwError $ UnknownName (Located sr (NTypeName n)) m typeNames'
        Just ((Global (Located sr' (Qualified n' m))) :| []) -> pure $ Located sr' (Qualified n' m)
        Just ((Local _) :| []) -> error "can't have local type names"
        Just many -> throwError $ AmbiguousTypeName (Located sr (NTypeName n)) many

askCurrentModule :: InnerRename r => Eff r (Module 'Desugared)
askCurrentModule = do
    m <- Eff.ask
    case m of
        Nothing -> throwError UnknownCurrentModule
        Just m -> pure m

lookupGenericName ::
    (UniqueGen :> r, Rock.Rock Elara.Query.Query :> r, _) =>
    (Ord name, ToName name, Show name) =>
    Lens' RenameState (Map name (NonEmpty (VarRef name))) ->
    (Located Name -> NonEmpty (VarRef name) -> RenameError) ->
    Located (MaybeQualified name) ->
    Eff r (Located (VarRef name))
lookupGenericName _ _ (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed m (Located sr (toName n))
    pure $ Located sr $ Global (Located sr (Qualified n m))
lookupGenericName lens ambiguousError (Located sr (MaybeQualified n Nothing)) = do
    names' <- use' lens
    m <- Eff.ask
    case m of
        Nothing ->
            case Map.lookup n names' of
                Nothing -> throwError $ UnknownName (Located sr $ toName n) m names'
                Just (v :| []) -> pure $ Located sr v
                Just many -> throwError $ ambiguousError (Located sr $ toName n) many
        Just m -> case maybe [] (NonEmpty.filter ((m `isImportedBy`) . fmap toName)) (Map.lookup n names') of
            [v] -> pure $ Located sr v
            [] -> throwError $ UnknownName (Located sr $ toName n) (Just m) names'
            (x : xs) -> throwError $ ambiguousError (Located sr $ toName n) (x :| xs)

lookupVarName :: _ => Located (MaybeQualified VarName) -> Eff r (Located (VarRef VarName))
lookupVarName = lookupGenericName (field' @"varNames") AmbiguousVarName

lookupTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => Located (MaybeQualified TypeName) -> Eff r (Located (Qualified TypeName))
lookupTypeName n =
    lookupGenericName (field' @"typeNames") AmbiguousTypeName n <<&>> \case
        Local _ -> error "can't have local type names"
        Global v -> v ^. unlocated

lookupTypeVar :: _ => LowerAlphaName -> Eff r (Maybe (Unique LowerAlphaName))
lookupTypeVar n = do
    typeVars' <- use' (field' @"typeVars")
    pure $ Map.lookup n typeVars'

uniquify :: UniqueGen :> r => Located name -> Eff r (Located (Unique name))
uniquify (Located sr n) = Located sr <$> makeUnique n

-- | Performs a topological sort of field' declarations, so as many
sortDeclarations :: [RenamedDeclaration] -> Eff r [RenamedDeclaration]
sortDeclarations = pure

rename :: Rename r => Module 'Desugared -> Eff r (Module 'Renamed)
rename m = do
    -- debug $ "Renaming module " <> pretty (m ^. _Unwrapped % unlocated % field' @"name")
    traverseOf
        (_Unwrapped % unlocated)
        ( \m' -> do
            addImportsToContext (m' ^. field' @"imports")
            traverseOf_ (field' @"declarations" % each) (addDeclarationToContext False) m' -- add our own declarations to field' context
            exposing' <- renameExposing (m' ^. field' @"name" % unlocated) (m' ^. field' @"exposing")
            imports' <- traverse renameImport (m' ^. field' @"imports")
            declarations' <- Eff.runReader (Just m) (traverse renameDeclaration (m' ^. field' @"declarations"))
            sorted <- sortDeclarations declarations'
            pure (Module' (m' ^. field' @"name") exposing' imports' sorted)
        )
        m
  where
    renameExposing :: Rename r => ModuleName -> Exposing 'Desugared -> Eff r (Exposing 'Renamed)
    renameExposing _ ExposingAll = pure ExposingAll
    renameExposing mn (ExposingSome es) = ExposingSome <$> traverse (renameExposition mn) es

    renameExposition :: Rename r => ModuleName -> Exposition 'Desugared -> Eff r (Exposition 'Renamed)
    renameExposition mn (ExposedValue vn) = ExposedValue <$> traverse (qualifyIn mn) vn
    renameExposition mn (ExposedOp opn) = ExposedOp <$> traverse (qualifyIn mn) opn
    renameExposition mn (ExposedType tn) = ExposedType <$> traverse (qualifyIn mn) tn
    renameExposition mn (ExposedTypeAndAllConstructors tn) = ExposedTypeAndAllConstructors <$> traverse (qualifyIn mn) tn

    renameImport :: Rename r => Import 'Desugared -> Eff r (Import 'Renamed)
    renameImport = traverseOf (_Unwrapped % unlocated) renameImport'

    renameImport' :: Rename r => Import' 'Desugared -> Eff r (Import' 'Renamed)
    renameImport' imp = do
        exposing' <- renameExposing (imp ^. field' @"importing" % unlocated) (imp ^. field' @"exposing")
        pure $ Import' (imp ^. field' @"importing") (imp ^. field' @"as") (imp ^. field' @"qualified") exposing'

addImportsToContext :: Rename r => [Import 'Desugared] -> Eff r ()
addImportsToContext = traverse_ addImportToContext

addImportToContext :: Rename r => Import 'Desugared -> Eff r ()
addImportToContext imp =
    addModuleToContext
        (imp ^. _Unwrapped % unlocated % field' @"importing" % unlocated)
        (imp ^. _Unwrapped % unlocated % field' @"exposing")

getModuleFromName mn = do
    runErrorOrReport @DesugarError $
        Rock.fetch (Elara.Query.DesugaredModule mn)

addModuleToContext :: Rename r => ModuleName -> Exposing 'Desugared -> Eff r ()
addModuleToContext mn exposing = do
    imported <- getModuleFromName mn
    let isExposingL =
            declarationName
                % unlocated
                % to (isExposingAndExists imported)
    let exposed = case exposing of
            ExposingAll -> imported ^. _Unwrapped % unlocated % field' @"declarations"
            ExposingSome _ -> imported ^.. _Unwrapped % unlocated % field' @"declarations" % folded % filteredBy isExposingL
    traverse_ (addDeclarationToContext False) exposed

addDeclarationToContext :: Rename r => Bool -> DesugaredDeclaration -> Eff r ()
addDeclarationToContext _ decl = do
    -- for global declarations we can have many with the same name, so we need to merge them
    let insertMerging :: (Ord k, Eq a) => k -> a -> Map k (NonEmpty a) -> Map k (NonEmpty a)
        insertMerging k x = Map.insertWith ((NonEmpty.nub .) . (<>)) k (one x)

    let global :: name -> VarRef name
        global vn =
            Global
                ( Qualified vn (decl ^. _Unwrapped % unlocated % field' @"moduleName" % unlocated)
                    <$ decl ^. _Unwrapped
                )
    case decl ^. declarationName % unlocated of
        NVarName vn -> Eff.modify $ over (the @"varNames") $ insertMerging vn (global vn)
        NTypeName vn -> Eff.modify $ over (the @"typeNames") $ insertMerging vn (global vn)

    case decl ^. _Unwrapped % unlocated % field @"body" % _Unwrapped % unlocated of
        -- Add all the constructor names to field' context
        TypeDeclaration name _ (Located _ (ADT ctors)) _ -> do
            traverseOf_ (each % _1 % unlocated) (\tn -> Eff.modify $ over (the @"typeNames") $ insertMerging tn (global tn)) ctors
        _ -> pass

-- | Ensure that a name exists in the context and is exposed
ensureExistsAndExposed :: (Rock.Rock Elara.Query.Query :> r, _) => ModuleName -> Located Name -> Eff r ()
ensureExistsAndExposed mn n = do
    thisMod <- Eff.ask
    m <- getModuleFromName mn
    unless (elementExistsInModule m (n ^. unlocated)) $ throwError $ NonExistentModuleDeclaration mn n
    unless (isExposingAndExists m (n ^. unlocated)) $ throwError $ UnknownName @Name n thisMod mempty

elementExistsInModule :: Module 'Desugared -> Name -> Bool
elementExistsInModule m' n' =
    any
        ( \d ->
            d ^. declarationName % unlocated == n'
                || case d ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
                    TypeDeclaration name _ (Located _ (ADT ctors)) _ -> anyOf (each % _1 % unlocated) (\n -> NTypeName n == n') ctors
                    _ -> False
        )
        (m' ^. _Unwrapped % unlocated % field' @"declarations")

{- | Tests that n is exposed in m
I.e. that it is in the exposing list, or that the module is exposing everything
-}
isExposingAndExists :: Module 'Desugared -> Name -> Bool
isExposingAndExists m n =
    let mn = m ^. _Unwrapped % unlocated % field' @"name" % unlocated
     in case m ^. _Unwrapped % unlocated % field' @"exposing" of
            ExposingAll -> elementExistsInModule m n
            ExposingSome es -> elementExistsInModule m n && any (isExposition mn n) es
  where
    isExposition :: ModuleName -> Name -> Exposition 'Desugared -> Bool
    isExposition mn (NVarName vn) (ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False

renameDeclaration :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredDeclaration -> Eff r RenamedDeclaration
renameDeclaration decl@(Declaration ld) = Declaration <$> traverseOf unlocated renameDeclaration' ld
  where
    renameDeclaration' :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredDeclaration' -> Eff r RenamedDeclaration'
    renameDeclaration' fd = do
        -- qualify the name with the module name
        -- let name' =
        --         -- sequenceA @Qualified @Located
        --         over
        --             unlocated
        --             (\n -> Qualified n (fd ^. field' @"moduleName" % unlocated))
        --             (fd ^. declaration'Name)
        body' <- Eff.runReader (Just decl) $ renameDeclarationBody (fd ^. field' @"body")

        pure $ Declaration' (fd ^. field' @"moduleName") body'

    renameDeclarationBody :: (InnerRename r, Rock.Rock Elara.Query.Query :> r, Eff.Reader (Maybe DesugaredDeclaration) :> r) => DesugaredDeclarationBody -> Eff r RenamedDeclarationBody
    renameDeclarationBody (DeclarationBody ldb) = DeclarationBody <$> traverseOf unlocated renameDeclarationBody' ldb

    renameDeclarationBody' :: (InnerRename r, Rock.Rock Elara.Query.Query :> r, Eff.Reader (Maybe DesugaredDeclaration) :> r) => DesugaredDeclarationBody' -> Eff r RenamedDeclarationBody'
    renameDeclarationBody' (Value name val _ ty ann) = scoped $ do
        ty' <- traverse (traverseOf (_Unwrapped % _1 % unlocated) (renameType True)) ty
        val' <- renameExpr val
        let ann' = coerceValueDeclAnnotations ann
        thisModule <- askCurrentModule
        let qualifiedName =
                sequenceA $
                    Qualified name (thisModule ^. _Unwrapped % unlocated % field' @"name" % unlocated)
        pure $ Value qualifiedName val' NoFieldValue ty' ann'
    renameDeclarationBody' (TypeDeclaration name vars ty ann) = do
        vars' <- traverse uniquify vars
        let varAliases = zip vars vars' :: [(Located LowerAlphaName, Located (Unique LowerAlphaName))]
        let addAllVarAliases s =
                foldl'
                    (\s' (vn, uniqueVn) -> the @"typeVars" %~ Map.insert (vn ^. unlocated) (uniqueVn ^. unlocated) $ s')
                    s
                    varAliases
        let declModuleName = ld ^. unlocated % field' @"moduleName" % unlocated
        locally addAllVarAliases $ do
            ty' <- traverseOf unlocated (renameTypeDeclaration declModuleName) ty
            let ann' = coerceTypeDeclAnnotations ann
            thisModule <- askCurrentModule
            let qualifiedName =
                    sequenceA $
                        Qualified name (thisModule ^. _Unwrapped % unlocated % field' @"name" % unlocated)
            pure $ TypeDeclaration qualifiedName vars' ty' ann'

renameTypeDeclaration :: _ => ModuleName -> DesugaredTypeDeclaration -> Eff r RenamedTypeDeclaration
renameTypeDeclaration _ (Alias t) = do
    t' <- traverseOf (_Unwrapped % _1 % unlocated) (renameType False) t
    pure $ Alias t'
renameTypeDeclaration thisMod (ADT constructors) = do
    constructors' <-
        traverse
            (\(n, y) -> (over unlocated (`Qualified` thisMod) n,) <$> traverseOf (each % _Unwrapped % _1 % unlocated) (renameType False) y)
            constructors
    pure $ ADT constructors'

renameSimpleType :: _ => DesugaredType -> Eff r RenamedType
renameSimpleType = traverseOf (_Unwrapped % _1 % unlocated) (renameType False)

-- | Renames a type, qualifying type constructors and type variables where necessary
renameType ::
    (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
    {- | If new type variables are allowed - if False, this will throw an error if a type variable is not in scope
    This is useful for type declarations, where something like @type Invalid a = b@ would clearly be invalid
    But for local type annotations, we want to allow this, as it may be valid
    -}
    Bool ->
    DesugaredType' ->
    Eff r RenamedType'
renameType allowNewTypeVars (TypeVar (Located sr n)) = do
    inCtx <- lookupTypeVar n -- find field' type variable in the context, if it exists
    case inCtx of
        Just inCtx' -> pure $ TypeVar (Located sr inCtx') -- if it exists, use the unique name
        Nothing
            | allowNewTypeVars -> do
                -- if it doesn't exist, and we're allowed to make new type variables
                uniqueN <- makeUnique n -- make a new unique name
                Eff.modify $ over (the @"typeVars") $ Map.insert n uniqueN -- add it to the context
                pure (TypeVar $ Located sr uniqueN)
            | otherwise -> throwError $ UnknownTypeVariable n
renameType antv (FunctionType t1 t2) = FunctionType <$> traverseOf (_Unwrapped % _1 % unlocated) (renameType antv) t1 <*> traverseOf (_Unwrapped % _1 % unlocated) (renameType antv) t2
renameType _ UnitType = pure UnitType
renameType antv (TypeConstructorApplication t1 t2) = TypeConstructorApplication <$> traverseOf (_Unwrapped % _1 % unlocated) (renameType antv) t1 <*> traverseOf (_Unwrapped % _1 % unlocated) (renameType antv) t2
renameType _ (UserDefinedType ln) = UserDefinedType <$> qualifyTypeName ln
renameType antv (RecordType ln) = RecordType <$> traverse (traverseOf (_2 % _Unwrapped % _1 % unlocated) (renameType antv)) ln
renameType antv (TupleType ts) = TupleType <$> traverse (traverseOf (_Unwrapped % _1 % unlocated) (renameType antv)) ts
renameType antv (ListType t) = ListType <$> traverseOf (_Unwrapped % _1 % unlocated) (renameType antv) t

renameExpr :: (InnerRename r, Eff.Reader (Maybe DesugaredDeclaration) :> r, Rock.Rock Elara.Query.Query :> r) => DesugaredExpr -> Eff r RenamedExpr
renameExpr (Expr' (Block es)) = desugarBlock es
renameExpr e@(Expr' (Let{})) = desugarBlock (e :| [])
renameExpr (Expr le@(Located loc _, _)) =
    Expr
        <$> bitraverse
            (traverseOf unlocated renameExpr')
            (traverse (traverseOf (_Unwrapped % _1 % unlocated) (renameType False)))
            le
  where
    renameExpr' (Int i) = pure $ Int i
    renameExpr' (Float i) = pure $ Float i
    renameExpr' (String i) = pure $ String i
    renameExpr' (Char i) = pure $ Char i
    renameExpr' Unit = pure Unit
    renameExpr' (Var i) = Var <$> lookupVarName i
    renameExpr' (Constructor i) = Constructor <$> lookupTypeName i
    renameExpr' (Lambda pat e) = renameLambda pat e
    renameExpr' (FunctionCall e1 e2) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        pure $ FunctionCall e1' e2'
    renameExpr' (TypeApplication e1 t1) = do
        e1' <- renameExpr e1
        t1' <- traverseOf (_Unwrapped % _1 % unlocated) (renameType False) t1
        pure $ TypeApplication e1' t1'
    renameExpr' (If e1 e2 e3) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        e3' <- renameExpr e3
        pure $ If e1' e2' e3'
    renameExpr' (BinaryOperator (op, left, right)) = do
        op' <- renameBinaryOperator op
        left' <- renameExpr left
        right' <- renameExpr right
        pure $ BinaryOperator (op', left', right')
    renameExpr' (List []) = pure $ Constructor (Located loc emptyListCtorName)
    renameExpr' (List (x : xs)) = do
        xs' <- traverse renameExpr (x :| xs)
        let lastCons :: RenamedExpr =
                Expr (Constructor (Located loc emptyListCtorName) `withLocationOf` last xs', Nothing)
        let cons x y =
                -- create a cons of x and y i.e. (Cons x) y
                Expr
                    ( FunctionCall
                        ( Expr
                            ( FunctionCall
                                (Expr (Constructor (Located loc consCtorName) `withLocationOf` x, Nothing))
                                x
                                `withLocationOf` x
                            , Nothing
                            )
                        )
                        y
                        `withLocationOf` y
                    , Nothing
                    )
        let createConses :: [RenamedExpr] -> RenamedExpr
            createConses [] = lastCons
            createConses (x' : xs') = cons x' (createConses xs')
        pure (createConses (toList xs') ^. _Unwrapped % _1 % unlocated)
    renameExpr' (LetIn vn _ e body) = do
        vn' <- uniquify vn
        locally (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ Local vn')) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure $ LetIn vn' NoFieldValue exp' body'
    renameExpr' (Match e cases) = do
        e' <- renameExpr e
        cases' <- traverse (bitraverse renamePattern renameExpr) cases
        pure $ Match e' cases'
    renameExpr' (Tuple (x :| [x2])) = do
        -- turn it into Elara.Prim.Tuple2

        let tupleCtorName = TypeName <$> tuple2CtorName
        x' <- renameExpr x
        x2' <- renameExpr x2
        let base = Expr (FunctionCall (Expr (Located loc (Constructor (Located loc tupleCtorName)), Nothing)) x' `withLocationOf` x, Nothing)
        pure $ FunctionCall base x2'
    renameExpr' (Tuple bigger) = error "renameExpr': Tuple more than length 2"
    renameExpr' (InParens e) = InParens <$> renameExpr e
    renameExpr' (Let{}) = error "renameExpr': Let should be handled by renameExpr"
    renameExpr' (Block{}) = error "renameExpr': Block should be handled by renameExpr"

renameBinaryOperator :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredBinaryOperator -> Eff r RenamedBinaryOperator
renameBinaryOperator (MkBinaryOperator op) = MkBinaryOperator <$> traverseOf unlocated renameBinaryOperator' op
  where
    renameBinaryOperator' :: InnerRename r => DesugaredBinaryOperator' -> Eff r RenamedBinaryOperator'
    renameBinaryOperator' (SymOp o) = do
        op' <- lookupVarName (OperatorVarName <<$>> o)
        let onlyOpName (OperatorVarName o') = o'
            onlyOpName _ = error "renameBinaryOperator': I really don't like this"
        let op'' = onlyOpName <<$>> op'
        pure $ SymOp op''
    renameBinaryOperator' (Infixed (Located l o)) = do
        op' :: VarRef VarOrConName <- case o of -- TODO: tidy this up
            MaybeQualified (VarName n) q -> do
                vn <- lookupVarName (Located l (MaybeQualified (NormalVarName n) q))
                let onlyVarName (NormalVarName n') = n'
                    onlyVarName _ = error "renameBinaryOperator': I really don't like this"
                pure $ (VarName . onlyVarName <<$>> vn) ^. unlocated
            MaybeQualified (ConName n) q -> do
                tn <- lookupTypeName (Located l (MaybeQualified n q))
                pure $ Global (ConName <<$>> tn)
        pure $ Infixed op'

renamePattern :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredPattern -> Eff r RenamedPattern
renamePattern (Pattern fp@(Located loc _, _)) =
    Pattern
        <$> bitraverse
            (traverseOf unlocated renamePattern')
            (traverse (traverseOf (_Unwrapped % _1 % unlocated) (renameType False)))
            fp
  where
    renamePattern' :: InnerRename r => DesugaredPattern' -> Eff r RenamedPattern'
    renamePattern' (IntegerPattern i) = pure $ IntegerPattern i
    renamePattern' (FloatPattern i) = pure $ FloatPattern i
    renamePattern' (StringPattern i) = pure $ StringPattern i
    renamePattern' (CharPattern i) = pure $ CharPattern i
    renamePattern' WildcardPattern = pure WildcardPattern
    renamePattern' UnitPattern = pure UnitPattern
    renamePattern' (ListPattern []) = pure $ ConstructorPattern (Located loc emptyListCtorName) []
    renamePattern' (ListPattern (x : xs)) = do
        -- turn [x, y, z] into Cons x (Cons y (Cons z Nil))
        xs' <- traverse renamePattern (x :| xs)
        let lastCons :: Pattern Renamed =
                Pattern
                    ( ConstructorPattern
                        (Located loc emptyListCtorName)
                        []
                        `withLocationOf` last xs'
                    , Nothing
                    )
        let cons x y = Pattern (ConstructorPattern (Located loc consCtorName) [x, y] `withLocationOf` x, Nothing)
        let createConses :: [Pattern 'Renamed] -> Pattern 'Renamed
            createConses [] = lastCons
            createConses (x' : xs') = cons x' (createConses xs')
        pure (createConses (init xs') ^. _Unwrapped % _1 % unlocated)
    renamePattern' (ConsPattern (p1, p2)) = do
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        pure $ ConstructorPattern (Located loc consCtorName) [p1', p2']
    renamePattern' (TuplePattern (p1 :| [p2])) = do
        -- turn (x, y) into Elara.Prim.Tuple2 x y
        let tupleCtorName = TypeName <$> tuple2CtorName
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        pure $ ConstructorPattern (Located loc tupleCtorName) [p1', p2']
    renamePattern' (TuplePattern _) = error "renamePattern': TuplePattern more than length 2"
    renamePattern' (VarPattern vn) = do
        vn' <- uniquify vn
        Eff.modify (the @"varNames" %~ Map.insert (vn ^. unlocated % to NormalVarName) (one $ Local (NormalVarName <<$>> vn')))
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
            VarPattern vn -> NormalVarName $ vn ^. unlocated
            IntegerPattern _ -> mn "int"
            FloatPattern _ -> mn "float"
            StringPattern _ -> mn "string"
            CharPattern _ -> mn "char"
            ConstructorPattern _ _ -> mn "constructor"
            ConsPattern _ -> mn "cons"
            UnitPattern -> "unit"
            TuplePattern _ -> mn "tuple"

patternToMatch :: (InnerRename r, Eff.Reader (Maybe DesugaredDeclaration) :> r, Rock.Rock Elara.Query.Query :> r) => DesugaredPattern -> DesugaredExpr -> Eff r (Located (Unique VarName), RenamedExpr)
-- Special case, no match needed
-- We can just turn \x -> x into \x -> x
patternToMatch (Pattern (Located _ (VarPattern vn), _)) body = do
    uniqueVn <- uniquify (NormalVarName <$> vn)
    body' <- locally (the @"varNames" %~ Map.insert (vn ^. unlocated % to NormalVarName) (one $ Local uniqueVn)) $ renameExpr body
    pure (uniqueVn, body')
patternToMatch pat body = do
    let vn = patternToVarName pat
    let patLocation = pat ^. _Unwrapped % _1 % sourceRegion
    let bodyLocation = body ^. _Unwrapped % _1 % sourceRegion
    uniqueVn <- uniquify (Located patLocation vn)
    let varRef = Local uniqueVn `withLocationOf` uniqueVn
    pat' <- renamePattern pat
    body' <- renameExpr body
    let match =
            Match
                (Expr (Var varRef `withLocationOf` uniqueVn, Nothing))
                [(pat', body')]

    pure (uniqueVn, Expr (Located (enclosingRegion' patLocation bodyLocation) match, Nothing))

{- | Rename a lambda expression
This is a little bit special because patterns have to be converted to match expressions

For example,
@\(a, b) -> a@  becomes @\ab_ -> match ab_ with (a, b) -> a@
-}
renameLambda :: (InnerRename r, Eff.Reader (Maybe DesugaredDeclaration) :> r, Rock.Rock Elara.Query.Query :> r) => DesugaredPattern -> DesugaredExpr -> Eff r RenamedExpr'
renameLambda p@((Pattern (_, argType))) e = do
    (arg, match) <- patternToMatch p e
    argType' <- traverse renameSimpleType argType
    let arg' = fmap (,argType') arg
    pure (Lambda (TypedLambdaParam <$> arg') match)

desugarBlock :: (InnerRename r, Eff.Reader (Maybe DesugaredDeclaration) :> r, Rock.Rock Elara.Query.Query :> r) => NonEmpty DesugaredExpr -> Eff r RenamedExpr
desugarBlock (e@(Expr' (Let{})) :| []) = do
    decl <- Eff.ask @(Maybe DesugaredDeclaration)
    throwError (BlockEndsWithLet e (fmap (view (_Unwrapped % unlocated % the @"body")) decl))
desugarBlock (e :| []) = renameExpr e
desugarBlock (exp@(Expr (Located l (Let n p val), a)) :| (xs1 : xs')) = do
    n' <- uniquify n
    -- TODO this is almost identical to the normal let case
    locally (the @"varNames" %~ Map.insert (n ^. unlocated) (one $ Local n')) $ do
        val' <- renameExpr val
        a' <- traverse (traverseOf (_Unwrapped % _1 % unlocated) (renameType False)) a
        block <- desugarBlock (xs1 :| xs')
        pure $ Expr (Located l (LetIn n' p val' block), a')
desugarBlock xs = do
    let loc = spanningRegion' (xs <&> (^. _Unwrapped % _1 % sourceRegion))
    xs' <- traverse renameExpr xs
    pure $ Expr (Located loc (Block xs'), Nothing)
