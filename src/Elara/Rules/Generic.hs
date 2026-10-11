{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | AST agnostic implementations of rules
module Elara.Rules.Generic (
    genericGetDeclarationByName,
    genericGetRequiredDeclarationByName,
    genericGetConstructorDeclaration,
    genericGetDeclarationAnnotations,
    genericGetDeclarationAnnotationsOfType,
)
where

import Effectful
import Effectful.Error.Static

import Elara.AST.Location (TaggedLocate, TypeNode, VarNode)
import Elara.AST.Module
import Elara.AST.Name
import Elara.AST.Phase (ElaraPhase (..))
import Elara.AST.Region (HasSourceRegion (sourceRegion), SourceRegion, unlocated, withLocationOf)
import Elara.AST.Types
import Elara.Error
import Elara.Error.Internal

-- | Get the name from a declaration
declarationName ::
    ( ElaraPhase p
    , TopValueBinder p loc ~ TaggedLocate VarNode loc (Qualified VarName)
    , TopTypeBinder p loc ~ TaggedLocate TypeNode loc (Qualified TypeName)
    ) =>
    Declaration loc p -> Name
declarationName (Declaration _ (Declaration' _ body)) =
    let DeclarationBody _ body' = body
     in case body' of
            ValueDeclaration n _ _ _ _ _ -> toName (n ^. unlocated)
            TypeDeclarationBody n _ _ _ _ _ -> toName (n ^. unlocated)
            DeclBodyExtension _ -> error "declarationName: unexpected extension"

-- | Check if a declaration contains a type declaration with the given constructor name
hasConstructor ::
    ( ElaraPhase ast
    , ConstructorBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    Qualified TypeName ->
    Declaration SourceRegion ast ->
    Bool
hasConstructor qn (Declaration _ (Declaration' _ (DeclarationBody _ body'))) =
    case body' of
        TypeDeclarationBody _ _ (ADT ctors) _ _ _ ->
            any (\(cnLoc, _) -> (cnLoc ^. unlocated) == qn) ctors
        _ -> False

-- | Generic implementation for fetching a declaration by name
genericGetDeclarationByName ::
    forall ast es.
    ( ElaraPhase ast
    , Error ElaraError :> es
    , TopValueBinder ast SourceRegion ~ TaggedLocate VarNode SourceRegion (Qualified VarName)
    , TopTypeBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    -- | injected fetcher
    (ModuleName -> Eff es (Module SourceRegion ast)) ->
    Qualified Name ->
    Eff es (Maybe (Declaration SourceRegion ast))
genericGetDeclarationByName fetchMod (Qualified name modName) = do
    Module _ m' <- fetchMod modName
    let matchingBodies = filter (\d -> declarationName d == name) m'.moduleDeclarations
    case matchingBodies of
        [] -> pure Nothing
        [decl] -> pure (Just decl)
        _ -> throwElaraError $ DuplicateDeclAfterDesugar modName name

-- | Generic implementation for fetching a constructor declaration
genericGetConstructorDeclaration ::
    forall ast es.
    ( ElaraPhase ast
    , Error ElaraError :> es
    , ConstructorBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    , ConstructorOccurrence ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    -- | injected fetcher
    (ModuleName -> Eff es (Module SourceRegion ast)) ->
    Qualified TypeName ->
    Eff es (Maybe (Declaration SourceRegion ast))
genericGetConstructorDeclaration fetchMod constructorName@(Qualified typeName modName) = do
    Module _ m' <- fetchMod modName
    let matchingBodies = filter (hasConstructor constructorName) m'.moduleDeclarations
    case matchingBodies of
        [decl] -> pure $ Just decl
        [] -> pure Nothing
        _ -> throwElaraError $ DuplicateDeclAfterDesugar modName (toName typeName)

-- | Generic implementation for fetching a required declaration
genericGetRequiredDeclarationByName ::
    forall ast es.
    (ElaraPhase ast, Error ElaraError :> es) =>
    -- | injected fetcher
    (Qualified Name -> Eff es (Maybe (Declaration SourceRegion ast))) ->
    Qualified Name ->
    Eff es (Declaration SourceRegion ast)
genericGetRequiredDeclarationByName fetchDecl name = do
    mDecl <- fetchDecl name
    case mDecl of
        Just decl -> pure decl
        Nothing -> throwElaraError $ RequiredDeclNotFound name

-- | Generic implementation for fetching annotations on a declaration
genericGetDeclarationAnnotations ::
    forall ast es.
    ( ElaraPhase ast
    , DeclBodyExtension ast SourceRegion ~ Void
    ) =>
    -- | injected fetcher
    (Qualified Name -> Eff es (Declaration SourceRegion ast)) ->
    Qualified Name ->
    Eff es [Annotation SourceRegion ast]
genericGetDeclarationAnnotations fetchReqDecl qn = do
    Declaration _ (Declaration' _ (DeclarationBody _ body')) <- fetchReqDecl qn
    case body' of
        ValueDeclaration _ _ _ _ _ anns -> pure anns
        TypeDeclarationBody _ _ _ _ _ anns -> pure anns
        DeclBodyExtension v -> absurd v

-- | Generic implementation for fetching annotations of a specific type on a declaration
genericGetDeclarationAnnotationsOfType ::
    forall ast es.
    ( ElaraPhase ast
    , TypeOccurrence ast SourceRegion ~ ConstructorOccurrence ast SourceRegion
    , HasSourceRegion (ConstructorOccurrence ast SourceRegion)
    , TopTypeBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    , Error ElaraError :> es
    ) =>
    -- | injected fetchAnns
    (Qualified Name -> Eff es [Annotation SourceRegion ast]) ->
    -- | injected fetchCtor
    (Qualified TypeName -> Eff es (Maybe (Declaration SourceRegion ast))) ->
    (TypeOccurrence ast SourceRegion -> Qualified TypeName) ->
    (Qualified Name, Qualified TypeName) ->
    Eff es [Annotation SourceRegion ast]
genericGetDeclarationAnnotationsOfType fetchAnns fetchCtor extractTN (Qualified name modName, annName) = do
    annotations <- fetchAnns (Qualified name modName)
    fmap catMaybes $ for annotations $ \(Annotation annotName _args) -> do
        annotDecl <- requireConstructor (annotName ^. sourceRegion) fetchCtor (extractTN annotName)

        let Declaration _ (Declaration' _ (DeclarationBody _ body')) = annotDecl
        case body' of
            TypeDeclarationBody tName _ _ _ _ _ | tName ^. unlocated == annName -> pure (Just (Annotation annotName _args))
            _ -> pure Nothing

requireConstructor ::
    Error ElaraError :> es =>
    SourceRegion -> (Qualified TypeName -> Eff es (Maybe (Declaration SourceRegion ast))) -> Qualified TypeName -> Eff es (Declaration SourceRegion ast)
requireConstructor loc lookup name = do
    maybe <- lookup name
    case maybe of
        Just con -> pure con
        Nothing -> throwElaraError $ RequiredConstructorNotFound (name `withLocationOf` loc)
