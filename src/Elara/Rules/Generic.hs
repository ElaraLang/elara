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
import Effectful.Exception

import Elara.AST.Location (TaggedLocate, TypeNode, VarNode)
import Elara.AST.Name
import Elara.AST.Phase (ElaraPhase (..))
import Elara.AST.Region (SourceRegion, unlocated)
import Elara.Data.Pretty
import Elara.Error.Internal

import Elara.AST.Module qualified as NewModule
import Elara.AST.Types qualified as New

-- | Get the name from a declaration
declarationName ::
    ( ElaraPhase p
    , TopValueBinder p loc ~ TaggedLocate VarNode loc (Qualified VarName)
    , TopTypeBinder p loc ~ TaggedLocate TypeNode loc (Qualified TypeName)
    ) =>
    New.Declaration loc p -> Name
declarationName (New.Declaration _ (New.Declaration' _ body)) =
    let New.DeclarationBody _ body' = body
     in case body' of
            New.ValueDeclaration n _ _ _ _ _ -> toName (n ^. unlocated)
            New.TypeDeclarationBody n _ _ _ _ _ -> toName (n ^. unlocated)
            New.DeclBodyExtension _ -> error "declarationName: unexpected extension"

-- | Check if a declaration contains a type declaration with the given constructor name
hasConstructor ::
    ( ElaraPhase ast
    , ConstructorBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    TaggedLocate TypeNode SourceRegion (Qualified TypeName) ->
    New.Declaration SourceRegion ast ->
    Bool
hasConstructor qnLoc (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
    let qn = qnLoc ^. unlocated
     in case body' of
            New.TypeDeclarationBody _ _ (New.ADT ctors) _ _ _ ->
                any (\(cnLoc, _) -> (cnLoc ^. unlocated) == qn) ctors
            _ -> False

-- | Generic implementation for fetching a declaration by name
genericGetDeclarationByName ::
    forall ast es.
    ( ElaraPhase ast
    , TopValueBinder ast SourceRegion ~ TaggedLocate VarNode SourceRegion (Qualified VarName)
    , TopTypeBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    -- | injected fetcher
    (ModuleName -> Eff es (NewModule.Module SourceRegion ast)) ->
    Qualified Name ->
    Eff es (Maybe (New.Declaration SourceRegion ast))
genericGetDeclarationByName fetchMod (Qualified name modName) = do
    NewModule.Module _ m' <- fetchMod modName
    let matchingBodies = filter (\d -> declarationName d == name) m'.moduleDeclarations
    case matchingBodies of
        [] -> pure Nothing
        [decl] -> pure (Just decl)
        _ -> throwIO $ DuplicateDeclAfterDesugar modName name

-- | Generic implementation for fetching a constructor declaration
genericGetConstructorDeclaration ::
    forall ast es.
    ( ElaraPhase ast
    , ConstructorBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    , ConstructorOccurrence ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    -- | injected fetcher
    (ModuleName -> Eff es (NewModule.Module SourceRegion ast)) ->
    ConstructorOccurrence ast SourceRegion ->
    Eff es (New.Declaration SourceRegion ast)
genericGetConstructorDeclaration fetchMod locatedQn = do
    let Qualified typeName modName = locatedQn ^. unlocated
    NewModule.Module _ m' <- fetchMod modName
    let matchingBodies = filter (hasConstructor locatedQn) m'.moduleDeclarations
    case matchingBodies of
        [decl] -> pure decl
        [] -> throwIO $ RequiredDeclNotFound (toName <$> Qualified typeName modName)
        _ -> throwIO $ DuplicateDeclAfterDesugar modName (toName typeName)

-- | Generic implementation for fetching a required declaration
genericGetRequiredDeclarationByName ::
    forall ast es.
    ElaraPhase ast =>
    -- | injected fetcher
    (Qualified Name -> Eff es (Maybe (New.Declaration SourceRegion ast))) ->
    Qualified Name ->
    Eff es (New.Declaration SourceRegion ast)
genericGetRequiredDeclarationByName fetchDecl name = do
    mDecl <- fetchDecl name
    case mDecl of
        Just decl -> pure decl
        Nothing -> throwIO $ RequiredDeclNotFound name

-- | Generic implementation for fetching annotations on a declaration
genericGetDeclarationAnnotations ::
    forall ast es.
    ( ElaraPhase ast
    , DeclBodyExtension ast SourceRegion ~ Void
    ) =>
    -- | injected fetcher
    (Qualified Name -> Eff es (New.Declaration SourceRegion ast)) ->
    Qualified Name ->
    Eff es [New.Annotation SourceRegion ast]
genericGetDeclarationAnnotations fetchReqDecl qn = do
    New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body')) <- fetchReqDecl qn
    case body' of
        New.ValueDeclaration _ _ _ _ _ anns -> pure anns
        New.TypeDeclarationBody _ _ _ _ _ anns -> pure anns
        New.DeclBodyExtension v -> absurd v

-- | Generic implementation for fetching annotations of a specific type on a declaration
genericGetDeclarationAnnotationsOfType ::
    forall ast es.
    ( ElaraPhase ast
    , TypeOccurrence ast SourceRegion ~ ConstructorOccurrence ast SourceRegion
    , TopTypeBinder ast SourceRegion ~ TaggedLocate TypeNode SourceRegion (Qualified TypeName)
    ) =>
    -- | injected fetchAnns
    (Qualified Name -> Eff es [New.Annotation SourceRegion ast]) ->
    -- | injected fetchCtor
    (ConstructorOccurrence ast SourceRegion -> Eff es (New.Declaration SourceRegion ast)) ->
    (Qualified Name, Qualified TypeName) ->
    Eff es [New.Annotation SourceRegion ast]
genericGetDeclarationAnnotationsOfType fetchAnns fetchCtor (Qualified name modName, annName) = do
    annotations <- fetchAnns (Qualified name modName)
    fmap catMaybes $ for annotations $ \(New.Annotation annotName _args) -> do
        annotDecl <- fetchCtor annotName
        let New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body')) = annotDecl
        case body' of
            New.TypeDeclarationBody tName _ _ _ _ _ | tName ^. unlocated == annName -> pure (Just (New.Annotation annotName _args))
            _ -> pure Nothing
