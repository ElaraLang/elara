{-# LANGUAGE UndecidableInstances #-}

-- | AST agnostic implementations of rules
module Elara.Rules.Generic where

import Effectful
import Effectful.Exception
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name
import Elara.AST.Phase (ElaraPhase (..))
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Types qualified as New
import Elara.Data.Pretty
import Elara.Error.Internal
import Elara.Query
import Elara.Query.Effects
import Elara.Query.Errors
import Rock qualified

-- | Helper to get the name from a declaration (works for phases with Located Qualified binders)
declarationName ::
    ( ElaraPhase p
    , TopValueBinder p loc ~ Located (Qualified VarName)
    , TopTypeBinder p loc ~ Located (Qualified TypeName)
    ) =>
    New.Declaration loc p -> Name
declarationName (New.Declaration _ (New.Declaration' _ body)) =
    let New.DeclarationBody _ body' = body
     in case body' of
            New.ValueDeclaration n _ _ _ _ _ -> toName (n ^. unlocated)
            New.TypeDeclarationBody n _ _ _ _ _ -> toName (n ^. unlocated)
            New.DeclBodyExtension _ -> error "declarationName: unexpected extension"

instance
    ( SupportsQuery QueryModuleByName ast
    , HasMinimumQueryEffects (QueryEffectsOf QueryDeclarationByName ast)
    , Rock.Rock Query :> QueryEffectsOf QueryDeclarationByName ast
    , Typeable ast
    , ElaraPhase ast
    , TopValueBinder ast SourceRegion ~ Located (Qualified VarName)
    , TopTypeBinder ast SourceRegion ~ Located (Qualified TypeName)
    , Subset
        (QueryEffectsOf QueryModuleByName ast)
        (QueryEffectsOf QueryDeclarationByName ast)
    , QueryReturnTypeOf QueryModuleByName ast ~ NewModule.Module SourceRegion ast
    ) =>
    SupportsQuery QueryDeclarationByName ast
    where
    type QuerySpecificEffectsOf QueryDeclarationByName ast = StandardQueryError ast
    query (Qualified name modName) = do
        NewModule.Module _ m' <- Rock.fetch $ ModuleByName @ast modName
        let matchingBodies =
                filter (\d -> declarationName d == name) m'.moduleDeclarations

        case matchingBodies of
            [] -> pure Nothing
            [decl] -> pure (Just decl)
            _ -> throwIO $ DuplicateDeclAfterDesugar modName name

instance
    ( HasMinimumQueryEffects (QueryEffectsOf QueryConstructorDeclaration ast)
    , ConstructorOccurrence ast SourceRegion ~ Located (Qualified TypeName)
    , ConstructorBinder ast SourceRegion ~ Located (Qualified TypeName)
    , ElaraPhase ast
    , Subset
        (QueryEffectsOf QueryModuleByName ast)
        (QueryEffectsOf QueryConstructorDeclaration ast)
    , SupportsQuery QueryModuleByName ast
    , Typeable ast
    , Rock.Rock Query :> QueryEffectsOf QueryConstructorDeclaration ast
    , QueryReturnTypeOf QueryConstructorDeclaration ast ~ New.Declaration SourceRegion ast
    , QueryReturnTypeOf QueryModuleByName ast ~ NewModule.Module SourceRegion ast
    ) =>
    SupportsQuery QueryConstructorDeclaration ast
    where
    query qn@(Located _ (Qualified typeName modName)) = do
        NewModule.Module _ m' <- Rock.fetch $ ModuleByName @ast modName
        let matchingBodies =
                filter (hasConstructor qn) m'.moduleDeclarations

        case matchingBodies of
            [decl] -> pure decl
            [] -> throwIO $ RequiredDeclNotFound (toName <$> Qualified typeName modName)
            _ -> throwIO $ DuplicateDeclAfterDesugar modName (toName typeName)

-- | Check if a declaration contains a type declaration with the given constructor name
hasConstructor :: (ElaraPhase ast, ConstructorBinder ast SourceRegion ~ Located (Qualified TypeName)) => Located (Qualified TypeName) -> New.Declaration SourceRegion ast -> Bool
hasConstructor (Located _ qn) (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
    case body' of
        New.TypeDeclarationBody _ _ (New.ADT ctors) _ _ _ ->
            any (\(Located _ cn, _) -> cn == qn) ctors
        _ -> False

instance
    ( SupportsQuery QueryDeclarationByName ast
    , Subset
        (QueryEffectsOf QueryDeclarationByName ast)
        (QueryEffectsOf QueryRequiredDeclarationByName ast)
    , HasMinimumQueryEffects (QueryEffectsOf QueryRequiredDeclarationByName ast)
    , Rock.Rock Query :> QueryEffectsOf QueryRequiredDeclarationByName ast
    , Typeable ast
    ) =>
    SupportsQuery QueryRequiredDeclarationByName ast
    where
    type QuerySpecificEffectsOf QueryRequiredDeclarationByName ast = QuerySpecificEffectsOf QueryDeclarationByName ast
    query name = do
        mDecl <- Rock.fetch $ DeclarationByName @ast name
        case mDecl of
            Just decl -> pure decl
            Nothing -> throwIO $ RequiredDeclNotFound name

instance
    ( SupportsQuery QueryRequiredDeclarationByName ast
    , Typeable ast
    , DeclBodyExtension ast SourceRegion ~ Void
    ) =>
    SupportsQuery DeclarationAnnotations ast
    where
    query qn = do
        New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body')) <- Rock.fetch $ RequiredDeclarationByName @ast qn
        case body' of
            New.ValueDeclaration _ _ _ _ _ anns -> pure anns
            New.TypeDeclarationBody _ _ _ _ _ anns -> pure anns
            New.DeclBodyExtension v -> absurd v

instance
    ( Typeable ast
    , SupportsQuery DeclarationAnnotations ast
    , SupportsQuery QueryConstructorDeclaration ast
    , ElaraPhase ast
    , ConstructorOccurrence ast SourceRegion ~ Located (Qualified TypeName)
    , TypeOccurrence ast SourceRegion ~ Located (Qualified TypeName)
    , TopTypeBinder ast SourceRegion ~ Located (Qualified TypeName)
    ) =>
    SupportsQuery DeclarationAnnotationsOfType ast
    where
    type QuerySpecificEffectsOf DeclarationAnnotationsOfType ast = StandardQueryError ast
    query (Qualified name modName, annName) = do
        annotations <- Rock.fetch $ DeclarationAnnotations @ast (Qualified name modName)

        fmap catMaybes $ for annotations $ \(New.Annotation annotName _args) -> do
            annotDecl <- Rock.fetch (ConstructorDeclaration @ast annotName)
            let New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body')) = annotDecl
            case body' of
                New.TypeDeclarationBody tName _ _ _ _ _ | tName ^. unlocated == annName -> pure (Just (New.Annotation annotName _args))
                _ -> pure Nothing
