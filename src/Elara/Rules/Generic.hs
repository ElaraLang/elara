{-# LANGUAGE UndecidableInstances #-}

-- | AST agnostic implementations of rules
module Elara.Rules.Generic where

import Data.Generics.Product (HasField' (..))
import Data.Generics.Wrapped (_Unwrapped)
import Effectful
import Effectful.Exception
import Elara.AST.Generic.Types
import Elara.AST.Introspection
import Elara.AST.Module
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Data.Pretty
import Elara.Error.Internal
import Elara.Query
import Elara.Query.Effects
import Elara.Query.Errors
import Optics (filtered, has)
import Rock qualified

instance
    ( SupportsQuery QueryModuleByName ast
    , HasMinimumQueryEffects (QueryEffectsOf QueryDeclarationByName ast)
    , Rock.Rock Query :> QueryEffectsOf QueryDeclarationByName ast
    , Typeable ast
    , RUnlocate ast
    , Subset
        (QueryEffectsOf QueryModuleByName ast)
        (QueryEffectsOf QueryDeclarationByName ast)
    , QueryReturnTypeOf QueryModuleByName ast ~ Module ast
    , ToName (Select (ASTName ForValueDecl) ast)
    , ToName (Select (ASTName ForType) ast)
    , -- TODO these are stupid and we should find a way to remove them
      CleanupLocated (Located (Select (ASTName ForType) ast)) ~ Located (Select (ASTName ForType) ast)
    , CleanupLocated (Located (Select (ASTName ForValueDecl) ast)) ~ Located (Select (ASTName ForValueDecl) ast)
    ) =>
    SupportsQuery QueryDeclarationByName ast
    where
    type QuerySpecificEffectsOf QueryDeclarationByName ast = StandardQueryError ast
    query (Qualified name modName) = do
        mod :: Module ast <- Rock.fetch $ ModuleByName @ast modName

        let matchingBodies =
                mod
                    ^.. _Unwrapped @(Module ast) @(Module ast) @(ASTLocate ast (Module' ast))
                    % rUnlocated @_ @ast @(Module' ast)
                    % field' @"declarations"
                    % each
                    % filtered (\b -> b ^. (declarationName @ast) % (rUnlocated @_ @ast @Name) == name)

        case matchingBodies of
            [] -> pure Nothing
            [decl] -> pure (Just decl)
            _ -> throwIO $ DuplicateDeclAfterDesugar modName name

instance
    ( HasMinimumQueryEffects (QueryEffectsOf QueryConstructorDeclaration ast)
    , Select ConRef ast ~ Qualified TypeName
    , Subset
        (QueryEffectsOf QueryModuleByName ast)
        (QueryEffectsOf QueryConstructorDeclaration ast)
    , RUnlocate ast
    , SupportsQuery QueryModuleByName ast
    , Typeable ast
    , Rock.Rock Query :> QueryEffectsOf QueryConstructorDeclaration ast
    , Pretty (Select ADTParam ast)
    , Pretty (ASTLocate ast (Select (ASTName ForType) ast))
    , Pretty (ASTLocate ast (Select ASTTypeVar ast))
    , Pretty (ASTLocate ast (TypeDeclaration ast))
    , Pretty (ASTLocate ast (Declaration' ast))
    , CleanupLocated (Located (Select ConstructorName ast)) ~ Located (Select ConstructorName ast)
    , Select ConstructorName ast ~ Qualified TypeName
    , QueryReturnTypeOf QueryConstructorDeclaration ast ~ Declaration ast
    ) =>
    SupportsQuery QueryConstructorDeclaration ast
    where
    query qn@(Qualified typeName modName) = do
        mod <- Rock.fetch $ ModuleByName @ast modName
        let matchingBodies =
                mod
                    ^.. _Unwrapped @(Module ast) @(Module ast) @(ASTLocate ast (Module' ast))
                    % rUnlocated @_ @ast @(Module' ast)
                    % field' @"declarations"
                    % each
                    % filtered
                        ( has
                            ( _Unwrapped
                                % rUnlocated @_ @ast @(Declaration' ast)
                                % field' @"body"
                                % _Unwrapped
                                % rUnlocated @_ @ast @(DeclarationBody' ast)
                                % _Ctor' @"TypeDeclaration"
                                % _3
                                % rUnlocated @_ @ast @(TypeDeclaration ast)
                                % _Ctor' @"ADT"
                                % each
                                % _1
                                % rUnlocated @_ @ast @(Select ConstructorName ast)
                                % filtered (== qn)
                            )
                        )
        case matchingBodies of
            [decl] -> pure decl
            [] -> throwIO $ RequiredDeclNotFound (toName <$> qn)
            _ -> throwIO $ DuplicateDeclAfterDesugar modName (toName typeName)

instance
    ( SupportsQuery QueryRequiredDeclarationByName ast
    , Typeable ast
    , RUnlocate ast
    , IntrospectableAnnotations ast
    ) =>
    SupportsQuery DeclarationAnnotations ast
    where
    query qn = do
        decl <- Rock.fetch $ RequiredDeclarationByName @ast qn
        let body =
                decl
                    ^. _Unwrapped
                    % rUnlocated @_ @ast @(Declaration' ast)
                    % field' @"body"
                    % _Unwrapped
                    % rUnlocated @_ @ast @(DeclarationBody' ast)
        case body of
            Value{_valueAnnotations = annotations} -> pure (getAnnotations @ast @ForValueDecl (annotations ^. _Unwrapped))
            ValueTypeDef _ _ annotations -> pure (getAnnotations @ast @ForValueDecl (annotations ^. _Unwrapped)) -- i am fairly sure this branch should never be hit
            TypeDeclaration{typeAnnotations = annotations} -> pure (getAnnotations @ast @ForTypeDecl (annotations ^. field' @"typeDeclAnnotations"))

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
    ( Typeable ast
    , RUnlocate ast
    , SupportsQuery DeclarationAnnotations ast
    , SupportsQuery QueryConstructorDeclaration ast
    , annName ~ Qualified TypeName
    , Select AnnotationName ast ~ annName
    , Select ConRef ast ~ annName
    , Ord annName
    , Hashable annName
    , CleanupLocated (Located annName) ~ Located annName
    , CleanupLocated
        (ASTLocate' ast (Select (ASTName ForType) ast))
        ~ CleanupLocated (ASTLocate' ast (Qualified TypeName))
    ) =>
    SupportsQuery DeclarationAnnotationsOfType ast
    where
    type QuerySpecificEffectsOf DeclarationAnnotationsOfType ast = StandardQueryError ast
    query (declName@(Qualified name modName), annName) = do
        annotations <- Rock.fetch $ DeclarationAnnotations @ast (Qualified name modName)

        fmap catMaybes $ for annotations $ \ann -> do
            let name = ann ^. field' @"annotationName" % rUnlocated @_ @ast @(Select AnnotationName ast)
            annotationDecl <- Rock.fetch (ConstructorDeclaration @ast name)

            let body =
                    annotationDecl
                        ^. _Unwrapped
                        % rUnlocated @_ @ast @(Declaration' ast)
                        % field' @"body"
                        % _Unwrapped
                        % rUnlocated @_ @ast @(DeclarationBody' ast)
            case body of
                TypeDeclaration{_typeDeclarationName = tName} | tName ^. rUnlocated @_ @ast @annName == annName -> pure (Just ann)
                _ -> pure Nothing
