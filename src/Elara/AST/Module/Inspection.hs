{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Module.Inspection where

import Control.Lens ((^.))
import Data.Map qualified as M
import Elara.AST.Module (Declaration (Declaration), Exposing (ExposingAll, ExposingSome), Exposition (ExposedType, ExposedTypeAndAllConstructors, ExposedValue), HasAs (..), HasDeclarations (declarations), HasImports (imports), HasName (..), Import (Import), Module (..), importing, qualified)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), OpName, TypeName, VarName)
import Elara.AST.Select (ASTQual)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Reader
import Polysemy.State
import Prelude hiding (Reader, State, ask, modify, runState)

data InspectionError ast
    = -- | The name is not defined in any known module
      UnknownName (Name (ASTQual ast))
    | -- | Importing an unknown module
      UnknownImportModule (Import (ASTQual ast))
    | -- | Importing an unknown element from a known module
      UnknownImportElement (Import (ASTQual ast)) (Name (ASTQual ast))
    | -- | The name is defined in multiple modules
      AmbiguousName (Name (ASTQual ast)) (NonEmpty ModuleName)

deriving instance (Show (Name (ASTQual ast)), Show (Import (ASTQual ast))) => Show (InspectionError ast)
deriving instance (Eq (Name (ASTQual ast)), Eq (Import (ASTQual ast))) => Eq (InspectionError ast)

exposes ::
    forall ast.
    (Eq (ASTQual ast VarName), Eq (ASTQual ast TypeName), Eq (ASTQual ast OpName)) => -- don't really like this
    Name (ASTQual ast) ->
    Module ast ->
    Bool
exposes elementName (Module _ exposing _ decls) = case exposing of
    ExposingAll -> isJust (find declares decls)
    ExposingSome expos -> isJust (find (isExposing elementName) expos) -- just check that it's exposed, not that it's declared. The latter will be checked later on
  where
    declares :: Declaration ast -> Bool
    declares (Declaration _ name' _) = elementName == name'

    isExposing :: Name (ASTQual ast) -> Exposition (ASTQual ast) -> Bool
    isExposing (NVarName q) (ExposedValue name') = q == name'
    isExposing (NTypeName q) (ExposedType name') = q == name'
    isExposing (NTypeName q) (ExposedTypeAndAllConstructors name') = q == name'
    isExposing _ _ = False

importAliasFor :: Import qual -> ModuleName
importAliasFor import' = fromMaybe (import' ^. importing) (import' ^. as)

{- | Check if a module is importing another module, returning the import if so, otherwise Nothing.
  This function considers a module to be importing itself, for use in [buildContext]
-}
importFor :: forall ast. Module ast -> Module ast -> Maybe (Import (ASTQual ast))
importFor x y | x ^. name == y ^. name = Just (Import (x ^. name) Nothing False ExposingAll)
importFor this imported =
    find (isImporting' (imported ^. name)) (this ^. imports)
  where
    isImporting' :: ModuleName -> Import (ASTQual ast) -> Bool
    isImporting' name' (Import name'' _ _ _) = name' == name''

type InspectionState ast = M.Map ModuleName (Module ast)

type InspectionContext = M.Map (Name MaybeQualified) (NonEmpty ModuleName)

search ::
    ( ASTQual ast ~ MaybeQualified
    , Members [Reader InspectionContext, Error (InspectionError ast)] r
    ) =>
    -- | The name to search for
    Name MaybeQualified ->
    Sem r ModuleName
search elementName = do
    context <- ask
    case M.lookup elementName context of
        Nothing -> throw (UnknownName elementName)
        Just (x :| []) -> pure x
        Just possibles -> throw (AmbiguousName elementName possibles)

{- | There are a lot of cases to consider when looking for a module:

        1. The name is defined in this module, i.e @let bar = ... ; bar@. It may or may not be qualified
        2. The name is unqualified in an imported module, i.e @import Foo (bar) ; bar@
        3. The name is qualified in an imported module, i.e @import Foo (bar) ; Foo.bar@
        4. The name is qualified with an alias in an imported module, i.e @import Foo as F (bar) ; F.bar@

        Step 1 is more or less the same as step 2, if we treat every module as if it imports itself.

        Similarly, step 3 is the same as step 4, just with a different name for the module.

        This can be simplified to a single step where we build a "context" of all the imported modules, each with an optional qualified name, then
        we look for the name in that context.

        For example, if we have the following imports:

        >    module Quux exposing (quux)
        >    ...
        >    module This exposing (bar)
        >    import Bar (bar)
        >    import Foo (fooBar)
        >    import Baz as B (baz, Baz)
        >    import Quux qualified

        the context would be

        >    { (bar, [This, Bar])
        >    , (This.bar, [This])
        >    , (fooBar, [Foo])
        >    , Foo.fooBar -> [Foo]
        >    , baz -> [Baz]
        >    , B.baz -> [Baz]
        >    , Baz -> [Baz]
        >    , quux -> [Quux]
        >    , Quux.quux -> [Quux]
        >    }
-}
buildContext ::
    forall ast.
    (ASTQual ast ~ MaybeQualified) =>
    -- | The module to build the context in relation to
    Module ast ->
    -- | All the known modules. The @thisModule@ does not need to be included, but can be (it will be ignored)
    InspectionState ast ->
    InspectionContext
buildContext thisModule s =
    run $ buildContext' (M.insert (thisModule ^. name) thisModule s)
  where
    buildContext' :: InspectionState ast -> Sem r InspectionContext
    buildContext' inspectionState = do
        x <- runState M.empty $ do
            traverse_ addDecls (M.elems inspectionState)
        pure (fst x)

    addDecls :: Member (State InspectionContext) r0 => Module ast -> Sem r0 ()
    addDecls m =
        let mImport = thisModule `importFor` m
         in whenJust mImport $ \import' -> do
                traverse_ (add (importAliasFor import') (import' ^. qualified)) (m ^. declarations)

    add :: forall r0. Member (State InspectionContext) r0 => ModuleName -> Bool -> Declaration ast -> Sem r0 ()
    add moduleAlias onlyQualified (Declaration actualModule declarationName _) = do
        -- insert the qualified AND unqualified name into the context
        let qualify :: MaybeQualified a -> MaybeQualified a
            qualify (MaybeQualified x _) = MaybeQualified x (Just moduleAlias)
            unqualify :: MaybeQualified a -> MaybeQualified a
            unqualify (MaybeQualified x _) = MaybeQualified x Nothing

            addQualAndUnqual :: (MaybeQualified a -> Name MaybeQualified) -> MaybeQualified a -> Sem r0 ()
            addQualAndUnqual ctor qual = do
                modify (M.insertWith (<>) (ctor (qualify qual)) (pure actualModule))
                unless onlyQualified (modify (M.insertWith (<>) (ctor (unqualify qual)) (pure actualModule)))

        case declarationName of
            NVarName qual -> addQualAndUnqual NVarName qual
            NTypeName qual -> addQualAndUnqual NTypeName qual
            NOpName op -> addQualAndUnqual NOpName op