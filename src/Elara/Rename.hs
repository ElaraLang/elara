module Elara.Rename where

import Control.Lens
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module (Exposing (..), HasDeclarations (declarations), HasExposing (..), HasImports (imports), Import, Module, Module' (..), _Module, Exposition (..))
import Elara.AST.Name hiding (HasName (..))
import Elara.AST.Region (unlocated, Located)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select (Frontend, HasName (..), Renamed)
import Elara.Data.Unique (Unique, UniqueGen, makeUnique)
import Polysemy (Member, Sem)

renameName :: Member UniqueGen r => name -> Sem r (Unique name)
renameName = makeUnique

renameName' :: Member UniqueGen r => Located (MaybeQualified n) -> Sem r (Located (Unique name))
renameName' = traverseOf unlocated renameName

rename :: Member UniqueGen r => Module Frontend -> Sem r (Module Renamed)
rename =
    traverseOf
        (_Module @Frontend @Renamed . unlocated)
        ( \y -> do
            renamedExposing <- renameExposing (y ^. exposing)
            renamedImports <- traverse renameImport (y ^. imports)
            renamedDecls <- traverse renameDeclaration (y ^. declarations)
            pure
                ( Module'
                    { _module'Name = y ^. name
                    , _module'Exposing = renamedExposing
                    , _module'Imports = renamedImports
                    , _module'Declarations = renamedDecls
                    }
                )
        )

renameExposing :: Member UniqueGen r => Exposing Frontend -> Sem r (Exposing Renamed)
renameExposing ExposingAll = pure ExposingAll
renameExposing (ExposingSome xs) = ExposingSome <$> traverse renameExposition xs

renameExposition :: Member UniqueGen r => Exposition Frontend -> Sem r (Exposition Renamed)
renameExposition (ExposedValue x) = let y = renameName <<$>> x in undefined

renameImport :: Member UniqueGen r => Import Frontend -> Sem r (Import Renamed)
renameImport = undefined

renameDeclaration :: Member UniqueGen r => Frontend.Declaration -> Sem r Renamed.Declaration
renameDeclaration = undefined