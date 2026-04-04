{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Module where

import Elara.AST.Name (ModuleName)
import Elara.AST.Phase (ElaraPhase (..), Locate)
import Elara.AST.Types (Declaration)

-- | Module with location and phase parameters
data Module loc p = Module !loc (Module' loc p)
    deriving (Generic)

data Module' loc p = Module'
    { moduleName :: Locate loc ModuleName
    , moduleExposing :: Exposing loc p
    , moduleImports :: [Import loc p]
    , moduleDeclarations :: [Declaration loc p]
    }
    deriving (Generic)

data Import loc p = Import !loc (Import' loc p)
    deriving (Generic)

data Import' loc p = Import'
    { importModuleName :: Locate loc ModuleName
    , importAs :: Maybe (Locate loc ModuleName)
    , importQualified :: Bool
    , importExposingOrHiding :: ImportExposingOrHiding loc p
    {- ^ Whether this import has an exposing or hiding clause, and if so, what it exposes or hides.
    This field is not a 'Maybe' because the lack of an @exposing@ or @hiding@ clause is equivalent to 'ExposingAll'
    -}
    }
    deriving (Generic)

data ImportExposingOrHiding loc p
    = -- | The import exposes only the listed items
      ImportExposing (Exposing loc p)
    | -- | The import hides the listed items, exposing everything else
      ImportHiding [Exposition loc p]
    deriving (Generic)

-- | Exposing list for modules and imports
data Exposing loc p
    = -- | Expose everything
      ExposingAll
    | -- | Expose only the listed items
      ExposingSome [Exposition loc p]
    deriving (Generic)

-- | A single item in an exposing list
data Exposition loc p
    = ExposedValue (ValueOccurrence p loc)
    | ExposedOp (OperatorOccurrence p loc)
    | ExposedType (TypeOccurrence p loc)
    | ExposedTypeAndAllConstructors (TypeOccurrence p loc)
    deriving (Generic)
