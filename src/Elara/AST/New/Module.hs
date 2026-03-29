{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Module where

import Elara.AST.Name (ModuleName)
import Elara.AST.New.Phase (ElaraPhase (..), Locate)
import Elara.AST.New.Types (Declaration)

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
    , importExposing :: Exposing loc p
    }
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
