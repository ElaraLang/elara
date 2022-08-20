{-# LANGUAGE DataKinds #-}

module Elara.Data.Prelude (prelude) where

import Data.Map qualified as M
import Data.Multimap qualified as Mu
import Data.Text qualified as T
import Elara.Data.Module
  ( Declaration (Declaration),
    DeclarationBody (ValueTypeDef),
    Exposing (ExposingAll),
    Module (..),
  )
import Elara.Data.Name (ModuleName (ModuleName), Name (Name))
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.Type (AbsType (..), Concrete, ConcreteType, makeConcrete)
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))
import Elara.Data.Uniqueness
import Prelude hiding (String)

preludeName :: ModuleName
preludeName = ModuleName ("Prelude" :| [])

intType = UserDefinedType Nothing (Name "Int")

stringType = UserDefinedType Nothing (Name "String")

prelude :: Module expr pattern' TypeAnnotation MaybeQualified Many
prelude =
  Module
    { _moduleName = preludeName,
      _moduleImports = [],
      _moduleExposing = ExposingAll,
      _moduleDeclarations =
        Mu.fromList
          [ dummyElement "*" intType,
            dummyElement "+" intType,
            dummyElement "==" intType,
            dummyElement "-" intType,
            dummyElement "println" (Function (makeConcrete stringType) (makeConcrete Unit)),
            dummyElement "map" (Function (makeConcrete stringType) (makeConcrete stringType)), -- TODO make this an actual signature

            -- the types
            dummyElement "IO" (UserDefinedType Nothing (Name "IO")),
            dummyElement "()" (UserDefinedType Nothing (Name "()")),
            dummyElement "Int" intType,
            dummyElement "String" stringType
          ]
    }

dummyElement :: Text -> AbsType Concrete MaybeQualified -> (Name, Declaration expr pattern' TypeAnnotation qualified)
dummyElement nameText type' =
  let name = Name nameText
   in (name, Declaration preludeName name (ValueTypeDef (TypeAnnotation name (makeConcrete type'))))