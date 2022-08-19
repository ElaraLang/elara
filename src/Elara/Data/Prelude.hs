module Elara.Data.Prelude (prelude) where

import Data.Map qualified as M
import Data.Text qualified as T
import Elara.Data.Module
  ( Declaration (Declaration),
    DeclarationBody (ValueTypeDef),
    Exposing (ExposingAll),
    Module (..),
  )
import Elara.Data.Name (ModuleName (ModuleName), Name (Name))
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.Type (AbsType (Function, Int, String, Unit), Concrete, ConcreteType, ConcreteType' (Concrete), makeConcrete)
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))
import Prelude hiding (String)

preludeName :: ModuleName
preludeName = ModuleName ("Prelude" :| [])

prelude :: Module expr pattern' TypeAnnotation MaybeQualified
prelude =
  Module
    { _moduleName = preludeName,
      _moduleImports = [],
      _moduleExposing = ExposingAll,
      _moduleDeclarations =
        M.fromList
          [ dummyElement "*" Int,
            dummyElement "+" Int,
            dummyElement "==" Int,
            dummyElement "-" Int,
            dummyElement "println" (Function (makeConcrete String) (makeConcrete Unit))
          ]
    }

dummyElement :: Text -> AbsType Concrete MaybeQualified -> (Name, Declaration expr pattern' TypeAnnotation qualified)
dummyElement nameText type' =
  let name = Name nameText
   in (name, Declaration preludeName name (ValueTypeDef (TypeAnnotation name (makeConcrete type'))))