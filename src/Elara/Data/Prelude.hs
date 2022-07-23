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
import Elara.Data.Type (AbsType (Int, String), ConcreteAbs, ConcreteType (..))
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))

preludeName :: ModuleName
preludeName = ModuleName ["Prelude"]

prelude :: Module expr pattern TypeAnnotation qualified
prelude =
  Module
    { _moduleName = preludeName,
      _moduleImports = [],
      _exposing = ExposingAll,
      _declarations =
        M.fromList
          [ dummyElement "*" Int,
            dummyElement "+" Int,
            dummyElement "==" Int,
            dummyElement "-" Int,
            dummyElement "println" String
          ]
    }

dummyElement :: T.Text -> ConcreteAbs MaybeQualified -> (Name, Declaration expr pattern TypeAnnotation qualified)
dummyElement nameText type' =
  let name = Name nameText
   in ( name,
        Declaration preludeName name (ValueTypeDef (TypeAnnotation name (ConcreteType type')))
      )