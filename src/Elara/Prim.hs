{-# LANGUAGE OverloadedLists #-}

-- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
-- The compiler will then replace these with the actual primitive functions.
module Elara.Prim where

import Elara.AST.Name (ModuleName (..), Name (..), Qualified (..), TypeName (..), VarName (NormalVarName))
import Elara.AST.Region (IgnoreLocation (IgnoreLocation), Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef, VarRef' (Global))
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Unique (UniqueGen)
import Elara.TypeInfer.Context (Context, Entry (Annotation))
import Elara.TypeInfer.Domain (Domain (..))
import Elara.TypeInfer.Monotype (Scalar (..))
import Elara.TypeInfer.Type (Type (..))
import Elara.TypeInfer.Unique (makeUniqueTyVarWith)
import Polysemy

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

fetchPrimitive :: (VarRef VarName)
fetchPrimitive = Global (mkPrimVarRef fetchPrimitiveName)

stringName :: TypeName
stringName = TypeName "String"

intName :: TypeName
intName = TypeName "Int"

charName :: TypeName
charName = TypeName "Char"

ioName :: TypeName
ioName = TypeName "IO"

primModuleName :: ModuleName
primModuleName = ModuleName ["Elara", "Prim"]

primRegion :: SourceRegion
primRegion = generatedSourceRegion (Just "<primitive>")

mkPrimQual :: c -> Qualified c
mkPrimQual c = Qualified c primModuleName

mkPrimVarRef :: c -> Located (Qualified c)
mkPrimVarRef c = generatedLocated (Just "<primitive>") (mkPrimQual c)

primitiveVars :: [VarName]
primitiveVars = [fetchPrimitiveName]

primitiveTypes :: [TypeName]
primitiveTypes = [stringName, charName, intName]

primKindCheckContext :: Map (Qualified TypeName) ElaraKind
primKindCheckContext =
  -- assume all primitive types are kind Type
  fromList ((\x -> (Qualified x primModuleName, TypeKind)) <$> primitiveTypes)
    <> fromList [(Qualified ioName primModuleName, FunctionKind TypeKind TypeKind)] -- Except for IO which is kind Type -> Type

primitiveTCContext :: (Member UniqueGen r) => Sem r (Context SourceRegion)
primitiveTCContext = do
  let easies =
        [ Annotation
            (Global (IgnoreLocation $ mkPrimVarRef (NTypeName stringName)))
            (Scalar primRegion Text),
          Annotation
            (Global (IgnoreLocation $ mkPrimVarRef (NTypeName intName)))
            (Scalar primRegion Integer),
          Annotation
            (Global (IgnoreLocation $ mkPrimVarRef (NTypeName charName)))
            (Scalar primRegion Char),
          Annotation
            (Global (IgnoreLocation $ mkPrimVarRef (NTypeName ioName)))
            (Custom primRegion "IO" [])
        ]

  primTyVarName <- makeUniqueTyVarWith "a"
  let elaraPrimitive =
        Annotation -- elaraPrimitive :: forall a. String -> a
          (Global (IgnoreLocation $ mkPrimVarRef (NVarName fetchPrimitiveName)))
          (Forall primRegion primRegion primTyVarName Type (Function primRegion (Scalar primRegion Text) (VariableType primRegion primTyVarName)))

  pure (elaraPrimitive : easies)
