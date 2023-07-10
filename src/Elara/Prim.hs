{-# LANGUAGE OverloadedLists #-}

-- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
-- The compiler will then replace these with the actual primitive functions.
module Elara.Prim where

import Elara.AST.Name (ModuleName (..), Name (..), Qualified (..), TypeName (..), VarName (NormalVarName))
import Elara.AST.Region (IgnoreLocation (IgnoreLocation), Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef, VarRef' (Global))
import Elara.Data.Kind (ElaraKind (..))
import Elara.Rename (RenameState (RenameState))
import Elara.TypeInfer.Context (Context, Entry (Annotation))
import Elara.TypeInfer.Domain (Domain (..))
import Elara.TypeInfer.Monotype (Scalar (..))
import Elara.TypeInfer.Type (Type (..))

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

fetchPrimitive :: (VarRef VarName)
fetchPrimitive = (Global (mkPrimVarRef fetchPrimitiveName))

stringName :: TypeName
stringName = TypeName "String"

ioName :: TypeName
ioName = TypeName "IO"

primModuleName :: ModuleName
primModuleName = ModuleName ["Elara", "Prim"]

primRegion :: SourceRegion
primRegion = generatedSourceRegion (Just "<primitive>")

mkPrimVarRef :: c -> Located (Qualified c)
mkPrimVarRef c = generatedLocated (Just "<primitive>") (Qualified c primModuleName)

primitiveVars :: [VarName]
primitiveVars = [fetchPrimitiveName]

primitiveTypes :: [TypeName]
primitiveTypes = [stringName]

primitiveRenameState :: RenameState
primitiveRenameState =
  let vars =
        fromList ((\x -> (x, Global (mkPrimVarRef x))) <$> primitiveVars)
      types =
        fromList ((\x -> (x, Global (mkPrimVarRef x))) <$> primitiveTypes) <> fromList [(ioName, Global (mkPrimVarRef ioName))]
   in RenameState vars types mempty

primKindCheckContext :: Map (Qualified TypeName) ElaraKind
primKindCheckContext =
  -- assume all primitive types are kind Type
  fromList ((\x -> (Qualified x primModuleName, TypeKind)) <$> primitiveTypes)
    <> fromList [(Qualified ioName primModuleName, FunctionKind TypeKind TypeKind)] -- Except for IO which is kind Type -> Type

primitiveTCContext :: Context SourceRegion
primitiveTCContext =
  [ Annotation
      (Global (IgnoreLocation $ mkPrimVarRef (NVarName fetchPrimitiveName)))
      (Forall primRegion primRegion "a" Type (Function primRegion (Scalar primRegion String) (VariableType primRegion "a"))), -- elaraPrimitive :: forall a. String -> a
    Annotation
      (Global (IgnoreLocation $ mkPrimVarRef (NTypeName stringName)))
      (Scalar primRegion String),
    Annotation
      (Global (IgnoreLocation $ mkPrimVarRef (NTypeName ioName)))
      (Custom primRegion "IO" [VariableType primRegion "a"])
  ]
