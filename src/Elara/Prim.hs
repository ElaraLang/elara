{-# LANGUAGE OverloadedLists #-}

{- | Stores information about the primitive types and functions of Elara.
These are still written in the source code, with a special name and value.
The compiler will then replace these with the actual primitive functions.

This module is the single source of truth for primitive type metadata.
Downstream modules ('Elara.TypeInfer.Render', 'Elara.ToCore', etc.)
should use the registry functions here instead of hardcoding primitive names.
-}
module Elara.Prim (
    -- * Primitive Type Registry
    PrimType (..),
    PrimTypeInfo (..),
    primTypeInfo,
    lookupPrimByInternalName,
    lookupPrimByUserName,
    isPrimTypeName,
    allPrimTypes,

    -- * Primitive Operations
    PrimOp (..),
    parsePrimOp,
    primOpArity,

    -- * Legacy Names (re-exported for compatibility)
    consName,
    cons,
    fetchPrimitiveName,
    fetchPrimitive,
    stringName,
    intName,
    floatName,
    unitName,
    boolName,
    trueName,
    falseName,
    charName,
    doubleName,
    ioName,
    fullListName,

    -- * Module & Region Utilities
    primModuleName,
    primRegion,
    primLocated,
    mkPrimQual,
    mkPrimVarRef,

    -- * Collections
    primitiveVars,
    primitiveTypes,

    -- * Fixity Annotations
    fixityAnnotationName,
    associativityAnnotationName,
    leftAssociativeAnnotationName,
    rightAssociativeAnnotationName,
    nonAssociativeAnnotationName,

    -- * Kind Context
    primKindCheckContext,
) where

import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Elara.AST.Name (MaybeQualified (..), ModuleName (..), Qualified (..), TypeName (..), VarName (..), VarOrConName (..))
import Elara.AST.Region (Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef, VarRef' (Global))
import Elara.Data.Kind (ElaraKind (..))

-- ────────────────────────────────────────────────────────────────────────────
-- Primitive Type Registry
-- ────────────────────────────────────────────────────────────────────────────

-- | All known primitive types. This is the single source of truth.
data PrimType
    = PrimInt
    | PrimFloat
    | PrimDouble
    | PrimString
    | PrimChar
    | PrimIO
    | PrimUnit
    | PrimBool
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Core metadata for each primitive type (backend-agnostic).
data PrimTypeInfo = PrimTypeInfo
    { primInternalName :: !TypeName
    -- ^ The internal compiler name, e.g. @TypeName "Prim_Int"@
    , primUserFacingName :: !Text
    -- ^ The user-visible name for error messages, e.g. @"Int"@
    , primQualified :: !(Qualified TypeName)
    -- ^ Fully qualified, e.g. @Qualified (TypeName "Prim_Int") primModuleName@
    }
    deriving (Show)

-- | The registry: metadata for each primitive type.
primTypeInfo :: PrimType -> PrimTypeInfo
primTypeInfo = \case
    PrimInt -> mkInfo "Prim_Int" "Int"
    PrimFloat -> mkInfo "Prim_Float" "Float"
    PrimDouble -> mkInfo "Prim_Double" "Double"
    PrimString -> mkInfo "Prim_String" "String"
    PrimChar -> mkInfo "Prim_Char" "Char"
    PrimIO -> mkInfo "Prim_IO" "IO"
    PrimUnit -> mkInfo "()" "()"
    PrimBool -> mkInfo "Bool" "Bool"
  where
    mkInfo :: Text -> Text -> PrimTypeInfo
    mkInfo internal user =
        PrimTypeInfo
            (TypeName internal)
            user
            (mkPrimQual (TypeName internal))

-- | All primitive types (useful for iteration).
allPrimTypes :: [PrimType]
allPrimTypes = universe

{- | Look up a primitive type by its internal 'TypeName' (e.g. @TypeName "Prim_Int"@).
Used by 'Elara.TypeInfer.Render' to map internal names to user-facing ones.
-}
lookupPrimByInternalName :: TypeName -> Maybe PrimType
lookupPrimByInternalName tn = Map.lookup tn internalNameMap
  where
    internalNameMap :: Map TypeName PrimType
    internalNameMap = Map.fromList [(primInternalName (primTypeInfo p), p) | p <- allPrimTypes]
    {-# NOINLINE internalNameMap #-}

-- | Look up a primitive type by its user-facing name (e.g. @"Int"@).
lookupPrimByUserName :: Text -> Maybe PrimType
lookupPrimByUserName name = Map.lookup name userNameMap
  where
    userNameMap :: Map Text PrimType
    userNameMap = Map.fromList [(primUserFacingName (primTypeInfo p), p) | p <- allPrimTypes]
    {-# NOINLINE userNameMap #-}

{- | Check if a qualified type name refers to a known primitive.
Used by 'Elara.ToCore' to short-circuit TyCon lookups.
-}
isPrimTypeName :: Qualified TypeName -> Maybe PrimType
isPrimTypeName (Qualified tn modName)
    | modName == primModuleName = lookupPrimByInternalName tn
    | otherwise = Nothing

-- ────────────────────────────────────────────────────────────────────────────
-- Primitive Operations
-- ────────────────────────────────────────────────────────────────────────────

{- | Primitive operations, resolved from @elaraPrimitive@ string keys during ToCore.
This is backend-agnostic; the JVM backend maps these to its own 'Elara.JVM.IR.PrimOp'.
-}
data PrimOp
    = PrimIntAdd
    | PrimIntSubtract
    | PrimIntMultiply
    | PrimIntNegate
    | PrimPrintln
    | PrimToString
    | PrimStringCons
    | PrimStringHead
    | PrimStringIsEmpty
    | PrimStringTail
    | PrimEquals
    | PrimCompare
    | PrimIOBind
    | PrimThrowError
    | PrimDebugWithMsg
    | PrimReadFile
    | PrimGetArgs
    deriving (Eq, Ord, Show, Enum, Bounded, Data, Generic)

{- | Parse an @elaraPrimitive@ string key into a typed 'PrimOp'.
This is the single source of truth for the string → PrimOp mapping.
-}
parsePrimOp :: Text -> Maybe PrimOp
parsePrimOp = \case
    "+" -> Just PrimIntAdd
    "-" -> Just PrimIntSubtract
    "*" -> Just PrimIntMultiply
    "negate" -> Just PrimIntNegate
    "println" -> Just PrimPrintln
    "toString" -> Just PrimToString
    "stringCons" -> Just PrimStringCons
    "stringHead" -> Just PrimStringHead
    "stringIsEmpty" -> Just PrimStringIsEmpty
    "stringTail" -> Just PrimStringTail
    "==" -> Just PrimEquals
    "compare" -> Just PrimCompare
    ">>=" -> Just PrimIOBind
    "error" -> Just PrimThrowError
    "debugWithMsg" -> Just PrimDebugWithMsg
    "readFile" -> Just PrimReadFile
    "getArgs" -> Just PrimGetArgs
    _ -> Nothing

{- | The arity of each primitive operation (number of value arguments).
Used by the interpreter for currying logic.
-}
primOpArity :: PrimOp -> Int
primOpArity = \case
    PrimIntAdd -> 2
    PrimIntSubtract -> 2
    PrimIntMultiply -> 2
    PrimIntNegate -> 1
    PrimPrintln -> 1
    PrimToString -> 1
    PrimStringCons -> 2
    PrimStringHead -> 1
    PrimStringIsEmpty -> 1
    PrimStringTail -> 1
    PrimEquals -> 2
    PrimCompare -> 2
    PrimIOBind -> 2
    PrimThrowError -> 1
    PrimDebugWithMsg -> 2
    PrimReadFile -> 1
    PrimGetArgs -> 0

-- ────────────────────────────────────────────────────────────────────────────
-- Legacy Individual Names (kept for backward compatibility)
-- These are now derived from the registry but exported with the same API.
-- ────────────────────────────────────────────────────────────────────────────

consName :: TypeName
consName = "::"

cons :: MaybeQualified VarOrConName
cons = MaybeQualified (ConName "Cons") (Just primModuleName)

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

fetchPrimitive :: (VarRef VarName)
fetchPrimitive = Global (mkPrimVarRef fetchPrimitiveName)

stringName :: TypeName
stringName = primInternalName (primTypeInfo PrimString)

intName :: TypeName
intName = primInternalName (primTypeInfo PrimInt)

floatName :: TypeName
floatName = primInternalName (primTypeInfo PrimFloat)

unitName :: TypeName
unitName = primInternalName (primTypeInfo PrimUnit)

boolName :: TypeName
boolName = primInternalName (primTypeInfo PrimBool)

trueName :: TypeName
trueName = TypeName "True"

falseName :: TypeName
falseName = TypeName "False"

charName :: TypeName
charName = primInternalName (primTypeInfo PrimChar)

doubleName :: TypeName
doubleName = primInternalName (primTypeInfo PrimDouble)

ioName :: TypeName
ioName = primInternalName (primTypeInfo PrimIO)

fullListName :: IsString a => Qualified a
fullListName = mkPrimQual "List"

-- ────────────────────────────────────────────────────────────────────────────
-- Module & Region Utilities
-- ────────────────────────────────────────────────────────────────────────────

primModuleName :: ModuleName
primModuleName = ModuleName ["Elara", "Prim"]

primRegion :: SourceRegion
primRegion = generatedSourceRegion (Just "<primitive>")

primLocated :: a -> Located a
primLocated = generatedLocated (Just "<primitive>")

mkPrimQual :: c -> Qualified c
mkPrimQual c = Qualified c primModuleName

mkPrimVarRef :: c -> Located (Qualified c)
mkPrimVarRef c = generatedLocated (Just "<primitive>") (mkPrimQual c)

-- ────────────────────────────────────────────────────────────────────────────
-- Collections & Fixity
-- ────────────────────────────────────────────────────────────────────────────

primitiveVars :: [VarName]
primitiveVars = [fetchPrimitiveName]

primitiveTypes :: [TypeName]
primitiveTypes = [stringName, charName, intName, floatName, unitName, consName]

fixityAnnotationName :: Qualified TypeName
fixityAnnotationName = mkPrimQual (TypeName "Fixity")

associativityAnnotationName :: Qualified TypeName
associativityAnnotationName = mkPrimQual (TypeName "Associativity")

leftAssociativeAnnotationName :: Qualified TypeName
leftAssociativeAnnotationName = mkPrimQual (TypeName "LeftAssociative")
rightAssociativeAnnotationName :: Qualified TypeName
rightAssociativeAnnotationName = mkPrimQual (TypeName "RightAssociative")
nonAssociativeAnnotationName :: Qualified TypeName
nonAssociativeAnnotationName = mkPrimQual (TypeName "NonAssociative")

-- ────────────────────────────────────────────────────────────────────────────
-- Kind Context
-- ────────────────────────────────────────────────────────────────────────────

primKindCheckContext :: Map (Qualified TypeName) ElaraKind
primKindCheckContext =
    -- assume all primitive types are kind Type
    fromList ((\x -> (Qualified x primModuleName, TypeKind)) <$> primitiveTypes)
        <> fromList
            [ (Qualified ioName primModuleName, FunctionKind TypeKind TypeKind)
            ] -- Except for IO which is kind Type -> Type
