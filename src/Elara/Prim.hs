{-# LANGUAGE OverloadedLists #-}

{- | Stores information about the primitive types and functions of Elara.
These are still written in the source code, with a special name and value.
The compiler will then replace these with the actual primitive functions.

This module is the single source of truth for primitive type metadata.
Downstream modules ('Elara.TypeInfer.Render', 'Elara.ToCore', etc.)
should use the registry functions here instead of hardcoding primitive names.
-}
module Elara.Prim (
    -- * Known Type Registry
    OpaquePrim (..),
    WiredInPrim (..),
    KnownType (..),
    KnownTypeInfo (..),
    knownTypeInfo,
    wiredInPrimCtors,
    lookupByInternalName,
    lookupByUserName,
    lookupByQualifiedTypeName,
    allOpaquePrims,
    allWiredInPrims,
    allKnownTypes,

    -- * Primitive Operations
    PrimOp (..),
    parsePrimOp,
    primOpArity,

    -- * Module & Region Utilities
    primModuleName,
    primRegion,
    primLocated,
    mkPrimQual,
    mkPrimVarRef,
    elaraPrimitiveName,
    primitiveTypes,

    -- * Fixity Annotations
    fixityAnnotationName,
    associativityAnnotationName,
    leftAssociativeAnnotationName,
    rightAssociativeAnnotationName,
    nonAssociativeAnnotationName,

    -- * Kind Context
    primKindCheckContext,

    -- * Constructor Names
    trueCtorName,
    falseCtorName,
    nilCtorName,
    consCtorName,
    tuple2CtorName,
    ltCtorName,
    eqCtorName,
    gtCtorName,
    unitCtorName,
)
where

import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Elara.AST.Name (ModuleName (..), Qualified (..), TypeName (..))
import Elara.AST.Region (Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Pretty (Pretty (..))

-- ────────────────────────────────────────────────────────────────────────────
-- Known Type Registry
-- ────────────────────────────────────────────────────────────────────────────

{- | Opaque primitives: types with no source definition, backed directly by the backend.
Each gets an internal @Prim_*@ name and a user-facing alias in @Elara.Prim.elr@.
-}
data OpaquePrim
    = PrimInt
    | PrimFloat
    | PrimDouble
    | PrimString
    | PrimChar
    | PrimIO
    deriving (Eq, Ord, Show, Enum, Bounded, Data)

{- | Wired-in primitives: defined as normal source code in @stdlib/Elara.Prim.elr@,
but structurally required for the language to function (e.g. desugaring targets).
This includes @Bool@, @List@, @Ordering@, and @Tuple2@.
-}
data WiredInPrim
    = WiredInBool
    | WiredInList
    | WiredInTuple2
    | WiredInOrdering
    | WiredInUnit
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | All types the compiler needs to reference by name.
data KnownType
    = KnownOpaque OpaquePrim
    | KnownWiredIn WiredInPrim
    deriving (Eq, Ord, Show)

-- | Core metadata for each known type.
data KnownTypeInfo = KnownTypeInfo
    { knownInternalName :: !TypeName
    -- ^ The internal compiler name, e.g. @TypeName "Prim_Int"@
    , knownUserFacingName :: !Text
    -- ^ The user-visible name for error messages, e.g. @"Int"@
    , knownQualified :: !(Qualified TypeName)
    -- ^ Fully qualified, e.g. @Qualified (TypeName "Prim_Int") primModuleName@
    }
    deriving (Show)

-- | The registry: metadata for each known type.
knownTypeInfo :: KnownType -> KnownTypeInfo
knownTypeInfo = \case
    KnownOpaque PrimInt -> mkInfo "Prim_Int" "Int"
    KnownOpaque PrimFloat -> mkInfo "Prim_Float" "Float"
    KnownOpaque PrimDouble -> mkInfo "Prim_Double" "Double"
    KnownOpaque PrimString -> mkInfo "Prim_String" "String"
    KnownOpaque PrimChar -> mkInfo "Prim_Char" "Char"
    KnownOpaque PrimIO -> mkInfo "Prim_IO" "IO"
    KnownWiredIn WiredInBool -> mkInfo "Bool" "Bool"
    KnownWiredIn WiredInList -> mkInfo "List" "List"
    KnownWiredIn WiredInTuple2 -> mkInfo "Tuple2" "Tuple2"
    KnownWiredIn WiredInOrdering -> mkInfo "Ordering" "Ordering"
    KnownWiredIn WiredInUnit -> mkInfo "Unit" "Unit"
  where
    mkInfo :: Text -> Text -> KnownTypeInfo
    mkInfo internal user =
        KnownTypeInfo
            (TypeName internal)
            user
            (mkPrimQual (TypeName internal))

-- | The unqualified constructor names for each wired-in primitive.
wiredInPrimCtors :: IsString s => WiredInPrim -> [Qualified s]
wiredInPrimCtors = \case
    WiredInBool -> [trueCtorName, falseCtorName]
    WiredInList -> [nilCtorName, consCtorName]
    WiredInTuple2 -> [tuple2CtorName]
    WiredInOrdering -> [ltCtorName, eqCtorName, gtCtorName]
    WiredInUnit -> [unitCtorName]

allOpaquePrims :: [OpaquePrim]
allOpaquePrims = universe

allWiredInPrims :: [WiredInPrim]
allWiredInPrims = universe

allKnownTypes :: [KnownType]
allKnownTypes = (KnownOpaque <$> allOpaquePrims) ++ (KnownWiredIn <$> allWiredInPrims)

{- | Look up a known type by its internal 'TypeName' (e.g. @TypeName "Prim_Int"@).
Used by 'Elara.TypeInfer.Render' to map internal names to user-facing ones.
-}
lookupByInternalName :: TypeName -> Maybe KnownType
lookupByInternalName tn = Map.lookup tn internalNameMap
  where
    internalNameMap :: Map TypeName KnownType
    internalNameMap = Map.fromList [(knownInternalName (knownTypeInfo p), p) | p <- allKnownTypes]
    {-# NOINLINE internalNameMap #-}

-- | Look up a known type by its user-facing name (e.g. @"Int"@).
lookupByUserName :: Text -> Maybe KnownType
lookupByUserName name = Map.lookup name userNameMap
  where
    userNameMap :: Map Text KnownType
    userNameMap = Map.fromList [(knownUserFacingName (knownTypeInfo p), p) | p <- allKnownTypes]
    {-# NOINLINE userNameMap #-}

{- | Check if a qualified type name refers to a known type.
Used by 'Elara.ToCore' to short-circuit TyCon lookups.
-}
lookupByQualifiedTypeName :: Qualified TypeName -> Maybe KnownType
lookupByQualifiedTypeName (Qualified tn modName)
    | modName == primModuleName = lookupByInternalName tn
    | otherwise = Nothing

{- | Primitive operations, resolved from @elaraPrimitive@ string keys during ToCore.
This is backend-agnostic; the JVM backend maps these to its own 'Elara.JVM.IR.PrimOp'.
-}
data PrimOp
    = PrimIntAdd
    | PrimIntSubtract
    | PrimIntMultiply
    | PrimIntDivide
    | PrimIntRemainder
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

instance Pretty PrimOp

{- | Parse an @elaraPrimitive@ string key into a typed 'PrimOp'.
This is the single source of truth for the string → PrimOp mapping.
-}
parsePrimOp :: Text -> Maybe PrimOp
parsePrimOp = \case
    "+" -> Just PrimIntAdd
    "-" -> Just PrimIntSubtract
    "*" -> Just PrimIntMultiply
    "/" -> Just PrimIntDivide
    "%" -> Just PrimIntRemainder
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
    PrimIntDivide -> 2
    PrimIntRemainder -> 2
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

elaraPrimitiveName :: IsString s => Qualified s
elaraPrimitiveName = mkPrimQual "elaraPrimitive"

{- | Type names to seed into the primitive rename state.
Only opaque primitives — wired-in ADTs are discovered via normal imports.
Note: @()@ (unit syntax) is seeded separately in 'Elara.Prim.Rename' since
it uses a syntactic form that cannot appear as a module-level declaration.
-}
primitiveTypes :: [TypeName]
primitiveTypes =
    knownInternalName . knownTypeInfo . KnownOpaque <$> allOpaquePrims

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
    -- Higher-kinded overrides must come first since Map.<> is left-biased
    fromList
        [ (knownQualified (knownTypeInfo (KnownOpaque PrimIO)), FunctionKind TypeKind TypeKind)
        , (knownQualified (knownTypeInfo (KnownWiredIn WiredInList)), FunctionKind TypeKind TypeKind)
        , (knownQualified (knownTypeInfo (KnownWiredIn WiredInTuple2)), FunctionKind TypeKind (FunctionKind TypeKind TypeKind))
        ]
        <> fromList ((\kt -> (knownQualified (knownTypeInfo kt), TypeKind)) <$> allKnownTypes)

trueCtorName, falseCtorName :: IsString s => Qualified s
trueCtorName = mkPrimQual "True"
falseCtorName = mkPrimQual "False"

nilCtorName, consCtorName :: IsString s => Qualified s
nilCtorName = mkPrimQual "Nil"
consCtorName = mkPrimQual "Cons"

tuple2CtorName :: IsString s => Qualified s
tuple2CtorName = mkPrimQual "Tuple2"

ltCtorName, eqCtorName, gtCtorName :: IsString s => Qualified s
ltCtorName = mkPrimQual "LT"
eqCtorName = mkPrimQual "EQ"
gtCtorName = mkPrimQual "GT"

unitCtorName :: IsString s => Qualified s
unitCtorName = mkPrimQual "Unit"
