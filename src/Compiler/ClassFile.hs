module Compiler.ClassFile where

import Data.Binary.Put
import Data.Word
import Debug.Trace (traceShow, traceShowM)
import qualified Data.ByteString.Lazy as L

data ClassFile = ClassFile
  { magic :: Word32,
    minorVersion :: Word16,
    majorVersion :: Word16,
    constantPool :: [ConstantPoolInfo],
    accessFlags :: Word16,
    thisClass :: Word16,
    superClass :: Word16,
    interfaces :: [Word16],
    fields :: [FieldInfo],
    methods :: [MethodInfo],
    attributes :: [AttributeInfo]
  }

data ConstantPoolInfo =
  ClassInfo { classNameIndex :: Word16 }
  | UTF8Info { utf8InfoBytes :: [Word8] }
  | FieldRefInfo { fieldRefClassIndex :: Word16, fieldRefNameAndTypeIndex :: Word16 }
  | MethodRefInfo { methodRefClassIndex :: Word16, methodRefNameAndTypeIndex :: Word16 }
  | NameAndTypeInfo NameAndType
  | StringInfo { stringIndex :: Word16 }
  deriving (Show)

data NameAndType = NameAndType { nameAndTypeName :: Word16, nameAndTypeDescriptor :: Word16 }
  deriving (Show)

data FieldInfo = FieldInfo
  { fieldAccessFlags :: Word16,
    fieldNameIndex :: Word16,
    descriptorIndex :: Word16,
    fieldAttributes :: [AttributeInfo]
  }

data AttributeInfo = AttributeInfo
  { attributeNameIndex :: Word16,
    attributeInfo :: Attribute
  }

data Attribute =
  CodeAttribute {
    maxStack :: Word16,
    maxLocals :: Word16,
    code :: [Word8],
    exceptionTable :: [ExceptionTableEntry],
    codeAttributes :: [AttributeInfo]
  }

data ExceptionTableEntry = ExceptionTableEntry
  { startPc :: Word16,
    endPc :: Word16,
    handlerPc :: Word16,
    catchType :: Word16
  }

data MethodInfo = MethodInfo
  { methodAccessFlags :: Word16,
    methodNameIndex :: Word16,
    methodDescriptorIndex :: Word16,
    methodAttributes :: [AttributeInfo]
  }

putConstantPool :: [ConstantPoolInfo] -> Put
putConstantPool entries = do
  putWord16be $ fromIntegral (length entries + 1)
  mapM_ putConstantPoolInfo entries


putConstantPoolInfo :: ConstantPoolInfo -> Put
putConstantPoolInfo (ClassInfo ciNameIndex) = do
  putWord8 7
  putWord16be ciNameIndex
putConstantPoolInfo (UTF8Info uiBytes) = do
  putWord8 1
  putWord16be $ fromIntegral $ length uiBytes
  mapM_ putWord8 uiBytes
putConstantPoolInfo (FieldRefInfo frClassIndex frNameAndTypeIndex) = do
  putWord8 9
  putWord16be frClassIndex
  putWord16be frNameAndTypeIndex
putConstantPoolInfo (MethodRefInfo mrClassIndex mrNameAndTypeIndex) = do
  putWord8 10
  putWord16be mrClassIndex
  putWord16be mrNameAndTypeIndex
putConstantPoolInfo (NameAndTypeInfo nameAndType) = do
  putWord8 12
  putNameAndType nameAndType
putConstantPoolInfo (StringInfo siIndex) = do
  putWord8 8
  putWord16be siIndex

putNameAndType :: NameAndType -> Put
putNameAndType (NameAndType name descriptor) = do
  putWord16be name
  putWord16be descriptor


putInterfaceTable :: [Word16] -> Put
putInterfaceTable interfaces = do
  putWord16be $ fromIntegral (length interfaces)
  mapM_ putWord16be interfaces

putFieldTable :: [FieldInfo] -> Put
putFieldTable fields = do
  putWord16be $ fromIntegral (length fields)
  mapM_ putFieldInfo fields

putFieldInfo :: FieldInfo -> Put
putFieldInfo (FieldInfo fiAccessFlags fiNameIndex fiDescriptorIndex fiAttributes) = do
  putWord16be fiAccessFlags
  putWord16be fiNameIndex
  putWord16be fiDescriptorIndex
  putAttributes fiAttributes

putAttributes :: [AttributeInfo] -> Put
putAttributes attributes = do
  putWord16be $ fromIntegral (length attributes)
  mapM_ putAttributeInfo attributes
  where putAttributeInfo (AttributeInfo aiNameIndex aiInfo) = do
          putWord16be aiNameIndex
          let info  = runPut $ putAttribute aiInfo
          let aiLength = L.length info
          putWord32be $ fromIntegral aiLength
          putLazyByteString info

putAttribute :: Attribute -> Put
putAttribute (CodeAttribute maxStack maxLocals code exceptionTable codeAttributes) = do
  putWord16be maxStack
  putWord16be maxLocals
  putWord32be $ fromIntegral $ length code
  mapM_ putWord8 code
  putExceptionTable exceptionTable
  putAttributes codeAttributes

putExceptionTable :: [ExceptionTableEntry] -> Put
putExceptionTable exceptionTable = do
  putWord16be $ fromIntegral (length exceptionTable)
  mapM_ putExceptionTableEntry exceptionTable
  where putExceptionTableEntry (ExceptionTableEntry startPc endPc handlerPc catchType) = do
          putWord16be startPc
          putWord16be endPc
          putWord16be handlerPc
          putWord16be catchType

putMethodTable :: [MethodInfo] -> Put
putMethodTable methods = do
  putWord16be $ fromIntegral (length methods)
  mapM_ putMethodInfo methods
  where putMethodInfo (MethodInfo miAccessFlags miNameIndex miDescriptorIndex miAttributes) = do
          putWord16be miAccessFlags
          putWord16be miNameIndex
          putWord16be miDescriptorIndex
          putAttributes miAttributes

putClassFile :: ClassFile -> Put
putClassFile classfile = do
  putWord32be $ magic classfile
  putWord16be $ minorVersion classfile
  putWord16be $ majorVersion classfile
  putConstantPool $ constantPool classfile
  putWord16be $ accessFlags (classfile :: ClassFile)
  putWord16be $ thisClass (classfile :: ClassFile)
  putWord16be $ superClass (classfile :: ClassFile)
  putInterfaceTable $ interfaces classfile
  putFieldTable $ fields classfile
  putMethodTable $ methods classfile
  putAttributes $ attributes (classfile :: ClassFile)


-- Access flags
accPublic :: Word16
accPublic = 0x0001 :: Word16

accPrivate :: Word16
accPrivate = 0x0002 :: Word16

accProtected :: Word16
accProtected = 0x0004 :: Word16

accStatic :: Word16
accStatic = 0x0008 :: Word16

accFinal  :: Word16
accFinal = 0x0010 :: Word16

accSynchronized :: Word16
accSynchronized = 0x0020 :: Word16

accBridge :: Word16
accBridge = 0x0040 :: Word16

accVarargs :: Word16
accVarargs = 0x0080 :: Word16

accNative :: Word16
accNative = 0x0100 :: Word16

accAbstract :: Word16
accAbstract = 0x0400 :: Word16

acStrict :: Word16
acStrict = 0x0800 :: Word16

accSynthetic :: Word16
accSynthetic = 0x1000 :: Word16

accSuper :: Word16
accSuper = 0x0020 :: Word16

accInterface :: Word16
accInterface = 0x0200 :: Word16

accAnnotation :: Word16
accAnnotation = 0x2000 :: Word16

accEnum :: Word16
accEnum = 0x4000 :: Word16
