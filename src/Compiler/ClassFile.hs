module Compiler.ClassFile where

import Compiler.Instruction
import Data.Binary.Builder (toLazyByteString)
import Data.Binary.Put
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Vector as V
import Data.Word
import Debug.Trace

data ClassFile = ClassFile
  { magic :: Word32,
    minorVersion :: Word16,
    majorVersion :: Word16,
    constantPool :: V.Vector ConstantPoolInfo,
    accessFlags :: Word16,
    thisClass :: Word16,
    superClass :: Word16,
    interfaces :: [Word16],
    fields :: [FieldInfo],
    methods :: [MethodInfo],
    attributes :: [AttributeInfo]
  }

data ConstantPoolInfo
  = ClassInfo {classNameIndex :: Word16}
  | UTF8Info {utf8InfoBytes :: B.ByteString}
  | FieldRefInfo {fieldRefClassIndex :: Word16, fieldRefNameAndTypeIndex :: Word16}
  | MethodRefInfo {methodRefClassIndex :: Word16, methodRefNameAndTypeIndex :: Word16}
  | NameAndTypeInfo NameAndType
  | StringInfo {stringIndex :: Word16}
  | IntegerInfo {integerValue :: Word32}
  deriving (Show, Eq, Ord)

data NameAndType = NameAndType {nameAndTypeName :: Word16, nameAndTypeDescriptor :: Word16}
  deriving (Show, Eq, Ord)

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

data Attribute
  = CodeAttribute
      { maxStack :: Word16,
        maxLocals :: Word16,
        code :: [Instruction],
        exceptionTable :: [ExceptionTableEntry],
        codeAttributes :: [AttributeInfo]
      }
  | ConstantValueAttribute Word16

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

putConstantPool :: V.Vector ConstantPoolInfo -> Put
putConstantPool entries = do
  putWord16be $ fromIntegral (length entries + 1)
  mapM_ putConstantPoolInfo entries

putConstantPoolInfo :: ConstantPoolInfo -> Put
putConstantPoolInfo (ClassInfo ciNameIndex) = do
  putWord8 7
  putWord16be ciNameIndex
putConstantPoolInfo (UTF8Info uiBytes) = do
  putWord8 1
  putWord16be $ fromIntegral $ B.length uiBytes
  putByteString uiBytes
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
putConstantPoolInfo (IntegerInfo iValue) = do
  putWord8 3
  putWord32be iValue
putConstantPoolInfo other = error $ "putConstantPoolInfo: " ++ show other

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
  where
    putAttributeInfo (AttributeInfo aiNameIndex aiInfo) = do
      putWord16be aiNameIndex
      let info = toStrict $ runPut $ putAttribute aiInfo
      let aiLength = B.length info
      putWord32be $ fromIntegral aiLength
      putByteString info

-- Puts the attribute data. This must not include length or name, just the actual value
putAttribute :: Attribute -> Put
putAttribute (ConstantValueAttribute cvIndex) = do
  putWord16be cvIndex
putAttribute (CodeAttribute maxStack maxLocals code exceptionTable codeAttributes) = do
  putWord16be maxStack
  putWord16be maxLocals
  let codeStr = toStrict $ toLazyByteString $ execPut $ mapM_ putInstruction code
  putWord32be $ fromIntegral $ B.length codeStr
  putByteString codeStr
  putExceptionTable exceptionTable
  putAttributes codeAttributes

putExceptionTable :: [ExceptionTableEntry] -> Put
putExceptionTable exceptionTable = do
  putWord16be $ fromIntegral (length exceptionTable)
  mapM_ putExceptionTableEntry exceptionTable
  where
    putExceptionTableEntry (ExceptionTableEntry startPc endPc handlerPc catchType) = do
      putWord16be startPc
      putWord16be endPc
      putWord16be handlerPc
      putWord16be catchType

putMethodTable :: [MethodInfo] -> Put
putMethodTable methods = do
  putWord16be $ fromIntegral (length methods)
  mapM_ putMethodInfo methods
  where
    putMethodInfo (MethodInfo miAccessFlags miNameIndex miDescriptorIndex miAttributes) = do
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
