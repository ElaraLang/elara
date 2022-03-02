module Compiler.Transform.Abstract where

import Data.Int (Int64)
import Data.Text
import Data.Word (Word16)

data ClassFile = ClassFile
  { className :: ClassInfo,
    superName :: ClassInfo,
    --    interfaces :: [String],
    fields :: [Field]
    --    methods :: [Method]
  }
  deriving (Show)

type ClassInfo = Text

data AccessFlags
  = ACC_PUBLIC
  | ACC_PRIVATE
  | ACC_PROTECTED
  | ACC_STATIC
  | ACC_FINAL
  | ACC_VOLATILE
  | ACC_TRANSIENT
  | ACC_SUPER
  | ACC_INTERFACE
  | ACC_ABSTRACT
  | ACC_SYNTHETIC
  | ACC_ANNOTATION
  | ACC_ENUM
  deriving (Show)
  
accessFlagValue :: AccessFlags -> Word16
accessFlagValue ACC_PUBLIC = 0x0001
accessFlagValue ACC_PRIVATE = 0x0002
accessFlagValue ACC_PROTECTED = 0x0004
accessFlagValue ACC_STATIC = 0x0008
accessFlagValue ACC_FINAL = 0x0010
accessFlagValue ACC_VOLATILE = 0x0040
accessFlagValue ACC_TRANSIENT = 0x0080
accessFlagValue ACC_SUPER = 0x0020
accessFlagValue ACC_INTERFACE = 0x0200
accessFlagValue ACC_ABSTRACT = 0x0400
accessFlagValue ACC_SYNTHETIC = 0x1000
accessFlagValue ACC_ANNOTATION = 0x2000
accessFlagValue ACC_ENUM = 0x4000

data Field = Field
  { accessFlags :: [AccessFlags],
    name :: Text,
    descriptor :: Text,
    attributes :: [Attribute]
  }
  deriving (Show)

newtype Attribute
  = ConstantValueAttribute ConstantPoolEntry -- Constant value for a field
  deriving (Show)

data ConstantPoolEntry
  = CPClassEntry Word16 -- We use an index here because the class entry refers to an entry in the pool and idk whats going on anymore
  | CPFieldRefEntry ClassInfo NameAndType
  | CPMethodRefEntry ClassInfo NameAndType
  | CPInterfaceMethodRefEntry ClassInfo NameAndType
  | CPStringEntry Text
  | CPIntegerEntry Int
  | CPFloatEntry Float
  | CPLongEntry Int64
  | CPDoubleEntry Double
  | CPNameAndTypeEntry Text Text
  | CPUTF8Entry Text
  | CPMethodHandleEntry Int -- TODO figure this one out
  | CPMethodTypeEntry Text -- TODO and this one
  | CPInvokeDynamicEntry Int -- TODO and this one
  deriving (Show, Eq, Ord)

data NameAndType = NameAndType Text Text
  deriving (Show, Eq, Ord)
