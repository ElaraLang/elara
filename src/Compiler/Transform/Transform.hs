{-# LANGUAGE OverloadedStrings #-}

module Compiler.Transform.Transform (transform) where

import qualified Compiler.ClassFile as C
import qualified Compiler.Transform.Abstract as A
import Control.Monad.State
import qualified Data.Bimap as M
import Data.Bits ((.|.))
import Data.Text.Encoding
import qualified Data.Vector as V
import Data.Word (Word16)

data TransformState = TransformState
  { constants :: M.Bimap Word16 A.ConstantPoolEntry,
    currentClass :: C.ClassFile
  }

type Mutate a = State TransformState a

transform :: A.ClassFile -> C.ClassFile
transform clazz = evalState (transformClass clazz) (TransformState M.empty emptyClass)

emptyClass :: C.ClassFile
emptyClass =
  C.ClassFile
    { C.majorVersion = 0,
      C.minorVersion = 0,
      C.magic = 0,
      C.accessFlags = 0,
      C.thisClass = 0,
      C.superClass = 0,
      C.interfaces = [],
      C.fields = [],
      C.methods = [],
      C.attributes = [],
      C.constantPool = V.empty
    }

transformClass :: A.ClassFile -> Mutate C.ClassFile
transformClass clazz = do
  let name = A.className clazz
  nameIndex <- getConstantIndex (A.CPUTF8Entry name) -- Insert the class name
  classNameIndex <- getConstantIndex (A.CPClassEntry nameIndex) -- Class Info for the class name
  let superName = A.superName clazz
  superNameIndex <- getConstantIndex (A.CPUTF8Entry superName) -- Insert the super class name
  superClassIndex <- getConstantIndex (A.CPClassEntry superNameIndex) -- Class Info for the super class name
  mapM_ transformField (A.fields clazz)
  newConstants <- transformConstantPool

  fields <- gets (C.fields . currentClass)
  return
    C.ClassFile
      { C.magic = 0xCAFEBABE,
        C.minorVersion = 0,
        C.majorVersion = 60,
        C.accessFlags = 0,
        C.thisClass = classNameIndex,
        C.superClass = superClassIndex,
        C.constantPool = newConstants,
        C.interfaces = [],
        C.fields = fields,
        C.methods = [],
        C.attributes = []
      }

loadConstants :: Mutate [C.ConstantPoolInfo]
loadConstants = do
  constantMap <- gets constants
  let asList = M.toList constantMap
  infos <- mapM (transformConstant . snd) asList
  newConsts <- gets constants
  if M.size newConsts /= M.size constantMap
    then loadConstants
    else return infos

transformConstantPool :: Mutate (V.Vector C.ConstantPoolInfo)
transformConstantPool = V.fromList <$> loadConstants

transformConstant :: A.ConstantPoolEntry -> Mutate C.ConstantPoolInfo
transformConstant (A.CPUTF8Entry str) = return $ C.UTF8Info $ Data.Text.Encoding.encodeUtf8 str
transformConstant (A.CPClassEntry index) = return $ C.ClassInfo index
transformConstant (A.CPIntegerEntry value) = return $ C.IntegerInfo $ fromIntegral value
transformConstant (A.CPStringEntry str) = do
  strIndex <- getConstantIndex (A.CPUTF8Entry str)
  return $ C.StringInfo strIndex
transformConstant a = error $ "Can't transform " ++ show a

getConstantIndex :: A.ConstantPoolEntry -> Mutate Word16
getConstantIndex constant = do
  s <- get
  let consts = constants s
  index <- case M.lookupR constant consts of
    Just i -> return i
    Nothing -> do
      let newIndex = fromIntegral $ M.size consts
      let newConstants = M.insert newIndex constant consts
      put $ s {constants = newConstants}
      return newIndex
  return (index + 1) -- +1 because the constant pool starts at 1

transformField :: A.Field -> Mutate ()
transformField field = do
  let name = A.name field
  nameIndex <- getConstantIndex (A.CPUTF8Entry name)
  descriptorIndex <- getConstantIndex (A.CPUTF8Entry $ A.descriptor field)
  attributes <- transformAttributes (A.attributes field)
  let fieldInfo =
        C.FieldInfo
          { C.fieldAccessFlags = foldl (.|.) 0 (A.accessFlagValue <$> A.accessFlags field),
            C.fieldNameIndex = nameIndex,
            C.descriptorIndex = descriptorIndex,
            C.fieldAttributes = attributes
          }
  s <- get
  let classFile = currentClass s
  put $ s {currentClass = classFile {C.fields = fieldInfo : C.fields classFile}}

transformAttributes :: [A.Attribute] -> Mutate [C.AttributeInfo]
transformAttributes = mapM transformAttribute
  where
    transformAttribute :: A.Attribute -> Mutate C.AttributeInfo
    transformAttribute (A.ConstantValueAttribute entry) = do
      nameIndex <- getConstantIndex (A.CPUTF8Entry "ConstantValue")
      valueIndex <- getConstantIndex entry
      return $ C.AttributeInfo nameIndex (C.ConstantValueAttribute valueIndex)
