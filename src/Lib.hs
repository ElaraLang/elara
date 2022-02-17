{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Compiler.ClassFile
import Control.Monad (forM_)
import Data.Binary.Put
import Data.Bits ((.&.), (.|.))
import Data.ByteString.Lazy as L (unpack, writeFile)
import Data.List (intercalate)
import Interpreter.Execute
import Parse.AST
import Parse.Parser
import Parse.Reader
import Parse.Utils
import Preprocess.Preprocessor

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  print content
  let tokens = evalP readTokens content
  putStrLn "Tokens: "
  print tokens
  let ast = parse content
  env <- initialEnvironment
  forM_ ast $ \line -> do
    print line
    let preprocessed = preprocess line
    putStrLn "Preprocessed AST: "
    print preprocessed
    execute preprocessed env

  let s =
        runPut $
          putClassFile $
            ClassFile
              { magic = 0xCAFEBABE,
                minorVersion = 0,
                majorVersion = 60,
                accessFlags = accPublic .|. accSuper,
                constantPool =
                  [ ClassInfo 3, -- 1
                    ClassInfo 4, -- 2
                    UTF8Info (unpack "test"), -- 3
                    UTF8Info (unpack "java/lang/Object"), -- 4
                    UTF8Info $ unpack "main", -- 5
                    UTF8Info $ unpack "([Ljava/lang/String;)V", -- 6
                    UTF8Info $ unpack "Code", -- 7
                    ClassInfo 9, -- 9
                    UTF8Info $ unpack "java/lang/System", -- 9
                    UTF8Info $ unpack "out", -- 10
                    UTF8Info $ unpack "Ljava/io/PrintStream;", -- 11
                    NameAndTypeInfo (NameAndType 10 11), -- 12
                    FieldRefInfo 8 12, -- 13
                    StringInfo 15, -- 14
                    UTF8Info $ unpack "Hello, World!", -- 15
                    UTF8Info $ unpack "java/io/PrintStream", -- 16
                    UTF8Info $ unpack "println", -- 17
                    UTF8Info $ unpack "(Ljava/lang/String;)V", -- 18
                    ClassInfo 16, -- 19
                    NameAndTypeInfo (NameAndType 17 18), -- 20
                    MethodRefInfo 19 20 -- 21
                  ],
                thisClass = 1,
                superClass = 2,
                interfaces = [],
                fields = [],
                methods =
                  [ MethodInfo
                      { methodAccessFlags = accPublic .|. accStatic,
                        methodNameIndex = 5,
                        methodDescriptorIndex = 6,
                        methodAttributes =
                          [ AttributeInfo
                              { attributeNameIndex = 7,
                                attributeInfo =
                                  CodeAttribute
                                    { maxStack = 2,
                                      maxLocals = 1,
                                      code =
                                        [ 178,
                                          00,
                                          13,
                                          18,
                                          14,
                                          182,
                                          00,
                                          21,
                                          177
                                        ],
                                      exceptionTable = [],
                                      codeAttributes = []
                                    }
                              }
                          ]
                      }
                  ],
                attributes = []
              }
  L.writeFile "test.class" s
