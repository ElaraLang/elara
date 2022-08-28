module Parse.LetDec where

import Elara.AST.Frontend as AST
import Elara.Data.Module as Mod
import Elara.Data.Name
import NeatInterpolation
import Parse.Common
import Test.Hspec (Spec, describe, it)

testModuleName :: ModuleName
testModuleName = ModuleName ("Main" :| [])

spec :: Spec
spec = describe "Test Let Dec Parser" $ do
  it "Parses a simple let declaration" $ do
    "let x = 1"
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression = Identity (Int 1),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }

  it "Parses an indented let declaration" $
    do
      [text|
      let x = 
        1
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression = Identity (Int 1),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses an indented let declaration with multiple lines" $
    do
      [text|
      let x = 
        1
        2
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Block
                        [ Identity (Int 1),
                          Identity (Int 2)
                        ]
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }

  it "Parses a normal let with an indented lambda" $
    do
      [text|
      let x = \y ->
               1
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body = Identity (Int 1)
                        }
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses a normal let with a non-indented lambda" $
    do
      [text|
      let x = 
        \y -> 1
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body =
                            Identity
                              ( Int 1
                              )
                        }
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses an indented let with an indented lambda" $
    [text|
      let x = 
        \y ->
            1
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body =
                            Identity
                              ( Int 1
                              )
                        }
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses a nested let" $
    do
      [text|
      let main = 
        let x = 1
        x
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "main",
          _declarationBody =
            Value
              { _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing,
                _declarationBodyExpression =
                  Identity
                    ( Block
                        [ Identity $
                            Let
                              (Name "x")
                              []
                              (Identity (Int 1)),
                          Identity (Var $ Name "x")
                        ]
                    )
              }
        }
  it "Parses a nested let with some indentation" $
    do
      [text|
      let main = 
        let x =
          1
        x
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "main",
          _declarationBody =
            Value
              { _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing,
                _declarationBodyExpression =
                  Identity
                    ( Block
                        [ Identity $
                            Let
                              (Name "x")
                              []
                              (Identity (Int 1)),
                          Identity (Var $ Name "x")
                        ]
                    )
              }
        }