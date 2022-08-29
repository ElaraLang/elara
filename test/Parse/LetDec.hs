module Parse.LetDec where

import Elara.AST.Frontend as AST
import Elara.Data.Located (TypeIdentity (..))
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
              { _declarationBodyExpression = TypeIdentity (Int 1),
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
              { _declarationBodyExpression = TypeIdentity (Int 1),
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
                  TypeIdentity
                    ( Block
                        [ TypeIdentity (Int 1),
                          TypeIdentity (Int 2)
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
                  TypeIdentity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body = TypeIdentity (Int 1)
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
                  TypeIdentity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body =
                            TypeIdentity
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
                  TypeIdentity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body =
                            TypeIdentity
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
                  TypeIdentity
                    ( Block
                        [ TypeIdentity $
                            Let
                              (Name "x")
                              []
                              (TypeIdentity (Int 1)),
                          TypeIdentity (Var $ Name "x")
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
                  TypeIdentity
                    ( Block
                        [ TypeIdentity $
                            Let
                              (Name "x")
                              []
                              (TypeIdentity (Int 1)),
                          TypeIdentity (Var $ Name "x")
                        ]
                    )
              }
        }

  it "Parses a comically nested let with some indentation" $
    do
      [text|
      let main = 
        let x =
          let y = 
            let z =
              let a = 
                let b =
                  let c =
                    1
                  2
                3
              4
            5
          6
      |]
      <: Declaration
        { _declarationModule_ = testModuleName,
          _declarationName = Name "main",
          _declarationBody =
            Value
              { _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing,
                _declarationBodyExpression =
                  let gen n b = TypeIdentity (Let (Name n) [] (TypeIdentity $ Block b))
                   in gen
                        "x"
                        [ gen
                            "y"
                            [ gen
                                "z"
                                [ gen
                                    "a"
                                    [ gen
                                        "b"
                                        [ TypeIdentity (Let (Name "c") [] (TypeIdentity $ Int 1)),
                                          TypeIdentity (Int 2)
                                        ],
                                      TypeIdentity (Int 3)
                                    ],
                                  TypeIdentity (Int 4)
                                ],
                              TypeIdentity (Int 5)
                            ],
                          TypeIdentity (Int 6)
                        ]
              }
        }
