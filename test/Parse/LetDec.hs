module Parse.LetDec where

import Elara.AST.Frontend.Unlocated as AST
import Elara.AST.Module as Mod
import Elara.AST.Name
import NeatInterpolation
import Parse.Common
import Orphans ()
import Test.Hspec (Spec, describe, it)

testModuleName :: ModuleName
testModuleName = ModuleName ("Main" :| [])

spec :: Spec
spec = describe "Test Let Dec Parser" $ do
    it "Parses a simple let declaration" $
        "let x = 1"
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "x"
                , _declarationBody =
                    Value
                        { _declarationBodyExpression = Int 1
                        , _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        }
                }

    it "Parses an indented let declaration" $
        [text|
      let x = 
        1
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "x"
                , _declarationBody =
                    Value
                        { _declarationBodyExpression = Int 1
                        , _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        }
                }
    it "Parses an indented let declaration with multiple lines" $
        [text|
      let x = 
        1
        2
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "x"
                , _declarationBody =
                    Value
                        { _declarationBodyExpression =
                            Block
                                [ Int 1
                                , Int 2
                                ]
                        , _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        }
                }

    it "Parses a normal let with an indented lambda" $
        [text|
      let x = \y ->
               1
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "x"
                , _declarationBody =
                    Value
                        { _declarationBodyExpression =
                            Lambda [NamedPattern "y"] (Int 1)
                        , _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        }
                }
    it "Parses a normal let with a non-indented lambda" $
        [text|
      let x = 
        \y -> 1
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "x"
                , _declarationBody =
                    Value
                        { _declarationBodyExpression =
                            Lambda [NamedPattern "y"] (Int 1)
                        , _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        }
                }
    it "Parses an indented let with an indented lambda" $
        [text|
      let x = 
        \y ->
            1
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "x"
                , _declarationBody =
                    Value
                        { _declarationBodyExpression =
                            Lambda [NamedPattern "y"] (Int 1)
                        , _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        }
                }
    it "Parses a nested let" $
        [text|
      let main = 
        let x = 1
        x
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "main"
                , _declarationBody =
                    Value
                        { _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        , _declarationBodyExpression =
                            Block
                                [ Let
                                    "x"
                                    []
                                    (Int 1)
                                , Var "x"
                                ]
                        }
                }
    it "Parses a nested let with some indentation" $
        [text|
      let main = 
        let x =
          1
        x
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "main"
                , _declarationBody =
                    Value
                        { _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        , _declarationBodyExpression =
                            Block
                                [ Let
                                    "x"
                                    []
                                    (Int 1)
                                , Var "x"
                                ]
                        }
                }

    it "Parses a comically nested let with some indentation" $
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
                { _declarationModule' = testModuleName
                , _declarationName = "main"
                , _declarationBody =
                    Value
                        { _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        , _declarationBodyExpression =
                            let gen n b = Let n [] (Block b)
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
                                                    [ Let "c" [] (Int 1)
                                                    , Int 2
                                                    ]
                                                , Int 3
                                                ]
                                            , Int 4
                                            ]
                                        , Int 5
                                        ]
                                    , Int 6
                                    ]
                        }
                }
    it "Parses a let declaration with a let .. in as its body" $
        [text|
      let main = 
        let x = 3 in x
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "main"
                , _declarationBody =
                    Value
                        { _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        , _declarationBodyExpression =
                            LetIn
                                "x"
                                []
                                (Int 3)
                                (Var "x")
                        }
                }
    it "Parses a let declaration with a let and a let .. in as its body" $
        [text|
      let main =
        let x = 3
        let y = 2 in x
      |]
            <: Declaration
                { _declarationModule' = testModuleName
                , _declarationName = "main"
                , _declarationBody =
                    Value
                        { _declarationBodyPatterns = []
                        , _declarationBodyTypeAnnotation = Nothing
                        , _declarationBodyExpression =
                            Block
                                [ Let "x" [] (Int 3)
                                , LetIn
                                    "y"
                                    []
                                    (Int 2)
                                    (Var "x")
                                ]
                        }
                }