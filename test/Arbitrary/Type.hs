module Arbitrary.Type where

import Elara.TypeInfer.Monotype qualified as Scalar
import Elara.TypeInfer.Type
import Hedgehog
import Hedgehog.Gen qualified as Gen

arbitraryType :: Gen (Type ())
arbitraryType =
    Gen.recursive
        Gen.choice
        [ Scalar ()
            <$> Gen.choice
                [ pure Scalar.Unit
                , pure Scalar.Bool
                , pure Scalar.Integer
                , pure Scalar.Real
                , pure Scalar.Char
                , pure Scalar.Text
                ]
        ]
        [Gen.subterm2 arbitraryType arbitraryType (Function ())]
