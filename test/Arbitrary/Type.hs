module Arbitrary.Type where

import Elara.AST.Name
import Elara.Data.Unique (unsafeMkUnique)
import Elara.TypeInfer.Type
import Elara.TypeInfer.Unique
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Region (qualifiedTest)

-- | contrary to what the name suggests, this will NOT be unique :)
genUniqueTypeVar :: Gen TypeVariable
genUniqueTypeVar = UnificationVar <$> (unsafeMkUnique Nothing <$> Gen.integral (Range.linear 0 100))

typeConstructorNames :: [TypeName]
typeConstructorNames = ["List", "Maybe", "Pair", "Box", "IO"]

genMonotype :: Gen (Monotype ())
genMonotype =
    Gen.recursive
        Gen.choice
        [ TypeVar <$> genUniqueTypeVar
        , Scalar <$> Gen.enumBounded
        ]
        [ TypeConstructor <$> Gen.element (qualifiedTest <$> typeConstructorNames) <*> Gen.list (Range.linear 0 2) genMonotype
        , Function <$> genMonotype <*> genMonotype
        ]
