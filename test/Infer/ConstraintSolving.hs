{-# OPTIONS_GHC -Wno-missing-methods #-}

module Infer.ConstraintSolving where

import Elara.AST.Name (Name (..), Qualified (..))
import Elara.AST.VarRef (VarRef' (Global), mkGlobal')
import Elara.Data.Unique (unsafeMkUnique)
import Elara.Prim (primLocated, primModuleName)
import Elara.TypeInfer.ConstraintSolving as Constraint
import Elara.TypeInfer.Context
import Elara.TypeInfer.Existential (Existential (..))
import Elara.TypeInfer.Monotype qualified as MonoType
import Elara.TypeInfer.Type as Type
import Test.Hspec
import Test.Hspec.Hedgehog

ignoreLocVarRef v = mkGlobal' (primLocated (Qualified (NVarName v) primModuleName))

instance Num (Existential a) where
    fromInteger i = UnsafeExistential (unsafeMkUnique Nothing (fromInteger i))

spec :: Spec
spec = describe "ConstraintSolving" $ do
    it "works with a single constraint" $ hedgehog $ do
        let context = [Annotation (ignoreLocVarRef "x") (Type.Scalar () MonoType.Natural)]
        let constraints = createConstraints context
        constraints === [Constraint.EmptyConstraint]

    it "works with 2 constraints" $ hedgehog $ do
        let context =
                [ Annotation (ignoreLocVarRef "x") (Type.Scalar () MonoType.Natural)
                , Annotation (ignoreLocVarRef "x") (Type.UnsolvedType () 0)
                ]
        let constraints = createConstraints context
        constraints
            === [ Constraint.Equality
                    (Type.Scalar () MonoType.Natural)
                    (Type.UnsolvedType () 0)
                ]

    it "works with 3 constraints" $ hedgehog $ do
        let context =
                [ Annotation (ignoreLocVarRef "x") (Type.Scalar () MonoType.Natural)
                , Annotation (ignoreLocVarRef "x") (Type.UnsolvedType () 0)
                , Annotation (ignoreLocVarRef "x") (Type.UnsolvedType () 1)
                ]
        let constraints = createConstraints context
        constraints
            === [ Constraint.Conjunction
                    ( Constraint.Equality
                        (Type.Scalar () MonoType.Natural)
                        (Type.UnsolvedType () 0)
                    )
                    ( Constraint.Equality
                        (Type.Scalar () MonoType.Natural)
                        (Type.UnsolvedType () 1)
                    )
                ]
