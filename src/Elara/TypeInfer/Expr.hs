{-# LANGUAGE PartialTypeSignatures #-}

module Elara.TypeInfer.Expr where

import Control.Lens (mapped, over, traverseByOf, traverseOf, view, (^.), _1, _2)
import Data.Bitraversable (bisequenceA)
import Elara.AST.Name (ModuleName (ModuleName), Qualified (..), TypeName (TypeName))
import Elara.AST.Region (Located (Located), generatedSourcePos, generatedSourceRegion, unlocated)
import Elara.AST.Shunted qualified as Shunted
import Elara.AST.Typed
import Elara.Data.Unique (UniqueGen)

import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State
import Prelude hiding (Type)

-- inferExpression ::
--     forall r.
--     (Member (Error TypeError) r, Member (State InferState) r, Member UniqueGen r) =>
--     Shunted.Expr ->
--     Sem r (Expr PartialType)
-- inferExpression (Shunted.Expr le) = Expr <$> fmap unwrap (traverseOf unlocated inferExpression' le)
--   where
--     inferExpression' :: Shunted.Expr' -> Sem r (Expr' PartialType, PartialType)
--     inferExpression' = undefined

-- -- inferExpression' (Shunted.Int _) = pure (UserDefinedType (Located (generatedSourceRegion Nothing) (Qualified (TypeName "Int") (ModuleName ("Prelude" :| [])))))
-- -- inferExpression' (Shunted.Float _) = pure (UserDefinedType (Located (generatedSourceRegion Nothing) (Qualified (TypeName "Float") (ModuleName ("Prelude" :| [])))))
-- -- inferExpression' (Shunted.String _) = pure (UserDefinedType (Located (generatedSourceRegion Nothing) (Qualified (TypeName "String") (ModuleName ("Prelude" :| [])))))
-- -- inferExpression' (Char _) = pure (UserDefinedType (Located (generatedSourceRegion Nothing) (Qualified (TypeName "Char") (ModuleName ("Prelude" :| [])))))
-- -- inferExpression' (Var v) = liftA2 (,) (lookupTypeEnv v) (lookupTypeEnv v)
