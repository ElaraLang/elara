module Elara.AST.Select where

import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Frontend.Unlocated qualified as Frontend.Unlocated
import Elara.AST.Name (MaybeQualified)

data Frontend

data UnlocatedFrontend

type family ASTExpr ast where
    ASTExpr Frontend = Frontend.Expr
    ASTExpr UnlocatedFrontend = Frontend.Unlocated.Expr

type family ASTPattern ast where
    ASTPattern Frontend = Frontend.Pattern
    ASTPattern UnlocatedFrontend = Frontend.Pattern

type family ASTAnnotation ast where
    ASTAnnotation Frontend = Maybe Frontend.TypeAnnotation
    ASTAnnotation UnlocatedFrontend = Maybe Frontend.TypeAnnotation
type family ASTQual ast where
    ASTQual Frontend = MaybeQualified
    ASTQual UnlocatedFrontend = MaybeQualified
