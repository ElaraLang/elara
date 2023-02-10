module Elara.AST.Select where

import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Frontend.Unlocated qualified as Frontend.Unlocated
import Elara.AST.Name (MaybeQualified, Qualified)

data Frontend

data UnlocatedFrontend

data Annotated

type family ASTExpr ast where
    ASTExpr Frontend = Frontend.Expr
    ASTExpr UnlocatedFrontend = Frontend.Unlocated.Expr
    ASTExpr Annotated = Annotated.Expr

type family ASTPattern ast where
    ASTPattern Frontend = Frontend.Pattern
    ASTPattern UnlocatedFrontend = Frontend.Pattern
    ASTPattern Annotated = Annotated.Pattern

type family ASTAnnotation ast where
    ASTAnnotation Frontend = Maybe Frontend.TypeAnnotation
    ASTAnnotation UnlocatedFrontend = Maybe Frontend.TypeAnnotation
    ASTAnnotation Annotated = Maybe Annotated.TypeAnnotation

type family ASTQual ast where
    ASTQual Frontend = MaybeQualified
    ASTQual UnlocatedFrontend = MaybeQualified
    ASTQual Annotated = Qualified

